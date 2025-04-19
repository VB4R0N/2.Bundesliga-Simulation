rm(list=ls()) # neue Environment
#### 0. Pakete laden, bei Bedarf mit install.packages() installieren ----
library(plotly)
library(rvest)
library(shiny)
library(shinydashboard)
library(httr)
library(MASS)
library(copula)
library(forcats)
library(scales)
library(magrittr)
library(tidyverse)

#### 1. Datenimport & -cleaning ----
##### 1.1 Tabelle -----
tabelle <- read_html("https://www.transfermarkt.de/2-bundesliga/tabelle/wettbewerb/L2/saison_id/2024") %>%
  html_element(css = ".items") %>%
  html_table()

names(tabelle)[names(tabelle) == ""] <- "Spiele"
tabelle <- tabelle %>%
  dplyr::select(1,3:ncol(.)) %>% 
  rename(Platz=`#`)

##### 1.2 Spielplan (bisher + zukünftig) -----
spielplan_all<- read_html("https://www.transfermarkt.de/2-bundesliga/gesamtspielplan/wettbewerb/L2/saison_id/2024") %>% 
  html_elements(css = "table:nth-child(2)") %>% 
  html_table() %>%
  bind_rows()


spielplan <- spielplan_all[379:nrow(spielplan_all),] %>%
  dplyr::select(Heimmannschaft...3, Ergebnis) %>% 
  filter(!grepl("^\\s*(Fr\\.|Sa\\.|So\\.|20:30)", Heimmannschaft...3) == TRUE) %>% 
  rename(heim = Heimmannschaft...3, gast = Ergebnis) %>% 
  mutate(
    heim = str_remove(heim, "^\\(\\d+\\.\\)\\s+"),
    gast = str_remove(gast, "\\s+\\(\\d+\\.\\)$")
  )

spielplan_bisher <- spielplan_all[1:378,] %>%
  bind_rows() %>% 
  dplyr::select(3,5,7) %>% 
  filter(!grepl("^\\s*(Fr\\.|Sa\\.|So\\.|20:30)", Heimmannschaft...3) == TRUE) %>% 
  rename(heim = Heimmannschaft...3, ergebnis = Heim...5, gast = Ergebnis) %>% 
  mutate(
    heim = str_remove(heim, "^\\(\\d+\\.\\)\\s+"),
    gast = str_remove(gast, "\\s+\\(\\d+\\.\\)$"),
    home_goals= as.numeric(sub(":.*", "", ergebnis)),
    away_goals= as.numeric(sub(".*:", "", ergebnis))
  )

##### 1.3 Kicker: Spielstatistiken -----
url <- "https://www.kicker.de/karlsruhe-gegen-elversberg-2024-bundesliga-4937102/spieldaten"
page <- read_html(url)

stat_bars <- page %>%
  html_elements(".kick__data-grid--max-width .kick__stats-bar")

clean_stats <- map_dfr(stat_bars, function(node) {
  team1 <- node %>% html_element(".kick__stats-bar__value--opponent1") %>% html_text2()
  stat  <- node %>% html_element(".kick__stats-bar__title") %>% html_text2()
  team2 <- node %>% html_element(".kick__stats-bar__value--opponent2") %>% html_text2()
  
  tibble(Stat = stat, Team1 = team1, Team2 = team2)
})


clean_stats <- clean_stats %>%
  mutate(across(c(Team1, Team2), ~ .x %>%
                  str_replace_all(",", ".") %>%
                  str_replace_all("%", "") %>%
                  str_replace_all(" km", "") %>%
                  as.numeric()))


get_match_links <- function(matchday_url) {
  page <- read_html(matchday_url)
  
  links <- page %>%
    html_elements("a") %>%
    html_attr("href") %>%
    str_subset("/analyse$") %>%                 
    str_replace("/analyse$", "/spieldaten") %>% 
    paste0("https://www.kicker.de", .)          
  
  return(unique(links))
}

scrape_match_stats <- function(match_url) {
  page <- read_html(match_url)
  
  stat_bars <- page %>%
    html_elements(".kick__data-grid--max-width .kick__stats-bar")
  
  if (length(stat_bars) == 0) return(NULL)
  
  df <- map_dfr(stat_bars, function(node) {
    team1 <- node %>% html_element(".kick__stats-bar__value--opponent1") %>% html_text2()
    stat  <- node %>% html_element(".kick__stats-bar__title") %>% html_text2()
    team2 <- node %>% html_element(".kick__stats-bar__value--opponent2") %>% html_text2()
    
    tibble(Stat = stat, Team1 = team1, Team2 = team2)
  }) %>%
    mutate(match_url = match_url)
  
  return(df)
}


scrape_matchday_stats <- function(matchday = 1) {
  base_url <- paste0("https://www.kicker.de/2-bundesliga/spieltag/2024-25/", matchday)
  match_links <- get_match_links(base_url)
  
  message("Matchday ", matchday, ": Found ", length(match_links), " matches.")
  
  map_dfr(match_links, possibly(scrape_match_stats, NULL))
}


all_data <- map_dfr(1:29, scrape_matchday_stats)

# Post-processing der Kickerdaten

all_data_named <- all_data %>%
  mutate(
    teams_raw = str_extract(match_url, "(?<=kicker\\.de/).*(?=-2024|2025)"),
    home_team = str_extract(teams_raw, "^[^-]+"),
    away_team = str_extract(teams_raw, "(?<=-gegen-).*")
  )

stats_wide <- all_data_named %>%
  pivot_wider(
    names_from = Stat,
    values_from = c(Team1, Team2),
    names_glue = "{.value}_{Stat}"
  )

colnames(stats_wide) <- colnames(stats_wide) %>%
  str_replace_all("Team1_", "") %>%
  str_replace_all("Team2_", "") %>%
  str_replace_all(" ", "_")

stat_cols <- names(stats_wide)[!(names(stats_wide) %in% c("match_url", "teams_raw", "home_team", "away_team"))]
n_stats <- length(stat_cols) / 2

home_cols <- stat_cols[1:n_stats]
away_cols <- stat_cols[(n_stats + 1):(2 * n_stats)]

names(stats_wide)[match(home_cols, names(stats_wide))] <- paste0(home_cols, "_home")
names(stats_wide)[match(away_cols, names(stats_wide))] <- paste0(away_cols, "_away")

stats_wide <- stats_wide %>%
  dplyr::select(-match_url, -teams_raw)

clean_numeric <- function(x) {
  x <- str_replace_all(x, ",", ".")
  x <- str_remove_all(x, "%| km")
  as.numeric(x)
}

stats_wide <- stats_wide %>%
  mutate(across(ends_with("_home") | ends_with("_away"), clean_numeric),
         away_team = if_else(row_number() >= 154, str_remove(away_team, "-"), away_team))

team_name_map <- c(
  "koeln" = "1.FC Köln", "karlsruhe" = "Karlsruher SC", "hannover" = "Hannover 96", 
  "hertha" = "Hertha BSC", "magdeburg" = "1.FC Magdeburg", "schalke" = "FC Schalke 04", 
  "darmstadt" = "Darmstadt 98", "fuerth" = "Greuther Fürth", "ulm" = "SSV Ulm 1846", 
  "klautern" = "1.FC K'lautern", "regensburg" = "J. Regensburg", "duesseldorf" = "F. Düsseldorf", 
  "elversberg" = "SV Elversberg", "nuernberg" = "1.FC Nürnberg", "hsv" = "Hamburger SV", 
  "paderborn" = "SC Paderborn", "braunschweig" = "E. Braunschweig", "muenster" = "Pr. Münster"
)

stats_wide <- stats_wide %>%
  mutate(
    home_team = recode(home_team, !!!team_name_map),
    away_team = recode(away_team, !!!team_name_map)
  )

stats_wide <- stats_wide %>%
  left_join(
    spielplan_bisher %>% dplyr::select(heim, gast, home_goals, away_goals),
    by = c("home_team" = "heim", "away_team" = "gast")
  ) %>%
  relocate(home_goals, away_goals, .after = away_team)


# Heim & Gastdaten Split
home_data <- stats_wide %>%
  dplyr::select(home_team, away_team, home_goals, away_goals, ends_with("_home"))

away_data <- stats_wide %>%
  dplyr::select(home_team, away_team, home_goals, away_goals, ends_with("_away"))


#### 2. Modellierung ----
##### 2.1 Dummies fürs Gegnerteam -----
team_tiers <- tabelle[,1:2] %>%
  mutate(
    opponent_tier = case_when(
      Platz %in% 18:15 ~ "Abstiegskandidaten",
      Platz %in% 14:10 ~ "Niemandsländer",
      Platz %in% 9:1 ~ "Aufstiegskandidaten"
    ),
    opponent_tier = factor(opponent_tier, 
                           levels = c("Abstiegskandidaten", "Niemandsländer", "Aufstiegskandidaten"))
  ) %>%
  dplyr::select(Verein, opponent_tier)

home_data <- home_data %>%
  left_join(team_tiers, by = c("away_team" = "Verein")) %>%
  rename(opponent_tier_home = opponent_tier)

away_data <- away_data %>%
  left_join(team_tiers, by = c("home_team" = "Verein")) %>%
  rename(opponent_tier_away = opponent_tier)

##### 2.2 Heimtore: Poisson regression -----
poisson_home<-glm(
  formula =  home_goals ~ opponent_tier_home+
    xGoals_home + xAssists_home + Torschüsse_home + Laufleistung_home +
    gespielte_Pässe_home + angekommene_Pässe_home + Flanken_home +
    angekommene_Flanken_home + Dribblings_home + erfolgreiche_Dribblings_home +
    Ballbesitz_home + Zweikämpfe_home + gewonnene_Zweikämpfe_home +
    Luftzweikämpfe_home + gewonnene_Luftzweikämpfe_home +
    `Foul/Hand_gespielt_home` + Gefoult_worden_home + Abseits_home + Ecken_home,
  data = home_data,
  family = poisson
)
poisson_home %>% summary
# Modell mit geringstem AIC 
poisson_model_selected_home <- stepAIC(poisson_home, direction = "both", trace = FALSE)
poisson_model_selected_home %>%  summary()


##### 2.3 Gasttore: Poisson regression -----
poisson_away<-glm(
  formula =  away_goals ~ opponent_tier_away +
    xGoals_away + xAssists_away + Torschüsse_away + Laufleistung_away +
    gespielte_Pässe_away + angekommene_Pässe_away + Flanken_away +
    angekommene_Flanken_away + Dribblings_away + erfolgreiche_Dribblings_away +
    Ballbesitz_away + Zweikämpfe_away + gewonnene_Zweikämpfe_away +
    Luftzweikämpfe_away + gewonnene_Luftzweikämpfe_away +
    `Foul/Hand_gespielt_away` + Gefoult_worden_away + Abseits_away + Ecken_away,
  data = away_data,
  family = poisson
)
poisson_away %>% summary
# AIC (wie davor)
poisson_model_selected <- stepAIC(poisson_away, direction = "both", trace = FALSE)
poisson_model_selected_home %>%  summary()

##### 2.4 Diagnostik Checks -----
performance::check_overdispersion(poisson_model_selected_home)
performance::check_overdispersion(poisson_model_selected)
# keine Overdispersion -> Ich bleibe bei Poisson

car::vif(poisson_model_selected_home)
car::vif(poisson_model_selected)
# passt

#### 3. Simulation ----
##### 3.1 Interdependencies der Variablen modellieren -----
home_team_full_stats <- home_data %>%
  select(c(1, 2, names(poisson_model_selected_home$model))) %>%
  group_by(home_team) %>%
  summarise(
    across(
      c(xGoals_home:Gefoult_worden_home),
      list(mean = mean, sd = sd),
      na.rm = TRUE
    ),
    cov_matrix = list(cov(across(c(xGoals_home:Gefoult_worden_home)), use = "complete.obs"))
  ) %>%
  rename(team = home_team)

away_team_full_stats <- away_data %>%
  select(c(1, 2, names(poisson_model_selected$model))) %>%
  group_by(away_team) %>%
  summarise(
    across(
      c(xGoals_away:Gefoult_worden_away),
      list(mean = mean, sd = sd),
      na.rm = TRUE
    ),
    cov_matrix = list(cov(across(c(xGoals_away:Gefoult_worden_away)), use = "complete.obs"))
  ) %>%
  rename(team = away_team)

home_model <- glm(home_goals ~ ., data = poisson_model_selected_home$model, family = poisson)
away_model <- glm(away_goals ~ ., data = poisson_model_selected$model, family = poisson)
home_vars <- all.vars(formula(home_model))[-1]  # Exclude response variable
away_vars <- all.vars(formula(away_model))[-1]

team_tiers <- away_data %>%
  distinct(team = away_team, opponent_tier = opponent_tier_away)

simulate_match_stats <- function(home_team, away_team, n_sim = 10000) {
  home_stats <- filter(home_team_full_stats, team == home_team)
  away_stats <- filter(away_team_full_stats, team == away_team)
  
  opponent_tier <- team_tiers %>%
    filter(team == away_team) %>%
    pull(opponent_tier) %>%
    first()
  
  home_sim <- home_stats %>%
    select(ends_with("_mean")) %>%
    select(-any_of("opponent_tier_home_mean")) %>%
    {
      as_tibble(mvrnorm(n_sim, 
                        mu = unlist(.), 
                        Sigma = home_stats$cov_matrix[[1]][
                          rownames(home_stats$cov_matrix[[1]]) != "opponent_tier_home",
                          colnames(home_stats$cov_matrix[[1]]) != "opponent_tier_home"
                        ]))
    } %>%
    set_names(str_remove(names(.), "_mean")) %>%
    mutate(across(any_of(c("Flanken_home", "Gefoult_worden_home")), ~round(pmax(., 0)))) %>%
    mutate(opponent_tier_home = factor(
      opponent_tier,
      levels = c("Abstiegskandidaten", "Niemandsländer", "Aufstiegskandidaten")
    ))
  
  away_sim <- away_stats %>%
    select(ends_with("_mean")) %>%
    {
      as_tibble(mvrnorm(n_sim, 
                        mu = unlist(.), 
                        Sigma = away_stats$cov_matrix[[1]]))
    } %>%
    set_names(str_remove(names(.), "_mean")) %>%
    mutate(across(any_of(c("Flanken_away", "Gefoult_worden_away")), ~round(pmax(., 0))))
  
  list(home_sim = home_sim, away_sim = away_sim)
}

results <- spielplan %>%
  mutate(
    sim_inputs = map2(heim, gast, simulate_match_stats),
    predictions = map(sim_inputs, ~{
      tibble(
        home_goals = rpois(nrow(.x$home_sim), predict(home_model, newdata = .x$home_sim, type = "response")),
        away_goals = rpois(nrow(.x$away_sim), predict(away_model, newdata = .x$away_sim, type = "response"))
      ) %>%
        summarise(
          avg_home = mean(home_goals),
          avg_away = mean(away_goals),
          prob_home_win = mean(home_goals > away_goals),
          prob_draw = mean(home_goals == away_goals),
          prob_away_win = mean(home_goals < away_goals)
        )
    })
  ) %>%
  unnest_wider(predictions) %>%
  select(-sim_inputs)

##### 3.2 Monte-Carlo Simulation ----
n_sim <- 10000
all_simulations <- vector("list", n_sim)


tabelle <- tabelle %>%
  rename(club = Verein) %>%
  mutate(
    F = as.numeric(str_extract(Tore, "^[0-9]+")),
    A = as.numeric(str_extract(Tore, "(?<=:)\\d+"))
  )


for (sim in 8684:n_sim) {
  table_sim <- tabelle %>%
    select(club, Spiele, G, U, V, F, A, Pkt.) %>%
    mutate(
      Spiele = as.integer(Spiele),
      G = as.integer(G),
      U = as.integer(U),
      V = as.integer(V),
      F = as.integer(F),
      A = as.integer(A),
      Pkt. = as.integer(Pkt.)
    )
  
  for (i in seq_len(nrow(results))) {
    home <- results$heim[i]
    away <- results$gast[i]
    

    g_home <- rpois(1, lambda = results$avg_home[i])
    g_away <- rpois(1, lambda = results$avg_away[i])
    
    pts_home <- ifelse(g_home > g_away, 3, ifelse(g_home == g_away, 1, 0))
    pts_away <- ifelse(g_home < g_away, 3, ifelse(g_home == g_away, 1, 0))
    
    table_sim <- table_sim %>%
      mutate(
        Spiele = case_when(club == home ~ Spiele + 1,
                           club == away ~ Spiele + 1,
                           TRUE ~ Spiele),
        G = case_when(club == home & g_home > g_away ~ G + 1,
                      club == away & g_away > g_home ~ G + 1,
                      TRUE ~ G),
        U = case_when(club %in% c(home, away) & g_home == g_away ~ U + 1,
                      TRUE ~ U),
        V = case_when(club == home & g_home < g_away ~ V + 1,
                      club == away & g_away < g_home ~ V + 1,
                      TRUE ~ V),
        F = case_when(club == home ~ F + g_home,
                      club == away ~ F + g_away,
                      TRUE ~ F),
        A = case_when(club == home ~ A + g_away,
                      club == away ~ A + g_home,
                      TRUE ~ A),
        Pkt. = case_when(club == home ~ Pkt. + pts_home,
                         club == away ~ Pkt. + pts_away,
                         TRUE ~ Pkt.)
      )
  }
  
  table_sim <- table_sim %>%
    mutate(diff = F - A) %>%
    arrange(desc(Pkt.), desc(diff), desc(F)) %>%
    mutate(pos = row_number(), simulation = sim)
  
  all_simulations[[sim]] <- table_sim
}

final_table_sim <- bind_rows(all_simulations)

#### 4.  Visualization ----
##### 4.1  Interaktiver Plot für Verteilung der Endplatzierung -----
p <- final_table_sim %>%
  count(club, pos) %>%
  group_by(club) %>%
  mutate(prob = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(club, -pos), y = prob, fill = factor(pos),
             text = paste0("Team: ", club, "<br>Position: ", pos, "<br>Prob: ", round(prob * 100, 1), "%"))) +
  geom_col(position = "stack", width = 0.8) +
  coord_flip() +
  scale_fill_viridis_d(option = "C", name = "Final Position") +
  labs(
    title = "Simulated Final League Position Distribution",
    subtitle = paste0("Based on ", length(unique(final_table_sim$simulation)), " Monte Carlo Simulations"),
    x = "Team",
    y = "Probability"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 12),
    legend.position = "right"
  )

ggplotly(p, tooltip = "text")

##### 4.2 Kompakte Darstellung: Direkter Auf-/ Abstieg, Klassenerhalt/verbleib, Auf-/ Abstiegsrelegation

prob_table<- final_table_sim %>%
  count(club, pos) %>%
  group_by(club) %>%
  mutate(prob = n / sum(n))

summary_table <- prob_table %>%
  group_by(club) %>%
  summarize(
    `Direct Promotion` = sum(prob[pos %in% 1:2]),
    `Promotion Play-off` = sum(prob[pos == 3]),
    `Direkter Klassenerhalt` = sum(prob[pos %in% 4:15]),
    `Relegation Play-off` = sum(prob[pos == 16]),
    `Direct Relegation` = sum(prob[pos %in% 17:18])
  ) %>%
  ungroup() %>%
  mutate(`Total Score` = `Direct Promotion` * 4 + `Promotion Play-off` * 3 - `Relegation Play-off` * 3 - `Direct Relegation` * 4) %>%
  arrange(desc(`Total Score`)) %>%
  select(-`Total Score`) %>%
  pivot_longer(-club, names_to = "Outcome", values_to = "Probability")

summary_table$club <- factor(summary_table$club, levels = unique(summary_table$club))
summary_table$Outcome <- factor(
  summary_table$Outcome,
  levels = c("Direct Promotion", "Promotion Play-off", "Direkter Klassenerhalt","Relegation Play-off", "Direct Relegation")
)
level_translation <- c(
  "Direct Promotion" = "Direkter Aufstieg",
  "Promotion Play-off" = "Aufstiegsrelegation",
  "Direkter Klassenerhalt" = "Klassenerhalt",
  "Relegation Play-off" = "Abstiegsrelegation",
  "Direct Relegation" = "Direkter Abstieg"
)

ggplot(summary_table, aes(x = Outcome, y = fct_rev(club), fill = Probability)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::percent(Probability, accuracy = 0.05)), size = 4.2) +
  scale_fill_gradient(low = "white", high = "deepskyblue", name = "Wahrscheinlichkeit") +
  scale_x_discrete(labels = level_translation) +  
  labs(
    title = "Auf- & Abstiegswahrscheinlichkeiten",
    subtitle = "auf Basis der simulierten Endplatzierungen",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 11),
    panel.grid = element_blank(),
    legend.position = "right"
  )

#### 5. Shiny Dashboard: Bedingte Wahrscheinlichkeiten ----

tabelle <- data.frame(
  club = c("Hamburger SV", "1.FC Köln", "SV Elversberg", "F. Düsseldorf", 
           "1.FC Magdeburg", "1.FC K'lautern", "SC Paderborn", "1.FC Nürnberg", 
           "Hannover 96", "Karlsruher SC", "FC Schalke 04", "Hertha BSC", 
           "Darmstadt 98", "Greuther Fürth", "E. Braunschweig", "Pr. Münster", 
           "SSV Ulm 1846", "J. Regensburg"),
  Pkt. = c(52, 51, 47, 47, 46, 46, 45, 44, 43, 41, 37, 36, 35, 35, 30, 28, 26, 22)
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .title-panel { font-size: 20px; }
      .select-input, .slider-input { font-size: 18px; }
      .plot-text { font-size: 16px; }
      .table { font-size: 16px; }
      .points-table { font-size: 14px; } /* Smaller font for points tables */
      .table-container {
        display: flex;
        flex-wrap: wrap;
        justify-content: space-between;
      }
      .table-half {
        width: 48%;
      }
    "))
  ),
  titlePanel(div("Team Position Distribution", class = "title-panel")),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Wähle ein Team:", 
                  choices = tabelle$club,
                  width = "100%"),
      uiOutput("points_slider")
    ),
    mainPanel(
      plotOutput("distPlot", height = "500px"),
      tableOutput("statsTable"),
      h4("Verteilung der möglichen Endpunkte"),
      div(class = "table-container",
          div(class = "table-half", tableOutput("pointsTableLeft")),
          div(class = "table-half", tableOutput("pointsTableRight"))
      )
    )
  )
)

server <- function(input, output, session) {
  team_base_points <- reactive({
    tabelle %>% 
      filter(club == input$team) %>% 
      pull(Pkt.)
  })
  
  output$points_slider <- renderUI({
    base_points <- team_base_points()
    
    possible_points <- final_table_sim %>%
      filter(club == input$team) %>%
      pull(Pkt.) %>%
      unique() %>%
      sort()
    
    if(length(possible_points) == 0) {
      min_val <- base_points
      max_val <- base_points + 15
    } else {
      min_val <- min(possible_points)
      max_val <- max(possible_points)
    }
    
    sliderInput("final_points", "Mögliche Endpunkte:", 
                min = min_val, max = max_val, value = base_points,
                step = 1,
                width = "100%",
                animate = animationOptions(interval = 3500, loop = TRUE))
  })
  
  filtered_data <- reactive({
    req(input$final_points)
    
    final_table_sim %>%
      group_by(simulation) %>%
      filter(any(club == input$team & Pkt. == input$final_points)) %>%
      arrange(simulation, desc(Pkt.)) %>%
      mutate(pos = rank(-Pkt., ties.method = "first")) %>%
      ungroup() %>%
      filter(club == input$team)
  })
  
  all_points_data <- reactive({
    all_team_sims <- final_table_sim %>%
      filter(club == input$team)
    
    total_simulations <- length(unique(all_team_sims$simulation))
    
    all_team_sims %>%
      count(Pkt.) %>%
      mutate(
        Punktzahl = Pkt.,
        Anzahl = n,
        Wahrscheinlichkeit = sprintf("%.1f%%", (n / total_simulations) * 100)
      ) %>%
      select(Punktzahl, Anzahl, Wahrscheinlichkeit) %>%
      arrange(desc(Punktzahl))
  })
  
  output$distPlot <- renderPlot({
    req(filtered_data())
    if(nrow(filtered_data()) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Keine Simulationen mit dieser Punktzahl verfügbar") +
               theme_void())
    }
    
    total_filtered_simulations <- length(unique(filtered_data()$simulation))
    
    plot_data <- filtered_data() %>%
      count(pos) %>%
      mutate(Probability = n / total_filtered_simulations * 100)
    
    ggplot(plot_data, aes(x = factor(pos), y = Probability)) +
      geom_col(fill = "#4682B4", width = 0.8) +
      geom_text(aes(label = sprintf("%.1f%%", Probability)), 
                vjust = -0.5, size = 6) +
      labs(title = paste("Endplatzierung von", input$team, "mit", input$final_points, "Punkten"),
           x = "Platzierung", 
           y = "Wahrscheinlichkeit (%)") +
      theme_minimal(base_size = 20) +
      theme(
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 22, face = "bold")
      ) +
      scale_y_continuous(limits = c(0, max(plot_data$Probability) * 1.1))
  })
  
  output$statsTable <- renderTable({
    req(filtered_data())
    if(nrow(filtered_data()) == 0) {
      return(data.frame(Hinweis = "Keine Simulationen mit dieser Punktzahl verfügbar"))
    }
    
    total_filtered_simulations <- length(unique(filtered_data()$simulation))
    
    filtered_data() %>%
      count(pos) %>%
      mutate(
        Anzahl = n,
        Wahrscheinlichkeit = sprintf("%.1f%%", (n / total_filtered_simulations) * 100)
      ) %>%
      select(Platz = pos, Anzahl, Wahrscheinlichkeit) %>%
      arrange(Platz)
  }, digits = 1, align = 'c')
  
  output$pointsTableLeft <- renderTable({
    points_data <- all_points_data()
    
    rows <- nrow(points_data)
    half_point <- ceiling(rows/2)
    
    points_data[1:half_point,]
  }, digits = 1, align = 'c', class = "points-table")
  
  output$pointsTableRight <- renderTable({
    points_data <- all_points_data()
    
    rows <- nrow(points_data)
    half_point <- ceiling(rows/2)
    
    if(half_point < rows) {
      points_data[(half_point+1):rows,]
    } else {
      data.frame()
    }
  }, digits = 1, align = 'c', class = "points-table")
}

shinyApp(ui, server)
