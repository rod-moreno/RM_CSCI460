# ==============================================================================
# 1. SETUP & INSTANTIATION
# ==============================================================================
library(shiny)
library(bslib) 
library(dplyr)
library(tidyr)
library(workflows)
library(xgboost)
library(shinyjs)
library(DT)

# Load your smooth model and aggregated reference statistics
final_smooth_model <- readRDS("final_smooth_model.rds")
champion_profiles  <- readRDS("champion_profiles.rds")

champion_choices <- as.character(sort(unique(champion_profiles$championName)))

# Derive a role-agnostic fallback by averaging each champion's per-role stats
build_champion_profiles_fallback_from_profiles <- function(champion_profiles) {
  champion_profiles %>%
    group_by(championName) %>%
    summarise(
      base_gold_pm    = weighted.mean(base_gold_pm, w = games_played, na.rm = TRUE),
      base_efficiency = weighted.mean(base_efficiency, w = games_played, na.rm = TRUE),
      base_stomp      = weighted.mean(base_stomp, w = games_played, na.rm = TRUE),
      base_proximity  = weighted.mean(base_proximity, w = games_played, na.rm = TRUE),
      base_roaming    = weighted.mean(base_roaming, w = games_played, na.rm = TRUE),
      base_crab_count = 0,
      games_played_fb = sum(games_played, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(championName = as.character(championName))
}

champion_profiles_fallback <- build_champion_profiles_fallback_from_profiles(champion_profiles)

# Compute global position-specific baseline averages to serve as fallback values
global_position_defaults <- champion_profiles %>%
  group_by(teamPosition) %>%
  summarise(
    across(c(base_gold_pm, base_efficiency, base_stomp, base_proximity, base_roaming, base_crab_count), 
           ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# ==============================================================================
# 2. CORE DRAFT SIMULATION ALGO (SYNTAX FIXED)
# ==============================================================================
simulate_final_draft <- function(blue_draft, red_draft,
                                 champion_profiles,
                                 champion_profiles_fallback) {
  
  champion_profiles <- champion_profiles %>%
    mutate(
      championName = as.character(championName),
      teamPosition = as.character(teamPosition)
    )
  
  champion_profiles_fallback <- champion_profiles_fallback %>%
    mutate(championName = as.character(championName))
  
  stat_cols <- c("base_gold_pm", "base_efficiency", "base_stomp",
                 "base_proximity", "base_roaming", "base_crab_count")
  
  get_position_default <- function(pos, metric) {
    global_position_defaults[[metric]][global_position_defaults$teamPosition == pos]
  }
  
  assemble_side <- function(draft, side_label) {
    champs <- c(as.character(draft$top), as.character(draft$jng),
                as.character(draft$mid), as.character(draft$adc),
                as.character(draft$sup))
    roles  <- c("top", "jng", "mid", "adc", "sup")
    
    base <- data.frame(position = roles, championName = champs,
                       stringsAsFactors = FALSE) %>%
      mutate(teamPosition = case_when(
        position == "top" ~ "TOP",
        position == "jng" ~ "JUNGLE",
        position == "mid" ~ "MIDDLE",
        position == "adc" ~ "BOTTOM",
        position == "sup" ~ "UTILITY"
      ))
    
    role_specific <- base %>%
      left_join(champion_profiles, by = c("championName", "teamPosition"))
    
    with_fallback <- role_specific %>%
      left_join(
        champion_profiles_fallback %>%
          rename_with(~ paste0(.x, "_fb"), all_of(c(stat_cols, "games_played_fb"))),
        by = "championName"
      )
    
    for (col in stat_cols) {
      fb_col <- paste0(col, "_fb")
      with_fallback[[col]] <- dplyr::coalesce(with_fallback[[col]], with_fallback[[fb_col]])
    }
    
    for (i in 1:nrow(with_fallback)) {
      if (with_fallback$championName[i] == "" || is.na(with_fallback$base_gold_pm[i])) {
        pos <- with_fallback$teamPosition[i]
        for (col in stat_cols) {
          with_fallback[[col]][i] <- get_position_default(pos, col) # FIXED: Changed 'guide=' to '<-'
        }
      }
    }
    
    stats <- with_fallback %>%
      select(position, championName, teamPosition, all_of(stat_cols)) %>%
      mutate(id = 1)
    
    list(stats = stats)
  }
  
  blue_stats <- assemble_side(blue_draft, "blue")$stats
  red_stats  <- assemble_side(red_draft,  "red")$stats
  
  blue_wide <- blue_stats %>%
    pivot_wider(
      id_cols = id, names_from = position,
      values_from = all_of(stat_cols),
      names_glue = "{.value}_{position}_blue"
    )
  
  red_wide <- red_stats %>%
    pivot_wider(
      id_cols = id, names_from = position,
      values_from = all_of(stat_cols),
      names_glue = "{.value}_{position}_red"
    )
  
  # VARIANCE INFLATION FACTOR
  variance_inflation <- 6.5
  
  simulated_match <- blue_wide %>%
    inner_join(red_wide, by = "id") %>%
    mutate(
      matchId        = as.factor("SIM_MATCH_01"),
      global_ult_gap = as.numeric(0),
      initialCrabCount = as.integer(round(base_crab_count_jng_blue + base_crab_count_jng_red)),
      
      top_gold_lead    = (base_gold_pm_top_blue - base_gold_pm_top_red) * variance_inflation,
      jungle_gold_lead = (base_gold_pm_jng_blue - base_gold_pm_jng_red) * variance_inflation,
      mid_gold_lead    = (base_gold_pm_mid_blue - base_gold_pm_mid_red) * variance_inflation,
      adc_gold_lead    = (base_gold_pm_adc_blue - base_gold_pm_adc_red) * variance_inflation,
      supp_gold_lead   = (base_gold_pm_sup_blue - base_gold_pm_sup_red) * variance_inflation,
      
      top_gold_eff_lead    = (base_efficiency_top_blue - base_efficiency_top_red) * variance_inflation,
      jungle_gold_eff_lead = (base_efficiency_jng_blue - base_efficiency_jng_red) * variance_inflation,
      mid_gold_eff_lead    = (base_efficiency_mid_blue - base_efficiency_mid_red) * variance_inflation,
      adc_gold_eff_lead    = (base_efficiency_adc_blue - base_efficiency_adc_red) * variance_inflation,
      supp_gold_eff_lead   = (base_efficiency_sup_blue - base_efficiency_sup_red) * variance_inflation,
      
      top_stomp_gap    = base_stomp_top_blue - base_stomp_top_red,
      jungle_stomp_gap = base_stomp_jng_blue - base_stomp_jng_red,
      mid_stomp_gap    = base_stomp_mid_blue - base_stomp_mid_red,
      adc_stomp_gap    = base_stomp_adc_blue - base_stomp_adc_red,
      supp_stomp_gap   = base_stomp_sup_blue - base_stomp_sup_red,
      
      top_prox_gap  = base_proximity_top_blue - base_proximity_top_red,
      mid_prox_gap  = base_proximity_mid_blue - base_proximity_mid_red,
      adc_prox_gap  = base_proximity_adc_blue - base_proximity_adc_red,
      supp_prox_gap = base_proximity_sup_blue - base_proximity_sup_red,
      
      mid_roam_gap  = base_roaming_mid_blue - base_roaming_mid_red,
      adc_roam_gap  = base_roaming_adc_blue - base_roaming_adc_red,
      supp_roam_gap = base_roaming_sup_blue - base_roaming_sup_red
    )
  
  model_ready_data <- simulated_match %>%
    select(matchId, initialCrabCount, global_ult_gap, ends_with("_lead"), ends_with("_gap"))
  
  probabilities <- predict(final_smooth_model, model_ready_data, type = "prob")
  
  prob_blue <- as.numeric(probabilities[[2]][1])
  
  return(list(dragon_blue = as.numeric(prob_blue * 100)))
}
# ==============================================================================
# 3. USER INTERFACE SPECIFICATION
# ==============================================================================
ui <- page_navbar(
  title = "LoL Draft Simulator (with dragon prediction)",
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  
  # Inject dependencies into the header to prevent bslib collection warnings
  header = tagList(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .progress-bar { transition: width 0.6s ease !important; }
      "))
    )
  ),
  
  nav_panel(
    title = "Draft Simulator",
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Simulation Dashboard",
        width = 300,
        p("Don't worry about the default split being 30%/70%, red side just has that much advantage inherently")
      ),
      
      layout_columns(
        col_widths = c(6, 6, 12),
        
        card(
          card_header(class = "bg-primary text-white", "Blue Side Team Selection"),
          selectInput("blue_top", "Top Lane:", choices = c("Select Champion" = "", champion_choices)),
          selectInput("blue_jng", "Jungle:",   choices = c("Select Champion" = "", champion_choices)),
          selectInput("blue_mid", "Mid Lane:", choices = c("Select Champion" = "", champion_choices)),
          selectInput("blue_adc", "ADC:",      choices = c("Select Champion" = "", champion_choices)),
          selectInput("blue_sup", "Support:",  choices = c("Select Champion" = "", champion_choices))
        ),
        
        card(
          card_header(class = "bg-danger text-white", "Red Side Team Selection"),
          selectInput("red_top", "Top Lane:", choices = c("Select Champion" = "", champion_choices)),
          selectInput("red_jng", "Jungle:",   choices = c("Select Champion" = "", champion_choices)),
          selectInput("red_mid", "Mid Lane:", choices = c("Select Champion" = "", champion_choices)),
          selectInput("red_adc", "ADC:",      choices = c("Select Champion" = "", champion_choices)),
          selectInput("red_sup", "Support:",  choices = c("Select Champion" = "", champion_choices))
        ),
        
        card(
          card_header(class = "bg-dark text-white", "Live Matchup Metrics"),
          p(strong("First Dragon Control Probability")),
          div(class = "progress", style = "height: 10px;",
              div(id = "blue_bar", class = "progress-bar bg-primary", style = "width: 50%;"),
              div(id = "red_bar",  class = "progress-bar bg-danger",  style = "width: 50%;")
          )
        )
      )
    )
  ),
  
  nav_panel(
    title = "Champion Stats",
    
    card(
      card_header(class = "bg-dark text-white", "Filter by Role"),
      selectInput("stats_role", "Role:",
                  choices = c(
                    "Top"     = "TOP",
                    "Jungle"  = "JUNGLE",
                    "Mid"     = "MIDDLE",
                    "ADC"     = "BOTTOM",
                    "Support" = "UTILITY"
                  ),
                  selected = "TOP")
    ),
    
    card(
      card_header(class = "bg-dark text-white", "Champion Baseline Stats"),
      DT::dataTableOutput("champion_stats_table")
    )
  )
)

# ==============================================================================
# 4. SERVER ENGINE
# ==============================================================================
server <- function(input, output, session) {
  
  live_metrics <- reactive({
    blue_team <- list(top = input$blue_top, jng = input$blue_jng, mid = input$blue_mid, adc = input$blue_adc, sup = input$blue_sup)
    red_team  <- list(top = input$red_top, jng = input$red_jng, mid = input$red_mid, adc = input$red_adc, sup = input$red_sup)
    
    tryCatch({
      simulate_final_draft(
        blue_draft = blue_team,
        red_draft  = red_team,
        champion_profiles          = champion_profiles,
        champion_profiles_fallback = champion_profiles_fallback
      )
    }, error = function(e) {
      message("simulate_final_draft() failed: ", conditionMessage(e))
      list(dragon_blue = 50)
    })
  })
  
  observe({
    
    metrics <- live_metrics()
    
    b_drag <- max(min(round(metrics$dragon_blue), 100), 0)
    r_drag <- 100 - b_drag
    
    runjs(sprintf("
      var blueBar = document.getElementById('blue_bar');
      var redBar  = document.getElementById('red_bar');
      blueBar.style.width = '%d%%';
      redBar.style.width  = '%d%%';
    ", b_drag, r_drag))
    
  })
  
  output$champion_stats_table <- DT::renderDataTable({
    req(input$stats_role)
    
    if (input$stats_role == "JUNGLE") {
      tbl <- champion_profiles %>%
        filter(teamPosition == input$stats_role) %>%
        arrange(championName) %>%
        select(
          Champion              = championName,
          Position              = teamPosition,
          `Avg. Gold/min`       = base_gold_pm,
          `Efficiency Score`    = base_efficiency,
          `Lane Stomp%`         = base_stomp,
          `Jungle Proximity%`   = base_proximity,
          `Roaming%`            = base_roaming,
          `Avg. Early Crabs`    = base_crab_count,
          `Win%`                = win_rate,
          `Kill Participation%` = kill_participation,
          `Games Played`        = games_played
        )
    } else {
      tbl <- champion_profiles %>%
        filter(teamPosition == input$stats_role) %>%
        arrange(championName) %>%
        select(
          Champion              = championName,
          Position              = teamPosition,
          `Avg. Gold/min`       = base_gold_pm,
          `Efficiency Score`    = base_efficiency,
          `Lane Stomp%`         = base_stomp,
          `Jungle Proximity%`   = base_proximity,
          `Roaming%`            = base_roaming,
          `Win%`                = win_rate,
          `Kill Participation%` = kill_participation,
          `Games Played`        = games_played
        )
    }
    
    pct_cols <- c("Lane Stomp%", "Jungle Proximity%", "Roaming%", "Win%", "Kill Participation%")
    
    dt <- DT::datatable(
      tbl,
      rownames = FALSE,
      options  = list(
        pageLength = 25,
        order      = list(list(0, "asc")),
        scrollX    = TRUE
      )
    ) %>%
      DT::formatPercentage(pct_cols, digits = 1) %>%
      DT::formatRound(c("Avg. Gold/min", "Efficiency Score"), digits = 1)
    
    if (input$stats_role == "JUNGLE") {
      dt <- dt %>% DT::formatRound("Avg. Early Crabs", digits = 1)
    }
    
    dt
  })
}

# Launch Application Instance
shinyApp(ui = ui, server = server)