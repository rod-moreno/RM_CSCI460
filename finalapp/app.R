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

top_choices <- champion_profiles %>% filter(teamPosition == "TOP") %>% pull(championName) %>% unique() %>% sort() %>% as.character()
jng_choices <- champion_profiles %>% filter(teamPosition == "JUNGLE") %>% pull(championName) %>% unique() %>% sort() %>% as.character()
mid_choices <- champion_profiles %>% filter(teamPosition == "MIDDLE") %>% pull(championName) %>% unique() %>% sort() %>% as.character()
adc_choices <- champion_profiles %>% filter(teamPosition == "BOTTOM") %>% pull(championName) %>% unique() %>% sort() %>% as.character()
sup_choices <- champion_profiles %>% filter(teamPosition == "UTILITY") %>% pull(championName) %>% unique() %>% sort() %>% as.character()

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
  
  # Stat columns that exist in champion_profiles
  stat_cols <- c("base_efficiency", "base_stomp", "base_proximity", 
                 "base_roaming", "base_crab_count")
  
  # ── Helper: position-level defaults when a champion is fully unknown ──────
  get_position_default <- function(pos, metric) {
    global_position_defaults[[metric]][global_position_defaults$teamPosition == pos]
  }
  
  # ── Helper: build a tidy 5-row stats frame for one side ──────────────────
  assemble_side <- function(draft) {
    champs <- c(as.character(draft$top), as.character(draft$jng),
                as.character(draft$mid), as.character(draft$adc),
                as.character(draft$sup))
    # Use consistent position keys throughout — "supp" matches model feature names
    roles  <- c("top", "jng", "mid", "adc", "supp")
    
    base <- data.frame(position = roles, championName = champs,
                       stringsAsFactors = FALSE) %>%
      mutate(teamPosition = case_when(
        position == "top"  ~ "TOP",
        position == "jng"  ~ "JUNGLE",
        position == "mid"  ~ "MIDDLE",
        position == "adc"  ~ "BOTTOM",
        position == "supp" ~ "UTILITY"
      ))
    
    # 1. Try role-specific lookup first (best quality)
    role_specific <- base %>%
      left_join(champion_profiles, by = c("championName", "teamPosition"))
    
    # 2. Fall back to cross-role profile for the same champion
    fb_cols <- stat_cols
    with_fallback <- role_specific %>%
      left_join(
        champion_profiles_fallback %>%
          rename_with(~ paste0(.x, "_fb"), all_of(fb_cols)),
        by = "championName"
      )
    
    for (col in stat_cols) {
      fb_col <- paste0(col, "_fb")
      with_fallback[[col]] <- dplyr::coalesce(with_fallback[[col]], with_fallback[[fb_col]])
    }
    
    # 3. Fall back to global position average for fully unknown champions
    for (i in seq_len(nrow(with_fallback))) {
      if (with_fallback$championName[i] == "" || is.na(with_fallback$base_efficiency[i])) {
        pos <- with_fallback$teamPosition[i]
        for (col in stat_cols) {
          with_fallback[[col]][i] <- get_position_default(pos, col)
        }
      }
    }
    
    with_fallback %>%
      select(position, championName, teamPosition, all_of(stat_cols)) %>%
      mutate(id = 1)
  }
  
  blue_stats <- assemble_side(blue_draft)
  red_stats  <- assemble_side(red_draft)
  
  # ── Pivot each side to wide format ───────────────────────────────────────
  blue_wide <- blue_stats %>%
    pivot_wider(
      id_cols     = id,
      names_from  = position,
      values_from = all_of(stat_cols),
      names_glue  = "{.value}_{position}_blue"
    )
  
  red_wide <- red_stats %>%
    pivot_wider(
      id_cols     = id,
      names_from  = position,
      values_from = all_of(stat_cols),
      names_glue  = "{.value}_{position}_red"
    )
  
  # ── Compute gaps (Blue - Red) matching model feature names exactly ────────
  simulated_match <- blue_wide %>%
    inner_join(red_wide, by = "id") %>%
    mutate(
      matchId          = as.factor("SIM_MATCH_01"),
      global_ult_gap   = as.numeric(0),
      initialCrabCount = as.integer(round(base_crab_count_jng_blue + base_crab_count_jng_red)),
      
      # Efficiency gaps: non-CS income (kills, assists, plates)
      top_eff_gap    = base_efficiency_top_blue  - base_efficiency_top_red,
      jng_eff_gap    = base_efficiency_jng_blue  - base_efficiency_jng_red,
      mid_eff_gap    = base_efficiency_mid_blue  - base_efficiency_mid_red,
      adc_eff_gap    = base_efficiency_adc_blue  - base_efficiency_adc_red,
      supp_eff_gap   = base_efficiency_supp_blue - base_efficiency_supp_red,
      
      # Lane stomp gaps (binary difference)
      top_stomp_gap  = base_stomp_top_blue  - base_stomp_top_red,
      jng_stomp_gap  = base_stomp_jng_blue  - base_stomp_jng_red,
      mid_stomp_gap  = base_stomp_mid_blue  - base_stomp_mid_red,
      adc_stomp_gap  = base_stomp_adc_blue  - base_stomp_adc_red,
      supp_stomp_gap = base_stomp_supp_blue - base_stomp_supp_red,
      
      # Jungle proximity gaps
      top_prox_gap   = base_proximity_top_blue  - base_proximity_top_red,
      mid_prox_gap   = base_proximity_mid_blue  - base_proximity_mid_red,
      adc_prox_gap   = base_proximity_adc_blue  - base_proximity_adc_red,
      supp_prox_gap  = base_proximity_supp_blue - base_proximity_supp_red,
      
      # Roaming gaps
      mid_roam_gap   = base_roaming_mid_blue  - base_roaming_mid_red,
      adc_roam_gap   = base_roaming_adc_blue  - base_roaming_adc_red,
      supp_roam_gap  = base_roaming_supp_blue - base_roaming_supp_red
    )
  
  # ── Select exactly the columns the model was trained on ──────────────────
  model_ready_data <- simulated_match %>%
    select(
      matchId,
      initialCrabCount,
      global_ult_gap,
      ends_with("_eff_gap"),
      ends_with("_stomp_gap"),
      ends_with("_prox_gap"),
      ends_with("_roam_gap")
    )
  
  
  # ── Predict ──────────────────────────────────────────────────────────────
  probabilities <- predict(final_smooth_model, model_ready_data, type = "prob")
  
  # Won_Drag is the positive class (blue secured dragon)
  prob_blue <- as.numeric(probabilities$.pred_Won_Drag[1])
  amplified_prob <- 0.5 + ((prob_blue - 0.5) * 2) 
  final_drag_prob <- max(min(amplified_prob * 100, 90), 10) 
  # Calculate Average Team Efficiency for the new UI Bar
  blue_avg_eff <- mean(c(simulated_match$base_efficiency_top_blue, simulated_match$base_efficiency_jng_blue, 
                         simulated_match$base_efficiency_mid_blue, simulated_match$base_efficiency_adc_blue, 
                         simulated_match$base_efficiency_supp_blue), na.rm = TRUE)
  
  red_avg_eff <- mean(c(simulated_match$base_efficiency_top_red, simulated_match$base_efficiency_jng_red, 
                        simulated_match$base_efficiency_mid_red, simulated_match$base_efficiency_adc_red, 
                        simulated_match$base_efficiency_supp_red), na.rm = TRUE)
  
  blue_eff_share <- if ((blue_avg_eff + red_avg_eff) == 0) 0.5 else  (blue_avg_eff / (blue_avg_eff + red_avg_eff))
  amplified_share <- 0.5 + ((blue_eff_share - 0.5) * 15)
  final_share <- max(min(amplified_share * 100, 90), 10) 
  # Calculate a "Disparity Score" by summing the absolute gaps for each role
  # (Handling roles that lack roaming/proximity features gracefully)
  disp_top  <- abs(simulated_match$top_eff_gap) + abs(simulated_match$top_stomp_gap) + abs(simulated_match$top_prox_gap)
  disp_jng  <- abs(simulated_match$jng_eff_gap) + abs(simulated_match$jng_stomp_gap)
  disp_mid  <- abs(simulated_match$mid_eff_gap) + abs(simulated_match$mid_stomp_gap) + abs(simulated_match$mid_prox_gap) + abs(simulated_match$mid_roam_gap)
  disp_adc  <- abs(simulated_match$adc_eff_gap) + abs(simulated_match$adc_stomp_gap) + abs(simulated_match$adc_prox_gap) + abs(simulated_match$adc_roam_gap)
  disp_supp <- abs(simulated_match$supp_eff_gap) + abs(simulated_match$supp_stomp_gap) + abs(simulated_match$supp_prox_gap) + abs(simulated_match$supp_roam_gap)
  
  disparities <- c("Top Lane" = disp_top, "Jungle" = disp_jng, "Mid Lane" = disp_mid, "ADC" = disp_adc, "Support" = disp_supp)
  max_disp_lane <- names(disparities)[which.max(disparities)]
  
  # Handle empty drafts or perfect mirror matches
  if(all(disparities == 0)) max_disp_lane <- "Even Matchup"
  
  return(list(
    dragon_blue = final_drag_prob,
    blue_eff_share = final_share,
    max_disp_lane = max_disp_lane
  ))
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
          selectInput("blue_top", "Top Lane:", choices = c("Select Champion" = "", top_choices)),
          selectInput("blue_jng", "Jungle:",   choices = c("Select Champion" = "", jng_choices)),
          selectInput("blue_mid", "Mid Lane:", choices = c("Select Champion" = "", mid_choices)),
          selectInput("blue_adc", "ADC:",      choices = c("Select Champion" = "", adc_choices)),
          selectInput("blue_sup", "Support:",  choices = c("Select Champion" = "", sup_choices))
        ),
        
        card(
          card_header(class = "bg-danger text-white", "Red Side Team Selection"),
          selectInput("red_top", "Top Lane:", choices = c("Select Champion" = "", top_choices)),
          selectInput("red_jng", "Jungle:",   choices = c("Select Champion" = "", jng_choices)),
          selectInput("red_mid", "Mid Lane:", choices = c("Select Champion" = "", mid_choices)),
          selectInput("red_adc", "ADC:",      choices = c("Select Champion" = "", adc_choices)),
          selectInput("red_sup", "Support:",  choices = c("Select Champion" = "", sup_choices))
        ),
        card(
          card_header(class = "bg-dark text-white", "Live Matchup Metrics"),
          
          # Metric 1: Dragon Control
          p(strong("First Dragon Control Probability")),
          div(class = "progress mb-3", style = "height: 15px;",
              div(id = "blue_bar", class = "progress-bar bg-primary", style = "width: 50%;"),
              div(id = "red_bar",  class = "progress-bar bg-danger",  style = "width: 50%;")
          ),
          
          # Metric 2: Efficiency Disparity
          p(strong("Predicted Team Efficiency Share")),
          div(class = "progress mb-4", style = "height: 10px;",
              div(id = "blue_eff_bar", class = "progress-bar bg-primary", style = "width: 50%;"),
              div(id = "red_eff_bar",  class = "progress-bar bg-danger",  style = "width: 50%;")
          ),
          
          # Metric 3: Highest Disparity Lane
          p(
            strong("Wackest Lane (Highest Stat Disparity): "), 
            textOutput("volatile_lane", inline = TRUE)
          )
        )
      )
    )
  ),
  
  nav_panel(
    title = "Champion Stats",
    
    card(
      card_header(class = "bg-dark text-white", "Filter by Role"),
      radioButtons("stats_role", "Role:",
                   choices = c(
                     "Top"     = "TOP",
                     "Jungle"  = "JUNGLE",
                     "Mid"     = "MIDDLE",
                     "ADC"     = "BOTTOM",
                     "Support" = "UTILITY"
                   ),
                   selected = "TOP",
                   inline = TRUE) # Forces items side-by-side
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
      # Include fallbacks for the new metrics so the UI doesn't break on load
      list(dragon_blue = 50, blue_eff_share = 50, max_disp_lane = "Awaiting Draft...")
    })
  })
  
  observeEvent(list(
    input$blue_top, input$blue_jng, input$blue_mid, input$blue_adc, input$blue_sup,
    input$red_top,  input$red_jng,  input$red_mid,  input$red_adc,  input$red_sup
  ), {
    metrics <- live_metrics()
    
    # Format Dragon Bar
    b_drag <- max(min(round(metrics$dragon_blue), 100), 0)
    r_drag <- 100 - b_drag
    
    # Format Efficiency Bar
    b_eff <- max(min(round(metrics$blue_eff_share), 100), 0)
    r_eff <- 100 - b_eff
    
    runjs(sprintf("
      document.getElementById('blue_bar').style.width = '%d%%';
      document.getElementById('red_bar').style.width  = '%d%%';
      document.getElementById('blue_eff_bar').style.width = '%d%%';
      document.getElementById('red_eff_bar').style.width  = '%d%%';
    ", b_drag, r_drag, b_eff, r_eff))
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # Render Text for the Highest Disparity Lane
  output$volatile_lane <- renderText({
    metrics <- live_metrics()
    metrics$max_disp_lane
  })
  
  output$champion_stats_table <- DT::renderDataTable({
    req(input$stats_role)
    
    if (input$stats_role == "JUNGLE") {
      tbl <- champion_profiles %>%
        filter(teamPosition == input$stats_role) %>%
        arrange(championName) %>%
        select(
          Champion             = championName,
          Position             = teamPosition,
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
          Champion             = championName,
          Position             = teamPosition,
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
        scrollX    = TRUE,
        scrollY    = "500px",       # Freezes titles by introducing an internal vertical scrollbox
        scrollCollapse = TRUE       # Shrinks container if there are fewer rows than the max height
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


