# ==============================================================================
# 1. SETUP & INSTANTIATION
# ==============================================================================
library(shiny)
library(bslib) 
library(dplyr)
library(tidyr)
library(workflows)
library(xgboost)

# Load your smooth model and aggregated reference statistics
final_smooth_model <- readRDS("final_smooth_model.rds")
champion_profiles  <- readRDS("champion_profiles.rds")

champion_choices <- as.character(sort(unique(champion_profiles$championName)))

# ==============================================================================
# 2. CORE DRAFT SIMULATION ALGO (DYNAMIC SELECTION FRIENDLY)
# ==============================================================================
simulate_final_draft <- function(blue_draft, red_draft, base_rate_leader = 0.68) {
  
  # Sanitize Reference Data Type Compatibility
  if (exists("champion_profiles")) {
    champion_profiles <- champion_profiles %>% 
      mutate(
        championName = as.character(championName),
        teamPosition = as.character(teamPosition)
      )
  }
  
  # Blue Side Data Frame Assembly
  blue_champs <- c(
    as.character(blue_draft$top), as.character(blue_draft$jng),
    as.character(blue_draft$mid), as.character(blue_draft$adc),
    as.character(blue_draft$sup)
  )
  blue_roles <- c("top", "jng", "mid", "adc", "sup")
  
  blue_stats <- data.frame(position = blue_roles, championName = blue_champs, stringsAsFactors = FALSE) %>%
    mutate(teamPosition = case_when(
      position == "top" ~ "TOP",
      position == "jng" ~ "JUNGLE",
      position == "mid" ~ "MIDDLE",
      position == "adc" ~ "BOTTOM",
      position == "sup" ~ "UTILITY"
    )) %>%
    left_join(champion_profiles, by = c("championName", "teamPosition")) %>%
    mutate(id = 1)
  
  # Red Side Data Frame Assembly
  red_champs <- c(
    as.character(red_draft$top), as.character(red_draft$jng),
    as.character(red_draft$mid), as.character(red_draft$adc),
    as.character(red_draft$sup)
  )
  red_roles <- c("top", "jng", "mid", "adc", "sup")
  
  red_stats <- data.frame(position = red_roles, championName = red_champs, stringsAsFactors = FALSE) %>%
    mutate(teamPosition = case_when(
      position == "top" ~ "TOP",
      position == "jng" ~ "JUNGLE",
      position == "mid" ~ "MIDDLE",
      position == "adc" ~ "BOTTOM",
      position == "sup" ~ "UTILITY"
    )) %>%
    left_join(champion_profiles, by = c("championName", "teamPosition")) %>%
    mutate(id = 1)
  
  # Safety Net: Fill NA profiles from unmatched dropdown items with neutral 0s
  blue_stats[is.na(blue_stats)] <- 0
  red_stats[is.na(red_stats)]   <- 0
  
  # Pivot wide for full match rows
  blue_wide <- blue_stats %>%
    pivot_wider(
      id_cols = id, names_from = position, 
      values_from = c(base_gold_pm, base_efficiency, base_stomp, base_proximity, base_roaming, base_crab_count),
      names_glue = "{.value}_{position}_blue"
    )
  
  red_wide <- red_stats %>%
    pivot_wider(
      id_cols = id, names_from = position, 
      values_from = c(base_gold_pm, base_efficiency, base_stomp, base_proximity, base_roaming, base_crab_count),
      names_glue = "{.value}_{position}_red"
    )
  
  # Compute Map Metrics and Predictor Columns
  simulated_match <- blue_wide %>%
    inner_join(red_wide, by = "id") %>%
    mutate(
      matchId        = as.factor("SIM_MATCH_01"),
      global_ult_gap = as.numeric(0),
      initialCrabCount = as.integer(round(base_crab_count_jng_blue + base_crab_count_jng_red)),
      
      blue_gold      = base_gold_pm_top_blue + base_gold_pm_jng_blue + base_gold_pm_mid_blue + base_gold_pm_adc_blue + base_gold_pm_sup_blue,
      red_gold       = base_gold_pm_top_red  + base_gold_pm_jng_red  + base_gold_pm_mid_red  + base_gold_pm_adc_red  + base_gold_pm_sup_red,
      blue_is_leader = blue_gold >= red_gold,
      
      top_gold_lead    = base_gold_pm_top_blue - base_gold_pm_top_red,
      jungle_gold_lead = base_gold_pm_jng_blue - base_gold_pm_jng_red,
      mid_gold_lead    = base_gold_pm_mid_blue - base_gold_pm_mid_red,
      adc_gold_lead    = base_gold_pm_adc_blue - base_gold_pm_adc_red,
      supp_gold_lead   = base_gold_pm_sup_blue - base_gold_pm_sup_red,
      
      top_gold_eff_lead    = base_efficiency_top_blue - base_efficiency_top_red,
      jungle_gold_eff_lead = base_efficiency_jng_blue - base_efficiency_jng_red,
      mid_gold_eff_lead    = base_efficiency_mid_blue - base_efficiency_mid_red,
      adc_gold_eff_lead    = base_efficiency_adc_blue - base_efficiency_adc_red,
      supp_gold_eff_lead   = base_efficiency_sup_blue - base_efficiency_sup_red,
      
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
  
  # Dynamic Directional Sign Flip if Red side holds Macro Lead
  if (!simulated_match$blue_is_leader[1]) {
    predictor_cols <- names(simulated_match)[grepl("_lead$|_gap$", names(simulated_match))]
    simulated_match[predictor_cols] <- -1 * simulated_match[predictor_cols]
  }
  
  model_ready_data <- simulated_match %>%
    select(matchId, initialCrabCount, global_ult_gap, ends_with("_lead"), ends_with("_gap"))
  
  # If both sides are completely unselected, avoid division by zero anomalies
  if (simulated_match$blue_gold[1] == 0 && simulated_match$red_gold[1] == 0) {
    prob_blue  <- 0.5
    gold_share <- 50
    agg_share  <- 50
  } else {
    # Predict probabilities via smooth classification workflow
    probabilities <- predict(final_smooth_model, model_ready_data, type = "prob")
    
    # SAFE POSITIONAL EXTRACTION: Grab the first row of the second column, regardless of its name
    prob_won_drag <- as.numeric(probabilities[[2]][1])
    
    if (simulated_match$blue_is_leader[1]) {
      prob_blue <- prob_won_drag
    } else {
      prob_blue <- 1 - prob_won_drag
    }
    
    # Safely compute relative ratios for partially complete selections
    total_gold <- simulated_match$blue_gold[1] + simulated_match$red_gold[1]
    gold_share <- if (total_gold > 0) (simulated_match$blue_gold[1] / total_gold) * 100 else 50
    
    blue_agg <- sum(blue_stats$base_efficiency, na.rm = TRUE)
    red_agg  <- sum(red_stats$base_efficiency, na.rm = TRUE)
    agg_share <- if ((blue_agg + red_agg) > 0) (blue_agg / (blue_agg + red_agg)) * 100 else 50
  }
  
  return(list(
    dragon_blue = as.numeric(prob_blue * 100),
    gold_blue   = as.numeric(gold_share),
    agg_blue    = as.numeric(agg_share)
  ))
}
# ==============================================================================
# 3. USER INTERFACE SPECIFICATION
# ==============================================================================
ui <- page_sidebar(
  title = "LoL Draft Simulator (with dragon prediction)",
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  
  sidebar = sidebar(
    title = "Simulation Dashboard",
    width = 300,
    p()
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
      uiOutput("metric_bars") 
    )
  )
)

# ==============================================================================
# 4. SERVER ENGINE
# ==============================================================================
server <- function(input, output, session) {
  
  live_metrics <- reactive({
    if (input$blue_top == "" && input$blue_jng == "" && input$blue_mid == "" && 
        input$blue_adc == "" && input$blue_sup == "" && input$red_top == "" && 
        input$red_jng == "" && input$red_mid == "" && input$red_adc == "" && 
        input$red_sup == "") {
      return(list(dragon_blue = 50))
    }
    
    blue_team <- list(top = input$blue_top, jng = input$blue_jng, mid = input$blue_mid, adc = input$blue_adc, sup = input$blue_sup)
    red_team  <- list(top = input$red_top, jng = input$red_jng, mid = input$red_mid, adc = input$red_adc, sup = input$red_sup)
    
    tryCatch({
      simulate_final_draft(blue_draft = blue_team, red_draft = red_team)
    }, error = function(e) {
      list(dragon_blue = 50)
    })
  })
  
  output$metric_bars <- renderUI({
    metrics <- live_metrics()
    
    b_drag <- max(min(round(metrics$dragon_blue), 100), 0)
    r_drag <- 100 - b_drag
    
    tagList(
      p(strong("First Dragon Control Probability")),
      div(class = "progress", style = "height: 35px; font-weight: bold;",
          div(class = "progress-bar bg-primary", style = paste0("width: ", b_drag, "%; font-size: 1.1rem;"), paste0("Blue: ", b_drag, "%")),
          div(class = "progress-bar bg-danger",  style = paste0("width: ", r_drag, "%; font-size: 1.1rem;"), paste0("Red: ", r_drag, "%"))
      )
    )
  })
}
# Launch Application Instance
shinyApp(ui = ui, server = server)