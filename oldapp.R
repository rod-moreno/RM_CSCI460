
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

# PATCH: champion_profiles_fallback needs to exist before
# simulate_final_draft() can be called (it's now a required argument).
# Ideally this is built from processed_data (raw per-player rows), but
# app.R only ships the pre-aggregated champion_profiles.rds. As a
# stand-in that needs no new files, derive a role-agnostic fallback by
# averaging each champion's per-role stats, weighted by games_played.
# If you later save processed_data.rds alongside the other .rds files,
# swap this for build_champion_profiles_fallback(processed_data)
# (defined below) for a more faithful role-agnostic average.
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

# ==============================================================================
# 2. CORE DRAFT SIMULATION ALGO (DYNAMIC SELECTION FRIENDLY)
# ==============================================================================
build_champion_profiles_fallback <- function(processed_data) {
  processed_data %>%
    group_by(championName) %>%
    summarise(
      base_gold_pm    = mean(early_gold_per_min, na.rm = TRUE),
      base_efficiency = mean(early_gold_per_min, na.rm = TRUE) -
        (mean(early_cs_per_min, na.rm = TRUE) * 20),
      base_stomp      = mean(earlyLaningPhaseGoldExpAdvantage, na.rm = TRUE),
      base_proximity  = mean(jungle_proximity_pct, na.rm = TRUE),
      base_roaming    = mean(roaming_pct, na.rm = TRUE),
      base_crab_count = 0,
      games_played_fb = n(),
      .groups = "drop"
    ) %>%
    mutate(championName = as.character(championName))
}


simulate_final_draft <- function(blue_draft, red_draft,
                                 champion_profiles,
                                 champion_profiles_fallback,
                                 base_rate_leader = 0.68) {
  
  champion_profiles <- champion_profiles %>%
    mutate(
      championName = as.character(championName),
      teamPosition = as.character(teamPosition)
    )
  
  champion_profiles_fallback <- champion_profiles_fallback %>%
    mutate(championName = as.character(championName))
  
  stat_cols <- c("base_gold_pm", "base_efficiency", "base_stomp",
                 "base_proximity", "base_roaming", "base_crab_count")
  
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
    
    with_fallback <- with_fallback %>%
      mutate(
        had_exact    = !is.na(base_gold_pm) &
          !is.na(role_specific[[stat_cols[1]]]),
        had_fallback = !is.na(.data[[paste0(stat_cols[1], "_fb")]]),
        impute_status = case_when(
          !is.na(role_specific$base_gold_pm) ~ "exact",
          had_fallback                       ~ "fallback",
          TRUE                                ~ "unknown"
        )
      )
    
    log_rows <- with_fallback %>%
      filter(impute_status != "exact") %>%
      transmute(
        side      = side_label,
        position  = position,
        champion  = championName,
        status    = impute_status
      )
    
    for (col in stat_cols) {
      with_fallback[[col]][is.na(with_fallback[[col]])] <- 0
    }
    
    stats <- with_fallback %>%
      select(position, championName, teamPosition, all_of(stat_cols)) %>%
      mutate(id = 1)
    
    list(stats = stats, log = log_rows)
  }
  
  blue_result <- assemble_side(blue_draft, "blue")
  red_result  <- assemble_side(red_draft,  "red")
  
  blue_stats <- blue_result$stats
  red_stats  <- red_result$stats
  
  imputation_log <- bind_rows(blue_result$log, red_result$log)
  
  if (nrow(imputation_log) > 0) {
    message("Note: ", nrow(imputation_log),
            " pick(s) used imputed (non-role-specific or unknown) data:")
    for (i in seq_len(nrow(imputation_log))) {
      message("  [", imputation_log$side[i], " ", imputation_log$position[i], "] ",
              imputation_log$champion[i], " -> ", imputation_log$status[i])
    }
  }
  
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
  
  simulated_match <- blue_wide %>%
    inner_join(red_wide, by = "id") %>%
    mutate(
      matchId        = as.factor("SIM_MATCH_01"),
      global_ult_gap = as.numeric(0),
      initialCrabCount = as.integer(round(base_crab_count_jng_blue + base_crab_count_jng_red)),
      
      blue_gold      = base_gold_pm_top_blue + base_gold_pm_jng_blue + base_gold_pm_mid_blue + base_gold_pm_adc_blue + base_gold_pm_sup_blue,
      red_gold       = base_gold_pm_top_red  + base_gold_pm_jng_red  + base_gold_pm_mid_red  + base_gold_pm_adc_red  + base_gold_pm_sup_red,
      blue_is_leader = blue_gold >= red_gold,  # STILL-FRAGILE: see note above function
      
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
  
  if (!simulated_match$blue_is_leader[1]) {
    predictor_cols <- names(simulated_match)[grepl("_lead$|_gap$", names(simulated_match))]
    simulated_match[predictor_cols] <- -1 * simulated_match[predictor_cols]
  }
  
  model_ready_data <- simulated_match %>%
    select(matchId, initialCrabCount, global_ult_gap, ends_with("_lead"), ends_with("_gap"))
  
  if (simulated_match$blue_gold[1] == 0 && simulated_match$red_gold[1] == 0) {
    prob_blue  <- 0.5
    gold_share <- 50
    agg_share  <- 50
  } else {
    probabilities <- predict(final_smooth_model, model_ready_data, type = "prob")
    prob_won_drag <- as.numeric(probabilities[[2]][1])
    
    if (simulated_match$blue_is_leader[1]) {
      prob_blue <- prob_won_drag
    } else {
      prob_blue <- 1 - prob_won_drag
    }
    
    total_gold <- simulated_match$blue_gold[1] + simulated_match$red_gold[1]
    gold_share <- if (total_gold > 0) (simulated_match$blue_gold[1] / total_gold) * 100 else 50
    
    blue_agg <- sum(blue_stats$base_efficiency, na.rm = TRUE)
    red_agg  <- sum(red_stats$base_efficiency, na.rm = TRUE)
    agg_share <- if ((blue_agg + red_agg) > 0) (blue_agg / (blue_agg + red_agg)) * 100 else 50
  }
  
  return(list(
    dragon_blue     = as.numeric(prob_blue * 100),
    gold_blue       = as.numeric(gold_share),
    agg_blue        = as.numeric(agg_share),
    imputation_log  = imputation_log
  ))
}
# ==============================================================================
# 3. USER INTERFACE SPECIFICATION
# ==============================================================================

ui <- page_navbar(
  title = "LoL Draft Simulator (with dragon prediction)",
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      .progress-bar { transition: width 1s ease !important; }
    "))
  ),
  
  nav_panel(
    title = "Draft Simulator",
    
    layout_sidebar(
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
          p(strong("First Dragon Control Probability")),
          div(class = "progress", style = "height: 10px;",
              div(id = "blue_bar", class = "progress-bar bg-primary", style = "width: 50%;"),
              div(id = "red_bar",  class = "progress-bar bg-danger",  style = "width: 50%;")
          )
        )
      )
    )
  ),
  
  # PATCH: champion stats browser. Default view is all champions for a
  # chosen role, alphabetical by champion name. Role filter replaces
  # the old single-champion dropdown per spec. Columns are renamed to
  # human-readable labels at display time (raw column names in
  # champion_profiles are left untouched so simulate_final_draft()
  # still works against the original names).
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
    
    if (input$blue_top == "" && input$blue_jng == "" && input$blue_mid == "" && 
        input$blue_adc == "" && input$blue_sup == "" && input$red_top == "" && 
        input$red_jng == "" && input$red_mid == "" && input$red_adc == "" && 
        input$red_sup == "") {
      
      return(list(dragon_blue = 50))
      
    }
    
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
  
  # PATCH: Champion Stats tab logic. Filters to the selected role,
  # sorts alphabetically by champion, and renames columns to
  # human-readable labels for display only. "Avg. Early Crabs" only
  # appears for JUNGLE, since base_crab_count is defined as 0 for
  # every non-jungle role in champion_profiles (see finalmodel.R / the
  # base_crab_count construction) and showing it elsewhere would just
  # be a column of zeros.
  #
  # NOTE: base_crab_count is a mean COUNT of early crabs (0/1/2-ish),
  # not a percentage, so it's labeled "Avg. Early Crabs" rather than
  # "Double Crab%" here. A true Double Crab% would need to be built
  # from raw processed_data (mean(initialCrabCount >= 2) * 100) rather
  # than derived from the existing aggregated champion_profiles table.
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
    
    # formatPercentage() multiplies by 100 and appends "%" for display
    # while keeping the underlying value numeric for correct sorting.
    # "Avg. Early Crabs" only exists in the JUNGLE branch so it's
    # formatted conditionally to avoid a column-not-found error on
    # other roles.
    pct_cols <- c("Lane Stomp%", "Jungle Proximity%", "Roaming%",
                  "Win%", "Kill Participation%")
    
    dt <- DT::datatable(
      tbl,
      rownames = FALSE,
      options  = list(
        pageLength = 25,
        order      = list(list(0, "asc")),  # default sort: Champion A-Z
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