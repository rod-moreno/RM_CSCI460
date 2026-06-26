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