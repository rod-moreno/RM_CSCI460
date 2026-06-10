library(httr2)
library(jsonlite)
library(tidyverse)
library(tidymodels)
library(xgboost)
library(vip)
library(LiblineaR)
library(themis)
#Read in Riot API Key (from secret file! so no leaks)
readRenviron(".Renviron")
api_key <- Sys.getenv("RIOT_API_KEY")


#Sanity check to see if api_key works
test_url <- "https://na1.api.riotgames.com/lol/status/v4/platform-data"
resp <- request(test_url) %>%
  req_headers("X-Riot-Token" = api_key) %>%
  req_error(is_error = function(resp) FALSE) %>% # Prevents R from crashing if it fails
  req_perform()
message("Riot API Connection Status: ", resp$status_code)
rm(resp)

#Hard-coding global ults to use for later
global_ult_champions <- c(
  "Karthus", "Soraka", "Gangplank", "Ezreal", "Jinx", "Ashe", "Senna", "Draven",
  "Shen", "Twisted Fate", "Pantheon", "Nocturne", "Galio", "Briar", "Ryze", "Taliyah", "Akshan", "Sion", 
  "Ornn", "Xerath", "Vex", "Ziggs"
)


#Function to pull match history from PUUID
gethistory <- function (x) {
  ### Get recent ranked queue matches from that player
  match_ids_url <- paste0("https://americas.api.riotgames.com/lol/match/v5/matches/by-puuid/", 
                          x, "/ids?queue=420&start=0&count=5")
  
  #saving API response into a list of 5 most recent matches
  match_ids_request <- request(match_ids_url) %>%
    req_headers("X-Riot-Token" = api_key)
  match_ids <- req_perform(match_ids_request) %>% resp_body_json()
  return(unlist(match_ids))
}


#Function to get PUUIDs from any match
getpuuids <- function(x) {
  m_url <- paste0("https://americas.api.riotgames.com/lol/match/v5/matches/",x)
  match_info <- request(m_url) %>%
    req_headers("X-Riot-Token" = api_key) %>%
    req_perform() %>%
    resp_body_json()
  puuids <- match_info$metadata$participant
  return(unlist(puuids))
}


#Function to provide seed matches and PUUID
seedplayer <- function(x, y) {
  puuid_url <- paste0("https://americas.api.riotgames.com/riot/account/v1/accounts/by-riot-id/", 
                      x, "/", y) #Get API call url
  request <- request(puuid_url) %>% #API request using game name and tagline
    req_headers("X-Riot-Token" = api_key)
  response <- req_perform(request)
  user_data <- resp_body_json(response)
  
  puuid1 <- user_data$puuid
  print(paste("PUUID is:", puuid1))
  return(puuid1)
}

#Function to take match details from "matches" request
get_match_details <- function(match_id) {
  url <- paste0("https://americas.api.riotgames.com/lol/match/v5/matches/", match_id)
  resp <- request(url) %>%
    req_headers("X-Riot-Token" = api_key) %>%
    req_perform() %>%
    resp_body_json() 
  
  return(resp)
}


#Function to take timeline details
get_match_timeline <- function(match_id) {
  url <- paste0("https://americas.api.riotgames.com/lol/match/v5/matches/", match_id, "/timeline")
  
  resp <- request(url) %>%
    req_headers("X-Riot-Token" = api_key) %>%
    req_perform() %>%
    resp_body_json()
  
  return(resp)
}


extract_target_features <- function(player_json, match_id) {
  
  # 1. Safely isolate the nested challenges layer
  # (If a player disconnected or has no challenges, create an empty list)
  challs <- player_json$challenges
  if (is.null(challs)) challs <- list()
  
  # Helper function to safely extract NULL values from nested lists
  safely_get <- function(source_list, key, default = 0) {
    val <- source_list[[key]]
    if (is.null(val)) return(default)
    return(val)
  }
  
  # 2. Build the exact flat row structure for this player
  player_row <- tibble(
    matchId                           = match_id,
    participantId                     = safely_get(player_json, "participantId"),
    
    # --- Base Info Layer Metrics ---
    championName                      = safely_get(player_json, "championName", "Unknown"),
    teamId                            = safely_get(player_json, "teamId"),
    teamPosition                      = safely_get(player_json, "teamPosition", "UNKNOWN"),
    totalMinionsKilled                = safely_get(player_json, "totalMinionsKilled"),
    kills                             = safely_get(player_json, "kills"),
    assists                           = safely_get(player_json, "assists"),
    win                               = if_else(safely_get(player_json, "win", FALSE) == TRUE, 1, 0), # Label
    
    # --- ChallengesDTO Layer Metrics ---
    firstTowerKill                    = if_else(safely_get(challs, "firstTowerKill", FALSE) == TRUE, 1, 0),
    firstBloodKill                    = if_else(safely_get(challs, "firstBloodKill", FALSE) == TRUE, 1, 0),
    firstBloodAssist                  = if_else(safely_get(challs, "firstBloodAssist", FALSE) == TRUE, 1, 0),
    quickSoloKills                    = safely_get(challs, "quickSoloKills"),
    quickFirstTurret                  = if_else(safely_get(challs, "quickFirstTurret", FALSE) == TRUE, 1, 0),
    initialCrabCount                  = safely_get(challs, "initialCrabCount"),
    teamRiftHeraldKills               = if_else(safely_get(challs, "teamRiftHeraldKills", FALSE) == TRUE, 1, 0), 
    earliestDragonTakedown            = safely_get(challs, "earliestDragonTakedown", 0), 
    jungleKillEarlyJungle             = safely_get(challs, "jungleKillEarlyJungle"),
    KillsOnLanersEarlyJungleAsJungler = safely_get(challs, "KillsOnLanersEarlyJungleAsJungler"),
    KillsOnLanersEarlyJungleAsLaner   = safely_get(challs, "KillsOnLanersEarlyJungleAsLaner"),
    earlyLaningPhaseGoldExpAdvantage  = if_else(safely_get(challs, "earlyLaningPhaseGoldExpAdvantage", FALSE) == TRUE, 1, 0)
  )
  
  return(player_row)
}


extract_timeline_summary <- function(m_id) {
  # 1. Fetch raw timeline
  raw_timeline <- get_match_timeline(m_id)
  
  # 2. Extract frames 0 through 7 (Indices 1 to 8)
  early_frames <- raw_timeline$info$frames[1:8]
  
  # 3. Parse minute-by-minute positions and values
  minute_level_df <- map_df(seq_along(early_frames), function(frame_idx) {
    minute <- frame_idx - 1
    p_frames <- early_frames[[frame_idx]]$participantFrames
    
    map_df(1:10, function(p_id) {
      player_state <- p_frames[[as.character(p_id)]]
      tibble(
        matchId       = m_id,
        participantId = as.integer(p_id),
        minute        = minute,
        totalGold     = player_state$totalGold,
        totalCS       = player_state$minionsKilled + player_state$jungleMinionsKilled,
        x             = if (!is.null(player_state$position)) player_state$position$x else NA,
        y             = if (!is.null(player_state$position)) player_state$position$y else NA
      )
    })
  })
  
  # 4. Assign Map Zones based on Summoner's Rift Geometry
  minute_level_df <- minute_level_df %>% 
    mutate(
      lane_width = 2000,
      max_grid   = 16000,
      map_zone = case_when(
        is.na(x) | is.na(y) ~ "Dead/Unknown",
        x < 3000 & y < 3000 ~ "Blue Base",
        x > 13000 & y > 13000 ~ "Red Base",
        abs(x - y) < lane_width ~ "Mid Lane",
        x < lane_width | y > (max_grid - lane_width) ~ "Top Lane",
        y < lane_width | x > (max_grid - lane_width) ~ "Bot Lane",
        TRUE ~ "Jungle"
      )
    )
  
  # 5. Calculate Jungler Paths for Proximity
  jungler_coords <- minute_level_df %>% 
    filter(participantId %in% c(2, 7)) %>% 
    select(minute, participantId, jng_x = x, jng_y = y) %>% 
    mutate(teamId = if_else(participantId == 2, 100, 200)) %>% 
    select(-participantId)
  
  # 6. Compress into 1 summary row per player
  timeline_summary <- minute_level_df %>% 
    mutate(teamId = if_else(participantId <= 5, 100, 200)) %>% 
    left_join(jungler_coords, by = c("minute", "teamId")) %>% 
    
    # Calculate tracking metrics
    mutate(
      dist_to_jng = sqrt((x - jng_x)^2 + (y - jng_y)^2),
      is_near_jng = if_else(!is.na(dist_to_jng) & dist_to_jng <= 2000, 1, 0),
      
      # Hardcoded structural assignment based on metadata rules
      assigned_lane = case_when(
        participantId %in% c(1, 6)  ~ "Top Lane",
        participantId %in% c(3, 8)  ~ "Mid Lane",
        participantId %in% c(4, 5, 9, 10) ~ "Bot Lane",
        TRUE ~ "Jungle" # Junglers
      ),
      
      # Roaming logic: From minute 3 onwards, are they out of their assigned lane?
      # Exclude times they are sitting in base or dead so back timings don't skew the metric.
      is_roaming = if_else(
        minute >= 3 & 
          assigned_lane != "Jungle" & 
          map_zone != assigned_lane & 
          !map_zone %in% c("Blue Base", "Red Base", "Dead/Unknown"), 
        1, 0
      )
    ) %>% 
    
    group_by(matchId, participantId) %>% 
    summarise(
      # Rate metrics at minute 7
      early_cs_per_min   = max(totalCS, na.rm = TRUE) / 7,
      early_gold_per_min = max(totalGold, na.rm = TRUE) / 7,
      
      # Percentage metrics
      jungle_proximity_pct = mean(is_near_jng, na.rm = TRUE),
      
      # Roaming percentage calculated strictly on minutes 3 to 7 (10 frames total)
      # Junglers automatically receive a 0% roaming rate since they belong in the jungle
      roaming_pct = if_else(unique(assigned_lane) == "Jungle", 0, sum(is_roaming, na.rm = TRUE) / 7),
      
      .groups = "drop"
    )
  
  return(timeline_summary)
}


Since we have moved to a "Blind Leader" engine, your simulation function needs to be precise. It must calculate the gold state for both sides, identify who the "Leader" is, calculate the gaps relative to that leader, and then map the final probability back to Blue/Red for your display.

Here is the final, optimized version of your simulator function:
  R

simulate_final_draft <- function(blue_draft, red_draft, scuttle_count = 1L) {
  
  # 1. Pull and Pivot Blue stats
  blue_stats <- tibble(position = names(blue_draft), championName = blue_draft) %>%
    left_join(champion_profiles, by = "championName") %>%
    mutate(id = 1) %>% 
    pivot_wider(
      id_cols = id, names_from = position, 
      values_from = c(base_gold_pm, base_efficiency, base_stomp, base_proximity, base_roaming),
      names_glue = "{.value}_{position}_blue"
    )
  
  # 2. Pull and Pivot Red stats
  red_stats <- tibble(position = names(red_draft), championName = red_draft) %>%
    left_join(champion_profiles, by = "championName") %>%
    mutate(id = 1) %>% 
    pivot_wider(
      id_cols = id, names_from = position, 
      values_from = c(base_gold_pm, base_efficiency, base_stomp, base_proximity, base_roaming),
      names_glue = "{.value}_{position}_red"
    )
  
  # 3. Compute Blind Leader-Centric Gaps
  # This matches the logic of your training dataset exactly
  simulated_match <- blue_stats %>%
    inner_join(red_stats, by = "id") %>%
    mutate(
      initialCrabCount = as.integer(scuttle_count),
      matchId = as.factor("SIM_MATCH_01"),
      
      # Determine who the "Leader" is based on total gold
      blue_gold = base_gold_pm_top_blue + base_gold_pm_jng_blue + base_gold_pm_mid_blue + base_gold_pm_adc_blue + base_gold_pm_sup_blue,
      red_gold  = base_gold_pm_top_red + base_gold_pm_jng_red + base_gold_pm_mid_red + base_gold_pm_adc_red + base_gold_pm_sup_red,
      blue_is_leader = blue_gold >= red_gold,
      total_gold_advantage = abs(blue_gold - red_gold),
      global_ult_gap = as.numeric(0),
      
      # Calculate gaps: Leader minus Trailer
      top_gold_gap    = if_else(blue_is_leader, base_gold_pm_top_blue - base_gold_pm_top_red, base_gold_pm_top_red - base_gold_pm_top_blue),
      jungle_gold_gap = if_else(blue_is_leader, base_gold_pm_jng_blue - base_gold_pm_jng_red, base_gold_pm_jng_red - base_gold_pm_jng_blue),
      mid_gold_gap    = if_else(blue_is_leader, base_gold_pm_mid_blue - base_gold_pm_mid_red, base_gold_pm_mid_red - base_gold_pm_mid_blue),
      adc_gold_gap    = if_else(blue_is_leader, base_gold_pm_adc_blue - base_gold_pm_adc_red, base_gold_pm_adc_red - base_gold_pm_adc_blue),
      supp_gold_gap   = if_else(blue_is_leader, base_gold_pm_sup_blue - base_gold_pm_sup_red, base_gold_pm_sup_red - base_gold_pm_sup_blue),
      
      top_gold_eff_gap    = if_else(blue_is_leader, base_efficiency_top_blue - base_efficiency_top_red, base_efficiency_top_red - base_efficiency_top_blue),
      jungle_gold_eff_gap = if_else(blue_is_leader, base_efficiency_jng_blue - base_efficiency_jng_red, base_efficiency_jng_red - base_efficiency_jng_blue),
      mid_gold_eff_gap    = if_else(blue_is_leader, base_efficiency_mid_blue - base_efficiency_mid_red, base_efficiency_mid_red - base_efficiency_mid_blue),
      adc_gold_eff_gap    = if_else(blue_is_leader, base_efficiency_adc_blue - base_efficiency_adc_red, base_efficiency_adc_red - base_efficiency_adc_blue),
      supp_gold_eff_gap   = if_else(blue_is_leader, base_efficiency_sup_blue - base_efficiency_sup_red, base_efficiency_sup_red - base_efficiency_sup_blue),
      
      top_stomp_gap    = if_else(blue_is_leader, base_stomp_top_blue - base_stomp_top_red, base_stomp_top_red - base_stomp_top_blue),
      jungle_stomp_gap = if_else(blue_is_leader, base_stomp_jng_blue - base_stomp_jng_red, base_stomp_jng_red - base_stomp_jng_blue),
      mid_stomp_gap    = if_else(blue_is_leader, base_stomp_mid_blue - base_stomp_mid_red, base_stomp_mid_red - base_stomp_mid_blue),
      adc_stomp_gap    = if_else(blue_is_leader, base_stomp_adc_blue - base_stomp_adc_red, base_stomp_adc_red - base_stomp_adc_blue),
      supp_stomp_gap   = if_else(blue_is_leader, base_stomp_sup_blue - base_stomp_sup_red, base_stomp_sup_red - base_stomp_sup_blue),
      
      top_prox_gap  = if_else(blue_is_leader, base_proximity_top_blue - base_proximity_top_red, base_proximity_top_red - base_proximity_top_blue),
      mid_prox_gap  = if_else(blue_is_leader, base_proximity_mid_blue - base_proximity_mid_red, base_proximity_mid_red - base_proximity_mid_blue),
      adc_prox_gap  = if_else(blue_is_leader, base_proximity_adc_blue - base_proximity_adc_red, base_proximity_adc_red - base_proximity_adc_blue),
      supp_prox_gap = if_else(blue_is_leader, base_proximity_sup_blue - base_proximity_sup_red, base_proximity_sup_red - base_proximity_sup_blue),
      
      mid_roam_gap  = if_else(blue_is_leader, base_roaming_mid_blue - base_roaming_mid_red, base_roaming_mid_red - base_roaming_mid_blue),
      adc_roam_gap  = if_else(blue_is_leader, base_roaming_adc_blue - base_roaming_adc_red, base_roaming_adc_red - base_roaming_adc_blue),
      supp_roam_gap = if_else(blue_is_leader, base_roaming_sup_blue - base_roaming_sup_red, base_roaming_sup_red - base_roaming_sup_blue)
    )
  
  # 4. Predict
  probabilities <- predict(final_dragon_fit, simulated_match, type = "prob")
  
  # Identify which column index actually correlates to the "Leader" winning.
  # If your output is skewed, it means the model thinks the other class is the winner.
  # Let's explicitly define them:
  prob_leader  <- probabilities$.pred_Leader_Drag
  prob_trailer <- probabilities$.pred_Trailer_Drag
  
  # IF your output is inverted, swap the probabilities here:
   prob_leader <- probabilities$.pred_Trailer_Drag
   prob_trailer <- probabilities$.pred_Leader_Drag
  
  # Map back to Blue/Red
  if (simulated_match$blue_is_leader[1]) {
    prob_blue <- prob_leader
    prob_red  <- prob_trailer
  } else {
    prob_blue <- prob_trailer
    prob_red  <- prob_leader
  }
  
  # 5. Output Report
  cat("\n======================================================\n")
  cat("      SIMULATOR V3: BLIND LEADER ENGINE               \n")
  cat("======================================================\n")
  cat("Advantage Holder: ", if_else(simulated_match$blue_is_leader[1], "Blue Side", "Red Side"), "\n")
  cat("Prob. Blue Secures First Dragon: ", round(prob_blue * 100, 1), "%\n")
  cat("Prob. Red Secures First Dragon:  ", round(prob_red * 100, 1), "%\n")
  cat("======================================================\n")
  
  return(simulated_match)
}
