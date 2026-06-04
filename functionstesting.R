library(httr2)
library(jsonlite)
library(tidyverse)
library(tidymodels)
library(xgboost)
library(vip)
library(fast)
#Read in Riot API Key (from secret file! so no leaks)
readRenviron(".Renviron")
api_key <- Sys.getenv("RIOT_API_KEY")


#Sanity check to see if api_key works

test_url <- "https://na1.api.riotgames.com/lol/status/v4/platform-data"
resp <- request(test_url) |>
  req_headers("X-Riot-Token" = api_key) |>
  req_error(is_error = function(resp) FALSE) |> # Prevents R from crashing if it fails
  req_perform()
message("Riot API Connection Status: ", resp$status_code)
rm(resp)

#Function 
gethistory <- function (x) {
  ### Get recent ranked queue matches from that player
  match_ids_url <- paste0("https://americas.api.riotgames.com/lol/match/v5/matches/by-puuid/", 
                          x, "/ids?queue=420&start=0&count=5")
  
  #saving API response into a list of 5 most recent matches
  match_ids_request <- request(match_ids_url) |>
    req_headers("X-Riot-Token" = api_key)
  match_ids <- req_perform(match_ids_request) |> resp_body_json()
  return(unlist(match_ids))
}


#Function to get PUUIDs from any match
getpuuids <- function(x) {
  m_url <- paste0("https://americas.api.riotgames.com/lol/match/v5/matches/",x)
  match_info <- request(m_url) |>
    req_headers("X-Riot-Token" = api_key) |>
    req_perform() |>
    resp_body_json()
  puuids <- match_info$metadata$participant
  return(unlist(puuids))
}


#Function to provide seed matches and PUUID
seedplayer <- function(x, y) {
  puuid_url <- paste0("https://americas.api.riotgames.com/riot/account/v1/accounts/by-riot-id/", 
                      x, "/", y) #Get API call url
  request <- request(puuid_url) |> #API request using game name and tagline
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
  
  resp <- request(url) |>
    req_headers("X-Riot-Token" = api_key) |>
    req_perform() |>
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
  
  # 2. Extract frames 0 through 12 (Indices 1 to 13)
  early_frames <- raw_timeline$info$frames[1:13]
  
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
  minute_level_df <- minute_level_df |> 
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
  jungler_coords <- minute_level_df |> 
    filter(participantId %in% c(2, 7)) |> 
    select(minute, participantId, jng_x = x, jng_y = y) |> 
    mutate(teamId = if_else(participantId == 2, 100, 200)) |> 
    select(-participantId)
  
  # 6. Compress into 1 summary row per player
  timeline_summary <- minute_level_df |> 
    mutate(teamId = if_else(participantId <= 5, 100, 200)) |> 
    left_join(jungler_coords, by = c("minute", "teamId")) |> 
    
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
    ) |> 
    
    group_by(matchId, participantId) |> 
    summarise(
      # Rate metrics at minute 12
      early_cs_per_min   = max(totalCS, na.rm = TRUE) / 12,
      early_gold_per_min = max(totalGold, na.rm = TRUE) / 12,
      
      # Percentage metrics
      jungle_proximity_pct = mean(is_near_jng, na.rm = TRUE),
      
      # Roaming percentage calculated strictly on minutes 3 to 12 (10 frames total)
      # Junglers automatically receive a 0% roaming rate since they belong in the jungle
      roaming_pct = if_else(unique(assigned_lane) == "Jungle", 0, sum(is_roaming, na.rm = TRUE) / 10),
      
      .groups = "drop"
    )
  
  return(timeline_summary)
}


simulate_champion_draft <- function(blue_champions, red_champions, scuttle_count = 1) {
  
  # 1. Extract and aggregate Blue side features using your engineered formulas
  blue_simated_stats <- champion_profiles |> 
    filter(championName %in% blue_champions) |> 
    summarise(
      blue_gold      = sum(base_gold_per_min),
      # Replicating your custom efficiency metric: Total Gold / Total CS
      blue_efficiency = sum(base_gold_per_min) / sum(base_cs_per_min),
      blue_prox       = mean(base_proximity),
      blue_roam       = mean(base_roaming),
      blue_globals    = sum(has_global_ult)
    )
  
  # 2. Extract and aggregate Red side features
  red_simulated_stats <- champion_profiles |> 
    filter(championName %in% red_champions) |> 
    summarise(
      red_gold        = sum(base_gold_per_min),
      red_efficiency  = sum(base_gold_per_min) / sum(base_cs_per_min),
      red_prox        = mean(base_proximity),
      red_roam        = mean(base_roaming),
      red_globals     = sum(has_global_ult)
    )
  
  # 3. Combine into a single wide match matrix matching h2h_log_fit
  simulated_game <- bind_cols(blue_simated_stats, red_simulated_stats) |> 
    mutate(initialCrabCount = scuttle_count)
  
  # 4. Generate the pure linear probabilities
  probabilities <- predict(h2h_log_fit, simulated_game, type = "prob")
  
  # 5. Output the results
  cat("\n======================================================\n")
  cat("          LIVE DRAFT OBJECTIVE PROBABILITY ENGINE        \n")
  cat("======================================================\n\n")
  cat("BLUE TEAM: ", paste(blue_champions, collapse = ", "), "\n")
  cat("RED TEAM:  ", paste(red_champions, collapse = ", "), "\n\n")
  cat("------------------------------------------------------\n")
  cat("Prob. Blue Secures First Dragon: ", round(probabilities$.pred_1 * 100, 1), "%\n")
  cat("Prob. Red Secures First Dragon:  ", round(probabilities$.pred_0 * 100, 1), "%\n")
  cat("======================================================\n")
}
