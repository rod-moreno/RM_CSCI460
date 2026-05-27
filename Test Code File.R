library(httr2)
library(jsonlite)
library(tidyverse)

api_key <- "RGAPI-7e740907-b2dd-4053-83d4-91dd5320706a"

### Scraping high ELO games

#Sanity check to see if api_key works

test_url <- "https://na1.api.riotgames.com/lol/status/v4/platform-data"
resp <- request(test_url) |>
  req_headers("X-Riot-Token" = api_key) |>
  req_error(is_error = function(resp) FALSE) |> # Prevents R from crashing if it fails
  req_perform()
print(resp$status_code)

#Take #1 Player on leaderboard (5/27/26)
game_name <- "dusklol"
tag_line <- "000"

url <- paste0("https://americas.api.riotgames.com/riot/account/v1/accounts/by-riot-id/", 
              game_name, "/", tag_line)
request <- request(url) |>
  req_headers("X-Riot-Token" = api_key)

response <- req_perform(request)
user_data <- resp_body_json(response)

my_puuid <- user_data$puuid
print(paste("Your PUUID is:", my_puuid))

match_ids_url <- paste0("https://americas.api.riotgames.com/lol/match/v5/matches/by-puuid/", 
                        my_puuid, "/ids?start=0&count=20")

match_ids_request <- request(match_ids_url) |>
  req_headers("X-Riot-Token" = api_key)

match_ids <- req_perform(match_ids_request) |> resp_body_json()

seed_match_id <- "NA1_5569320365"

global_stats_list <- list()
match_counter <- 1
m_url <- paste0("https://americas.api.riotgames.com/lol/match/v5/matches/", seed_match_id)

m_info <- request(m_url) |>
  req_headers("X-Riot-Token" = api_key) |>
  req_perform() |>
  resp_body_json()


# Grab the 10 PUUIDs from this game to be our "seed players"
high_elo_puuids <- m_info$metadata$participants

processed_matches <- c(seed_match_id)

for (p_puuid in high_elo_puuids) {
  
  message(paste("Scraping matches for player PUUID:", substr(p_puuid, 1, 10), "..."))
  
  match_ids_url <- paste0("https://americas.api.riotgames.com/lol/match/v5/matches/by-puuid/", 
                          p_puuid, "/ids?queue=420&start=0&count=5")
  
  try({
    recent_matches <- request(match_ids_url) |>
      req_headers("X-Riot-Token" = api_key) |>
      req_perform() |>
      resp_body_json() |>
      unlist()
    
    for (m_id in recent_matches) {
      if (m_id %in% processed_matches) next
      processed_matches <- c(processed_matches, m_id)
      
      message(paste("Processing game:", m_id))
      
      # Pull individual match details (Fully allowed on Dev Keys)
      game_url <- paste0("https://americas.api.riotgames.com/lol/match/v5/matches/", m_id)
      game_info <- request(game_url) |> req_headers("X-Riot-Token" = api_key) |> req_perform() |> resp_body_json()
      
      game_duration <- game_info$info$gameDuration
      
      match_data <- map_df(game_info$info$participants, ~ tibble(
        matchId                         = m_id,
        gameDuration                    = game_duration,
        championName                    = .x$championName,
        totalDamageDealtToChampions     = .x$totalDamageDealtToChampions,
        damageDealtToObjectives         = .x$damageDealtToObjectives,
        totalMinionsKilled              = .x$totalMinionsKilled,
        neutralMinionsKilled            = .x$neutralMinionsKilled,
        totalTimeCCDealt                = .x$totalTimeCCDealt,
        firstBloodKill  =  .x$firstBloodKill,
        firstBloodAssist = .x$firstBloodAssist, 
        firstTowerKill = .x$firstTowerKill,
        firstTowerAssist = .x$firstTowerAssist
      ))
      
      global_stats_list[[match_counter]] <- match_data
      match_counter <- match_counter + 1
      Sys.sleep(1.2)
    }
  })
  Sys.sleep(1.2)
}

global_player_stats <- bind_rows(global_stats_list)
saveRDS(global_player_stats, "global_crawled_baselines.rds")
