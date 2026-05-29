library(httr2)
library(jsonlite)
library(tidyverse)
library(tidymodels)


#Read in Riot API Key (from secret file! so no leaks)
readRenviron(".Renviron")
api_key <- Sys.getenv("RIOT_API_KEY")

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

#Obtain player ID from in-game name and tagline, used for finding match history 
puuid_url <- paste0("https://americas.api.riotgames.com/riot/account/v1/accounts/by-riot-id/", 
              game_name, "/", tag_line)
request <- request(puuid_url) |>
  req_headers("X-Riot-Token" = api_key)
response <- req_perform(request)
user_data <- resp_body_json(response)

puuid1 <- user_data$puuid
print(paste("PUUID is:", my_puuid))

### Get recent ranked queue matches from that player
match_ids_url <- paste0("https://americas.api.riotgames.com/lol/match/v5/matches/by-puuid/", 
                        my_puuid, "/ids?queue=420&start=0&count=5")

#saving API response into a list of 5 most recent matches
match_ids_request <- request(match_ids_url) |>
  req_headers("X-Riot-Token" = api_key)
match_ids <- req_perform(match_ids_request) |> resp_body_json()


  m_url <- paste0("https://americas.api.riotgames.com/lol/match/v5/matches/", match_ids[[1]])
  match_info <- request(m_url) |>
    req_headers("X-Riot-Token" = api_key) |>
    req_perform() |>
    resp_body_json()

puuids_list <- match_info$metadata$participants
#Now we have a list of 10 players that we can scrape matches from
#Next step, automating this scraper to pull PUUIDs from matches, maintain no duplicates
#and then pull matches from those PUUIDs, removing duplicates as necessary
#goal is probably anywhere from 2500-5000 match IDs to obtain 25000-50000 rows of data 