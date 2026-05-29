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
message("Riot API Connection Status: ", resp$status_code)

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
