#Connect to script which contains match history and puuid functions
source("functionstesting.R")

#Initialize matches and puuid vectors
if (!exists("match_pool"))     match_pool     <- character(0)
if (!exists("puuid_pool"))     puuid_pool     <- character(0)
if (!exists("visited_puuids")) visited_puuids <- character(0) 
if (!exists("visited_matches")) visited_matches <- character(0)

#Manual input of high elo player as seed
puuid1 <- seedplayer("dusklol", "000")


#Obtain seed matches from seed player to pull new profiles from
if (length(match_pool) == 0) {
  message("Creating seed player history. . .")
  match_pool <- gethistory(puuid1)
}


### Obtain first batch of PUUIDs from seed matches
message("Obtaining PUUIDs. . . ") 
for (match_id in match_pool) {
  message("Looking inside match: ", match_id)
  
  players <- getpuuids(match_id)
  
  puuid_pool <- unique(c(puuid_pool, players)) 
  visited_matches <- unique(c(visited_matches, match_id))
  Sys.sleep(1.2)
}


# ======================================================
# Obtain matches from PUUIDs
# ======================================================
#Batch size for how many players to run match history on each time
batch_size <- 20


unvisited_puuids <- setdiff(puuid_pool, visited_puuids) 
currentbatch <- head(unvisited_puuids, batch_size)
for(puuid in currentbatch) {
  message("Harvesting matches from: ", puuid) 
  
  matches <- gethistory(puuid) 
  
  match_pool <- unique(c(match_pool, matches)) 
  
  visited_puuids <- unique(c(visited_puuids, puuid)) #Mark them as visited so next time I loop it doesn't catch these players
  Sys.sleep(1.8)
}

for (puuid in puuid_pool) {
  matches <- tryCatch({
    gethistory(puuid)
  }, error = function(e) {
    message("Skipping ", puuid, " — ", e$message)
    return(character(0))
  })
  
  match_pool <- unique(c(match_pool, matches))
  Sys.sleep(1.2)
}

# ======================================================
# Obtain more PUUIDs from matches
# ======================================================
#Batch size for how many matches to pull player IDs from
batch_size <- 15


unvisited_matches <- setdiff(match_pool, scanned_matches)
currentmatchbatch <- head(unvisited_matches, batch_size)
for(match_id in currentmatchbatch) {
  message("Harvesting matches from: ", match_id) 
  
  players <- getpuuids(match_id) 
  
  match_pool <- unique(c(match_pool, players)) 
  
  scanned_matches <- unique(c(scanned_matches, match_id)) #Mark them as visited so next time I loop it doesn't catch these players
  
  Sys.sleep(0.5)
}

saveRDS(match_pool,      "data/match_pool.rds")
saveRDS(puuid_pool,      "data/puuid_pool.rds")
saveRDS(visited_puuids,  "data/visited_puuids.rds")
saveRDS(scanned_matches, "data/scanned_matches.rds")
message("Current Database:")
message("Total Players Found: ", length(puuid_pool))
message("Total Matches Found: ", length(match_pool))

match_pool <- readRDS("data/match_pool.rds")

match_pool <- unique(match_pool)
