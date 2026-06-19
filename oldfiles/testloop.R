#Connect to script which contains match history and puuid functions
source("functionstesting.R")
#Initialize matches and puuid vectors
if (!exists("match_pool"))     match_pool     <- character(0)
if (!exists("puuid_pool"))     puuid_pool     <- character(0)
if (!exists("visited_puuids")) visited_puuids <- character(0) 
if (!exists("visited_matches")) visited_matches <- character(0)
if (!dir.exists("data")) dir.create("data")


#Read in data from last pull 
file_list <- list.files(path = "data", pattern = "\\.rds$", full.names = TRUE)
#Loop through and assign each file to its own variable name
for (file in file_list) {
  # Extract the clean filename without the ".rds" extension
  obj_name <- tools::file_path_sans_ext(basename(file))
  
  # Read the file and assign it to that name in the global environment
  assign(obj_name, readRDS(file))
  rm(file, file_list)
}

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

###Next step is to create a loop which gets matches, gets PUUIDs, gets matches from those, gets PUUIDs from those, etc.

total_loops <- 10

for (loop in total_loops) {
  unvisited_matches <- setdiff(match_pool, visited_matches) 
  
  if(length(unvisited_matches) > 0) {
    message("Running PUUID scraping from ", length(unvisited_matches), " new matches")
    for (match_id in unvisited_matches) {
      message("Scraping PUUIDs from: ", match_id)
      players <- getpuuids(match_id)
      puuid_pool <- unique(c(puuid_pool, players))
      visited_matches <- unique(c(visited_matches, match_id)) #Mark visited matches as visited
      Sys.sleep(1)
    }
  }
  else {
    message("No new matches.")
  }
 #Now we run the loop which gets matches from players
  unvisited_puuids <- setdiff(puuid_pool, visited_puuids)
  if(length(unvisited_puuids) > 0) {
    player_batch <- head(unvisited_puuids, players_per_batch) 
    message("Harvesting matches from ", length(player_batch), " players.")
    
    for (puuid in unvisited_puuids) {
      message("Scraping history from: ", puuid) 
      
      matches <- gethistory(puuid)
      match_pool <- unique(c(match_pool, matches)) 
      visited_puuids <- unique(c(visited_puuids, puuid)) 
      Sys.sleep(1)
    }
  }
  # ----------------------------------------------------------------------------
  # PROGRESS REPORT (At the end of every generation)
  # ----------------------------------------------------------------------------
  message("\n📊 GEN ", loop, " SUMMARY:")
  message("Total Unique Matches in Pool: ", length(match_pool))
  message("Total Unique Players in Pool: ", length(puuid_pool))
  message("Matches Scanned for Players:  ", length(visited_matches))
  message("Players Scraped for Matches:  ", length(visited_puuids))
  saveRDS(match_pool,      "data/match_pool.rds")
  saveRDS(puuid_pool,      "data/puuid_pool.rds")
  saveRDS(visited_puuids,  "data/visited_puuids.rds")
  saveRDS(visited_matches, "data/scanned_matches.rds")
}

