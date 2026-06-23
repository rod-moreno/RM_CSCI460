#Read in Riot API Key (from secret file! so no leaks)
source("functionstesting.R")
source("readdata.R")

rm(training_dataset)
rawmatchdata <- tibble()



#Create dataset, setup unextracted matches pool 
if (nrow(rawmatchdata) > 0) {
  unextracted_matches <- setdiff(match_pool, unique(rawmatchdata$matchId))
} else {
  unextracted_matches <- match_pool
}


#Set how many matches to extract in each batch
matches_to_extract <- head(unextracted_matches, 100)
#Pull data from matches
for (m_id in matches_to_extract) {
  message("Processing Match: ", m_id)
  
  # Safe data pull
  raw_match <- get_match_details(m_id)
  
  # 1. Gather baseline match endpoints and targets
  match_features <- map_df(raw_match$info$participants, function(p) {
    extract_target_features(p, m_id) %>% 
      # Force make sure participantId is explicitly part of your return tibble!
      mutate(participantId = as.integer(p$participantId)) 
  })
  
  # 2. Gather compressed timeline features (Exactly 10 rows)
  timeline_summary <- extract_timeline_summary(m_id)
  
  # 3. Join cleanly (10 rows + 10 rows = 10 rows)
  combined_features <- match_features %>% 
    left_join(timeline_summary, by = c("matchId", "participantId"))
  
  # 4. Save to master
  rawmatchdata <- bind_rows(rawmatchdata, combined_features)
  Sys.sleep(2.5)

}
unextracted_matches <- setdiff(match_pool, unique(rawmatchdata$matchId))
saveRDS(rawmatchdata, "data/data2/rawmatchdata.rds")
saveRDS(unextracted_matches, "data/data2/unextracted_matches.rds")


#Sanity check to make sure there are no duplicates
rawmatchdata %>%
  count(matchId, name = "player_count") %>%
  count(player_count, name = "number_of_matches")

processed_data <- rawmatchdata %>%
  # 1. Group by match to evaluate game-wide metrics
  group_by(matchId) %>%
  mutate(
    # FIX: Calculate the min safely by checking the vector contents first
    match_fastest_drag = if (any(earliestDragonTakedown > 0, na.rm = TRUE)) {
      min(earliestDragonTakedown[earliestDragonTakedown > 0], na.rm = TRUE)
    } else {
      NA_real_
    },
    
    # 2. Assign the target label: Did this player's team secure it?
    firstDragon = if_else(
      !is.na(match_fastest_drag) & earliestDragonTakedown == match_fastest_drag, 
      1, 
      0
    ),
    
    # 3. Checking for global ult
    has_global_ult = if_else(championName %in% global_ult_champions, 1, 0)
  ) %>% 
  
  # 4. Clean up helper columns and ungroup
  select(-match_fastest_drag) %>%
  ungroup()


saveRDS(processed_data, "data/data2/processed_data.rds")



match_pool <- match_pool[is_match_id]
