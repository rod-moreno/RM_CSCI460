#Read in Riot API Key (from secret file! so no leaks)
readRenviron(".Renviron")
api_key <- Sys.getenv("RIOT_API_KEY")
source("functionstesting.R")

rm(training_dataset)
training_dataset <- tibble()


if (nrow(training_dataset) > 0) {
  unprocessed_matches <- setdiff(match_pool, unique(training_dataset$matchId))
} else {
  unprocessed_matches <- match_pool
}

matches_to_extract <- head(unprocessed_matches, 10)
for (m_id in head(matches_to_extract, 10)) {
  message("Processing Match: ", m_id)
  
  # Safe data pull
  raw_match <- get_match_details(m_id)
  
  # 1. Gather baseline match endpoints and targets
  match_features <- map_df(raw_match$info$participants, function(p) {
    extract_target_features(p, m_id) |> 
      # Force make sure participantId is explicitly part of your return tibble!
      mutate(participantId = as.integer(p$participantId)) 
  })
  
  # 2. Gather compressed timeline features (Exactly 10 rows)
  timeline_summary <- extract_timeline_summary(m_id)
  
  # 3. Join cleanly (10 rows + 10 rows = 10 rows)
  combined_features <- match_features |> 
    left_join(timeline_summary, by = c("matchId", "participantId"))
  
  # 4. Save to master
  training_dataset <- bind_rows(training_dataset, combined_features)
  
  Sys.sleep(0.5)
}
saveRDS(training_dataset, "data/training_dataset.rds")

# Inspect your beautiful training matrix!
view(training_dataset)


#Sanity check to make sure there are no duplicates
training_dataset %>%
  count(matchId, name = "player_count") %>%
  count(player_count, name = "number_of_matches")
#Hard-coding global ults
global_ult_champions <- c(
  "Karthus", "Soraka", "Gangplank", "Ezreal", "Jinx", "Ashe", "Senna", "Draven",
  "Shen", "Twisted Fate", "Pantheon", "Nocturne", "Galio", "Briar", "Ryze", "Taliyah", "Akshan", "Sion", 
  "Ornn", "Xerath", "Vex", "Ziggs"
)

processed_training_set <- training_dataset %>%
  # 1. Group by match to evaluate game-wide metrics
  group_by(matchId) %>%
  mutate(
    # Find the fastest non-zero dragon time in the entire match.
    # If no one took a dragon, this evaluates to Infinity quietly.
    match_fastest_drag = if_else(
      any(earliestDragonTakedown > 0), 
      min(earliestDragonTakedown[earliestDragonTakedown > 0], na.rm = TRUE), 
      Inf
    ),
    
    # 2. Assign the target label: Did this player's team secure it?
    # It checks if their individual team's kill matches the game's fastest kill.
    firstDragon = if_else(
      earliestDragonTakedown == match_fastest_drag & match_fastest_drag != Inf, 
      1, 
      0
    ),
    # 3. Checking for global ult
    has_global_ult = if_else(championName %in% global_ult_champions, 1, 0)
  ) %>%
  # 4. Clean up helper columns and ungroup
  select(-match_fastest_drag) %>%
  ungroup() 


saveRDS(processed_training_set, "data/gooddata.RDS")
