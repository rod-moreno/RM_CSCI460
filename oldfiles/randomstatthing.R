extract_total_gold <- function(timeline, match_id) {
  
  frames <- timeline$info$frames
  
  # Get champion/position lookup for this match
  participants <- get_match_participants(match_id)
  
  gold_data <- map_dfr(frames, function(frame) {
    timestamp <- frame$timestamp
    
    map_dfr(frame$participantFrames, function(pf) {
      tibble(
        timestamp      = timestamp,
        participant_id = pf$participantId,
        total_gold     = pf$totalGold, 
        total_cs       = pf$minionsKilled, 
        total_damage   = pf$damageStats$totalDamageDoneToChampions
      )
    })
  }) %>%
    left_join(participants, by = "participant_id")  # join champion + position
  
  return(gold_data)
}

get_match_participants <- function(match_id) {
  url <- paste0("https://americas.api.riotgames.com/lol/match/v5/matches/", match_id)
  
  resp <- request(url) %>%
    req_headers("X-Riot-Token" = api_key) %>%
    req_perform() %>%
    resp_body_json()
  
  # Pull champion name and position for each participant
  map_dfr(resp$info$participants, function(p) {
    tibble(
      participant_id = p$participantId,
      champion_name  = p$championName,
      position       = p$teamPosition   # "TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"
    )
  })
}
all_gold <- data.frame()



done <- unique(all_gold$match_id)
remaining <- setdiff(match_pool, done)
next_batch <- head(remaining, 50) 
for (match in next_batch) {
  message("Extracting from match: ", match)
  timeline_raw <- get_match_timeline(match)
  gold_df <- extract_total_gold(timeline_raw, match_id = pool)  # pass match ID
  
  gold_df$match_id <- match
  
  all_gold <- bind_rows(all_gold, gold_df)
  Sys.sleep(1.5)
}


all_gold <- all_gold %>%
  mutate(time_minutes = round(timestamp / 60000)) %>%
  mutate(team_id = factor(ifelse(participant_id <= 5, "BLUE", "RED"), levels = c("BLUE", "RED")
  ))


avg_gold_by_role <- all_gold %>%
  group_by(time_minutes, position) %>%
  summarise(avg_gold = mean(total_gold, na.rm = TRUE), .groups = "drop")

single_match <- all_gold %>%
  filter(match_id == "NA1_5569684064")  # your match of interest

gold_comparison <- single_match %>%
  left_join(avg_gold_by_role, by = c("time_minutes", "position"))

avg_gold_by_role %>%
  ggplot() + 
  geom_line(aes(x = time_minutes, y = avg_gold, color = position)) + 
  theme_classic()
