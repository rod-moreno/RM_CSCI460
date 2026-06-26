source("functionstesting.R")

newmatchestopullpuuids <- tail(match_pool, 200)
newpuuidstopullmatches <- character(0)
for (i in seq_along(newmatchestopullpuuids)) {
  match_id <- newmatchestopullpuuids[i]
  message("[", i, "/", length(newmatchestopullpuuids), "] Pulling PUUIDs from: ", match_id)
  
  players <- getpuuids(match_id)
  
  # Keep only PUUIDs not already in the existing pool
  new_players <- setdiff(players, puuid_pool)
  
  if (length(new_players) > 0) {
    newpuuidstopullmatches <- unique(c(newpuuidstopullmatches, new_players))
    message("  --> Found ", length(new_players), " new PUUIDs (", 
            10 - length(new_players), " already known)")
  } else {
    message("  --> All 10 players already in pool, skipping")
  }
  
  Sys.sleep(1.2)
}

newmatchpool <- character(0)

puuidstopull <- tail(newpuuidstopullmatches, 500)

for (i in seq_along(puuidstopull)) {
  puuid <- puuidstopull[i]
  message("[", i, "/", length(puuidstopull), "] Pulling matches from: ", puuid)
  
  matches <- gethistory(puuid)
  
  # Keep only matches not already in the existing pool
  new_matches <- setdiff(matches, match_pool)
  
  if (length(new_matches) > 0) {
    newmatchpool <- unique(c(newmatchpool, new_matches))
    message("  --> Found ", length(new_matches), " new matches (", 
            length(matches) - length(new_matches), " already known)")
  } else {
    message("  --> All matches already in pool, skipping")
  }
  
  Sys.sleep(1.2)
}



new_match_results <- vector("list", length(newmatchpool))
names(new_match_results) <- newmatchpool
for (i in seq_along(newmatchpool)) {
  m_id <- newmatchpool[i]
  message("[", i, "/", length(newmatchpool), "] Processing: ", m_id)
  
  raw_match <- get_match_details(m_id)
  
  match_features <- map_df(raw_match$info$participants, function(p) {
    extract_target_features(p, m_id) %>%
      mutate(participantId = as.integer(p$participantId))
  })
  
  timeline_summary <- extract_timeline_summary(m_id)
  
  new_match_results[[m_id]] <- match_features %>%
    left_join(timeline_summary, by = c("matchId", "participantId"))
  
  Sys.sleep(2.5)
}

new_match_data <- bind_rows(new_match_results)

length(intersect(unique(rawmatchdata$matchId), unique(new_match_results$matchId)))
new_match_data %>%
  count(matchId, name = "player_count") %>%
  count(player_count, name = "number_of_matches")

ncol(new_match_data) == ncol(rawmatchdata)
names(new_match_data) == names(rawmatchdata)

rawmatchdata <- bind_rows(rawmatchdata, new_match_data)
message("Total rows: ", nrow(rawmatchdata))

saveRDS(rawmatchdata, "data/data2/rawmatchdata.rds")

View(champion_profiles %>%
  filter(teamPosition == "BOTTOM"))
