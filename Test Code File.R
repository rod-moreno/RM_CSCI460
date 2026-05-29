library(httr2)
library(jsonlite)
library(tidyverse)
library(tidymodels)

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
        firstTowerAssist = .x$firstTowerAssist, 
        kills = .x$kills,
        assists = .x$assists, 
        earliestDragonTakedown = .x$earliestDragonTakedown
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


### Testing dimensionality reduction
pca_prep <- global_player_stats %>%
  mutate(
    mins = gameDuration / 60,
    dmg_per_min = totalDamageDealtToChampions / mins,
    obj_per_min = damageDealtToObjectives / mins,
    cs_per_min  = (totalMinionsKilled + neutralMinionsKilled) / mins
  ) %>%
  # 2. Drop unique IDs/characters; PCA only accepts raw numeric matrices
  select(dmg_per_min, obj_per_min, cs_per_min)

# Execute PCA
stats_pca <- prcomp(pca_prep, center = TRUE, scale. = TRUE)

# View how much variance each new dimension captures
summary(stats_pca)

stats_pca$rotation

# Extract the actual coordinates for each row
pca_coordinates <- as_tibble(stats_pca$x)

# Bind them back to your baseline info
final_reduced_dataset <- global_player_stats %>%
  select(matchId, gameDuration, championName) %>%
  bind_cols(pca_coordinates)

# Look at your sleek new feature space!
head(final_reduced_dataset)


#Looking at damage
adjusted_metrics <- global_player_stats %>%
  mutate(
    # Avoid dividing by zero if someone had a 0-damage game
    damage_clean = if_else(totalDamageDealtToChampions == 0, 1, totalDamageDealtToChampions),
    
    # Kills + Assists secured per 10,000 units of damage dealt
    dmg_efficiency = ((kills + assists) / damage_clean) * 10000
  )

pca_advanced_prep <- adjusted_metrics %>%
  mutate(
    mins = gameDuration / 60,
    obj_per_min = damageDealtToObjectives / mins,
    cs_per_min  = (totalMinionsKilled + neutralMinionsKilled) / mins
  ) %>%
  select(dmg_efficiency,obj_per_min, cs_per_min)

# Run the updated PCA
advanced_pca <- prcomp(pca_advanced_prep, center = TRUE, scale. = TRUE)
advanced_pca$rotation


library(tidyverse)

# 1. Extract the coordinate scores as a clean dataframe
pca_scores <- as_tibble(advanced_pca$x) %>% 
  select(PC1, PC2) # Keep only the top 2 components since they explain your variance

# 2. Bind them back to your baseline identifiers
model_ready_df <- adjusted_metrics %>%
  select(matchId, championName, firstTowerKill) %>%
  mutate(firstTowerKill = as.factor(firstTowerKill)) %>%
  bind_cols(pca_scores)

head(model_ready_df)

model_data <- global_player_stats %>%
  select(matchId, firstTowerKill) %>% 
  bind_cols(as_tibble(advanced_pca$x) %>% select(PC1, PC2)) %>%
  mutate(firstTower = as.factor(firstTowerKill)) # Classification requires factors!


# 1. Define the model type and engine
lr_spec <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

# 2. Fit the model using your compressed PCA features
lr_fit <- lr_spec %>% 
  fit(firstTower ~ PC1 + PC2, data = model_data)
# 3. Look at the mathematical results!
tidy(lr_fit)

library(ranger)
# Example: Training a Random Forest using your PCA dimensions
rf_spec <- rand_forest(trees = 500) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

# The formula uses your PCA components to predict the binary target
rf_fit <- rf_spec %>% 
  fit(firstTowerKill ~ PC1 + PC2, data = model_ready_df)



model_results <- augment(rf_fit, new_data = model_ready_df)

head(model_results)

model_results %>% 
  conf_mat(truth = firstTowerKill, estimate = .pred_class)


library(vip)

# Re-run the specification with importance turned on
rf_importance_spec <- rand_forest(trees = 500) %>% 
  set_engine("ranger", importance = "permutation") %>% 
  set_mode("classification")

rf_importance_fit <- rf_importance_spec %>% 
  fit(firstTowerKill ~ PC1 + PC2, data = model_ready_df)

# Plot the importance graph
vip(rf_importance_fit) + 
  theme_minimal() +
  labs(title = "Random Forest Feature Importance")

set.seed(123) # Keeps your split reproducible
data_split <- initial_split(model_ready_df, prop = 0.75, strata = firstTowerKill)
train_data <- training(data_split)
test_data  <- testing(data_split)

# Train the model strictly on the training partition
rf_final_fit <- rf_spec %>% 
  fit(firstTowerKill ~ PC1 + PC2, data = train_data)

# Test its true predictive accuracy on the held-out test data!
augment(rf_final_fit, new_data = test_data) %>% 
  metrics(truth = firstTowerKill, estimate = .pred_class)


library(themis)

# 1. Define the preprocessing recipe
game_recipe <- recipe(firstTowerKill ~ PC1 + PC2, data = model_ready_df) %>%
  step_downsample(firstTowerKill, under_ratio = 1.0) # Forces a perfect 50/50 balance

# 2. Re-specify your Random Forest
rf_spec <- rand_forest(trees = 500) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

# 3. Bundle them together into a tidymodels workflow
balanced_workflow <- workflow() %>%
  add_recipe(game_recipe) %>%
  add_model(rf_spec)

# 4. Fit the balanced workflow
balanced_fit <- fit(balanced_workflow, data = model_ready_df)


balanced_results <- augment(balanced_fit, new_data = model_ready_df)

# Check your fresh Confusion Matrix
balanced_results %>% 
  conf_mat(truth = firstTowerKill, estimate = .pred_class)

# Check your real Kappa score!
balanced_results %>% 
  metrics(truth = firstTowerKill, estimate = .pred_class)


# 1. Generate class probabilities instead of just hard predictions
balanced_probs <- augment(balanced_fit, new_data = model_ready_df)

# 2. Calculate the exact Area Under the Curve (AUC)
balanced_probs %>% 
  roc_auc(truth = firstTowerKill, .pred_FALSE) # Replace .pred_1 with your positive class column name if it differs

# 3. Plot the clean ROC Curve graph
balanced_probs %>% 
  roc_curve(truth = firstTowerKill, .pred_FALSE) %>% 
  autoplot() +
  theme_minimal() +
  labs(
    title = "Random Forest ROC Curve",
    subtitle = "Predicting Early Objective Control via Playstyle PCA"
  )



data <- readRDS("global_crawled_baselines.rds")

head(data$matchId)
