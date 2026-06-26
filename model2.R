source("functionstesting.R")
source("readData.R")

processed_data <- processed_data %>% 
  mutate(championName = if_else(championName == "MonkeyKing", "Wukong", championName))

# Drop matches with missing or malformed role assignments
valid_positions <- c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY")
invalid_match_ids <- processed_data %>%
  filter(teamPosition %in% valid_positions) %>%
  group_by(matchId, teamId) %>%
  summarise(unique_roles = n_distinct(teamPosition), .groups = "drop") %>%
  filter(unique_roles != 5) %>%
  pull(matchId) %>%
  unique()

processed_data <- processed_data %>%
  filter(!matchId %in% invalid_match_ids)


# ==============================================================
# STEP 1: Aggregate to team level
# ==============================================================

team_level_features <- processed_data %>% 
  group_by(matchId, teamId) %>% 
  summarise(
    firstDragon      = max(firstDragon),
    initialCrabCount = max(initialCrabCount, na.rm = TRUE),
    team_global_ults = sum(has_global_ult, na.rm = TRUE),
    
    # Raw Gold per lane (used only for efficiency calculation below)
    top_gold         = max(early_gold_per_min[teamPosition == "TOP"]),
    jungle_gold      = max(early_gold_per_min[teamPosition == "JUNGLE"]),
    mid_gold         = max(early_gold_per_min[teamPosition == "MIDDLE"]),
    adc_gold         = max(early_gold_per_min[teamPosition == "BOTTOM"]),    
    supp_gold        = max(early_gold_per_min[teamPosition == "UTILITY"]),    
    
    # Raw CS per lane (used only for efficiency calculation below)
    top_cs           = max(early_cs_per_min[teamPosition == "TOP"]),
    jungle_cs        = max(early_cs_per_min[teamPosition == "JUNGLE"]),
    mid_cs           = max(early_cs_per_min[teamPosition == "MIDDLE"]),
    adc_cs           = max(early_cs_per_min[teamPosition == "BOTTOM"]),      
    supp_cs          = max(early_cs_per_min[teamPosition == "UTILITY"]),      
    
    # Lane stomp challenge flag (binary: did this lane win the early trade?)
    top_stomp        = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "TOP"]),
    jungle_stomp     = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "JUNGLE"]),
    mid_stomp        = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "MIDDLE"]),
    adc_stomp        = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "BOTTOM"]),    
    supp_stomp       = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "UTILITY"]),   
    
    # Minutes 3-7 Jungle proximity per lane
    top_prox         = max(jungle_proximity_pct[teamPosition == "TOP"]),
    mid_prox         = max(jungle_proximity_pct[teamPosition == "MIDDLE"]),
    adc_prox         = max(jungle_proximity_pct[teamPosition == "BOTTOM"]),    
    supp_prox        = max(jungle_proximity_pct[teamPosition == "UTILITY"]),   
    
    # Minutes 3-7 Roaming pct per lane
    mid_roam         = max(roaming_pct[teamPosition == "MIDDLE"]),
    adc_roam         = max(roaming_pct[teamPosition == "BOTTOM"]),             
    supp_roam        = max(roaming_pct[teamPosition == "UTILITY"]),            
    
    .groups = "drop"
  ) %>%
  # Efficiency = gold income that didn't come from CS (kills, assists, plates)
  mutate(
    top_eff     = top_gold    - (top_cs    * 20),
    jungle_eff  = jungle_gold - (jungle_cs * 20),
    mid_eff     = mid_gold    - (mid_cs    * 20),
    adc_eff     = adc_gold    - (adc_cs    * 20),
    supp_eff    = supp_gold   - (supp_cs   * 20)
  )


# ==============================================================
# STEP 2: Split into blue/red and rename cleanly
# ==============================================================

blue_side <- team_level_features %>% 
  filter(teamId == 100) %>% 
  select(
    matchId, blue_drag = firstDragon, initialCrabCount, blue_globals = team_global_ults,
    blue_top_eff = top_eff, blue_jng_eff = jungle_eff, blue_mid_eff = mid_eff,
    blue_adc_eff = adc_eff, blue_supp_eff = supp_eff,
    blue_top_stomp = top_stomp, blue_jng_stomp = jungle_stomp, blue_mid_stomp = mid_stomp,
    blue_adc_stomp = adc_stomp, blue_supp_stomp = supp_stomp,
    blue_top_prox = top_prox, blue_mid_prox = mid_prox, blue_adc_prox = adc_prox, blue_supp_prox = supp_prox,
    blue_mid_roam = mid_roam, blue_adc_roam = adc_roam, blue_supp_roam = supp_roam
  )

red_side <- team_level_features %>% 
  filter(teamId == 200) %>% 
  select(
    matchId, red_globals = team_global_ults,
    red_top_eff = top_eff, red_jng_eff = jungle_eff, red_mid_eff = mid_eff,
    red_adc_eff = adc_eff, red_supp_eff = supp_eff,
    red_top_stomp = top_stomp, red_jng_stomp = jungle_stomp, red_mid_stomp = mid_stomp,
    red_adc_stomp = adc_stomp, red_supp_stomp = supp_stomp,
    red_top_prox = top_prox, red_mid_prox = mid_prox, red_adc_prox = adc_prox, red_supp_prox = supp_prox,
    red_mid_roam = mid_roam, red_adc_roam = adc_roam, red_supp_roam = supp_roam
  )


# ==============================================================
# STEP 3: Build match-level training set (gaps only, no raw gold)
# ==============================================================

match_level_training_set <- blue_side %>%
  inner_join(red_side, by = "matchId") %>%
  mutate(
    # Target variable
    who_won_drag = factor(blue_drag, levels = c(0, 1), labels = c("Lost_Drag", "Won_Drag")),
    
    # Structural/global features
    initialCrabCount = initialCrabCount,
    global_ult_gap   = blue_globals - red_globals,
    
    # Efficiency gaps: non-CS income (kills, assists, plates)
    top_eff_gap    = blue_top_eff  - red_top_eff,
    jng_eff_gap    = blue_jng_eff  - red_jng_eff,
    mid_eff_gap    = blue_mid_eff  - red_mid_eff,
    adc_eff_gap    = blue_adc_eff  - red_adc_eff,
    supp_eff_gap   = blue_supp_eff - red_supp_eff,
    
    # Lane stomp gaps (binary difference: -1, 0, or 1)
    top_stomp_gap  = blue_top_stomp  - red_top_stomp,
    jng_stomp_gap  = blue_jng_stomp  - red_jng_stomp,
    mid_stomp_gap  = blue_mid_stomp  - red_mid_stomp,
    adc_stomp_gap  = blue_adc_stomp  - red_adc_stomp,
    supp_stomp_gap = blue_supp_stomp - red_supp_stomp,
    
    # Jungle proximity gaps
    top_prox_gap   = blue_top_prox  - red_top_prox,
    mid_prox_gap   = blue_mid_prox  - red_mid_prox,
    adc_prox_gap   = blue_adc_prox  - red_adc_prox,
    supp_prox_gap  = blue_supp_prox - red_supp_prox,
    
    # Roaming gaps
    mid_roam_gap   = blue_mid_roam  - red_mid_roam,
    adc_roam_gap   = blue_adc_roam  - red_adc_roam,
    supp_roam_gap  = blue_supp_roam - red_supp_roam
  ) %>%
  select(
    matchId,
    who_won_drag,
    initialCrabCount,
    global_ult_gap,
    ends_with("_eff_gap"),
    ends_with("_stomp_gap"),
    ends_with("_prox_gap"),
    ends_with("_roam_gap")
  )

cat("--- MATCH LEVEL TRAINING SET BUILT ---\n")
cat("Rows:", nrow(match_level_training_set), "\n")
cat("Class balance:\n")
print(table(match_level_training_set$who_won_drag))


# ==============================================================
# STEP 4: Train/test split
# ==============================================================

set.seed(4)
split       <- initial_split(match_level_training_set, prop = 0.80, strata = who_won_drag)
training_set <- training(split)
testing_set  <- testing(split)

modelrecipe <- recipe(who_won_drag ~ ., data = training_set) %>%
  update_role(matchId, new_role = "id") %>%
  step_upsample(who_won_drag)


# ==============================================================
# STEP 5: Hyperparameter tuning
# ==============================================================

xgb_tune_spec <- boost_tree(
  trees      = 1000,
  tree_depth = tune(),
  learn_rate = tune(),
  min_n      = tune()
) %>%
  set_engine("xgboost", importance = "impurity") %>%
  set_mode("classification")

xgb_grid <- grid_regular(
  tree_depth(range = c(3, 6)),
  learn_rate(range = c(-3, -1)),
  min_n(range = c(5, 20)),
  levels = 3
)

xgb_wflow <- workflow() %>%
  add_recipe(modelrecipe) %>%
  add_model(xgb_tune_spec)

match_folds <- vfold_cv(training_set, v = 5, strata = who_won_drag)
cl <- makePSOCKcluster(parallel::detectCores() - 1)
clusterEvalQ(cl, {
  library(tidymodels)
  library(xgboost)
  library(themis)
})
registerDoParallel(cl)

start_time <- Sys.time()
tune_results <- tune_grid(
  xgb_wflow,
  resamples = match_folds,
  grid      = xgb_grid,
  metrics   = metric_set(accuracy, kap)
)
message("Tuning took: ", round(difftime(Sys.time(), start_time, units = "mins"), 1), " minutes")

show_best(tune_results, metric = "kap")
best_params     <- select_best(tune_results, metric = "kap")
final_xgb_wflow <- finalize_workflow(xgb_wflow, best_params)

model           <- fit(final_xgb_wflow, data = training_set)
final_xgb_model <- extract_fit_parsnip(model)


# ==============================================================
# STEP 6: Evaluate tuned model
# ==============================================================

performance_metrics <- predict(model, testing_set) %>%
  bind_cols(testing_set) %>%
  metrics(truth = who_won_drag, estimate = .pred_class) %>%
  filter(.metric %in% c("accuracy", "kap")) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)

cat("\n--- TUNED MODEL PERFORMANCE ---\n")
print(performance_metrics)

model %>%
  extract_fit_parsnip() %>%
  vip(num_features = 23, geom = "col", aesthetics = list(fill = "steelblue")) +
  theme_minimal() +
  labs(
    title    = "XGBoost Feature Importance (No Gold/CS Leads)",
    subtitle = "Strategic features only: efficiency, stomp, proximity, roaming",
    x        = "Features",
    y        = "Importance (Impurity)"
  )

saveRDS(final_xgb_model, "data/data2/final_xgb_model.rds")

# ==============================================================
# STEP 7: Smoothed model (regularised, less prone to overfit)
# ==============================================================

smooth_xgb_spec <- boost_tree(
  trees       = 500,
  learn_rate  = 0.01,
  tree_depth  = 3,
  min_n       = 20,
  mtry        = 4,
  sample_size = 0.75
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

smooth_wflow <- workflow() %>%
  add_recipe(modelrecipe) %>%
  add_model(smooth_xgb_spec)

smooth_fit <- fit(smooth_wflow, data = training_set)

smooth_metrics <- predict(smooth_fit, testing_set) %>%
  bind_cols(testing_set) %>%
  metrics(truth = who_won_drag, estimate = .pred_class) %>%
  filter(.metric %in% c("accuracy", "kap")) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)

cat("\n--- SMOOTHED MODEL PERFORMANCE ---\n")
print(smooth_metrics)

# Fix: assign before plotting
final_smooth_model <- extract_fit_parsnip(smooth_fit)

final_smooth_model %>%
  vip(num_features = 23, geom = "col", aesthetics = list(fill = "steelblue")) +
  theme_minimal() +
  labs(
    title    = "XGBoost Feature Importance (Smoothed, No Gold/CS Leads)",
    subtitle = "Strategic features only: efficiency, stomp, proximity, roaming",
    x        = "Features",
    y        = "Importance (Impurity)"
  )

saveRDS(final_smooth_model, "data/data2/final_smooth_model.rds")
# ==============================================================
# STEP 8: Champion profiles (n >= 75 threshold)
# ==============================================================

team_kill_totals <- processed_data %>%
  group_by(matchId, teamId) %>%
  summarise(team_kills = sum(kills, na.rm = TRUE), .groups = "drop")

processed_data_with_kp <- processed_data %>%
  left_join(team_kill_totals, by = c("matchId", "teamId")) %>%
  mutate(
    kill_participation = if_else(
      team_kills > 0,
      (kills + assists) / team_kills,
      NA_real_
    )
  )

champion_profiles <- processed_data_with_kp %>%
  group_by(championName, teamPosition) %>%
  summarise(
    base_gold_pm       = mean(early_gold_per_min, na.rm = TRUE),
    base_cs_pm         = mean(early_cs_per_min, na.rm = TRUE),
    base_efficiency    = mean(early_gold_per_min, na.rm = TRUE) - (mean(early_cs_per_min, na.rm = TRUE) * 20),
    base_stomp         = mean(earlyLaningPhaseGoldExpAdvantage, na.rm = TRUE),
    base_proximity     = mean(jungle_proximity_pct, na.rm = TRUE),
    base_roaming       = mean(roaming_pct, na.rm = TRUE),
    base_crab_count    = if (unique(teamPosition) == "JUNGLE") mean(initialCrabCount, na.rm = TRUE) else 0,
    games_played       = n(),
    win_rate           = mean(win, na.rm = TRUE),
    kill_participation = mean(kill_participation, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(games_played >= 30) %>%
  select(
    championName, teamPosition, base_gold_pm, base_efficiency,
    base_stomp, base_proximity, base_roaming, base_crab_count,
    games_played, win_rate, kill_participation
  )

saveRDS(champion_profiles, "data/data2/champion_profiles.rds")
cat("\n--- CHAMPION PROFILES BUILT ---\n")
cat("Champions with profiles:", nrow(champion_profiles), "\n")


# ==============================================================
# STEP 9: Save outputs
# ==============================================================

saveRDS(champion_profiles,  "data/data2/champion_profiles.rds")
saveRDS(model,              "data/data2/model.rds")
saveRDS(final_xgb_model,   "data/data2/final_xgb_model.rds")
saveRDS(final_smooth_model, "app/final_smooth_model.rds")


predict(model, testing_set) %>%
  bind_cols(testing_set) %>%
conf_mat(truth = who_won_drag, estimate = .pred_class,
         event_level = "second") %>%
  summary()

summary(smooth_fit)
