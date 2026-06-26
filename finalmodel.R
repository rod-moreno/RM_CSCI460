source("functionstesting.R")
source("readData.R")


team_level_features <- processed_data %>% 
  group_by(matchId, teamId) %>% 
  summarise(
    firstDragon      = max(firstDragon),
    initialCrabCount = max(initialCrabCount, na.rm = TRUE),
    team_global_ults = sum(has_global_ult, na.rm = TRUE),
    
    # Raw Gold per lane
    top_gold         = max(early_gold_per_min[teamPosition == "TOP"]),
    jungle_gold      = max(early_gold_per_min[teamPosition == "JUNGLE"]),
    mid_gold         = max(early_gold_per_min[teamPosition == "MIDDLE"]),
    adc_gold         = max(early_gold_per_min[teamPosition == "BOTTOM"]),    
    supp_gold        = max(early_gold_per_min[teamPosition == "UTILITY"]),    
    
    # Raw CS per lane
    top_cs           = max(early_cs_per_min[teamPosition == "TOP"]),
    jungle_cs        = max(early_cs_per_min[teamPosition == "JUNGLE"]),
    mid_cs           = max(early_cs_per_min[teamPosition == "MIDDLE"]),
    adc_cs           = max(early_cs_per_min[teamPosition == "BOTTOM"]),      
    supp_cs          = max(early_cs_per_min[teamPosition == "UTILITY"]),      
    
    # Lane stomp challenge flag
    top_stomp        = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "TOP"]),
    jungle_stomp     = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "JUNGLE"]),
    mid_stomp        = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "MIDDLE"]),
    adc_stomp        = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "BOTTOM"]),    
    supp_stomp       = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "UTILITY"]),   
    # 5. Minutes 3-7 Jungle Attention
    top_prox         = max(jungle_proximity_pct[teamPosition == "TOP"]),
    mid_prox         = max(jungle_proximity_pct[teamPosition == "MIDDLE"]),
    adc_prox         = max(jungle_proximity_pct[teamPosition == "BOTTOM"]),    
    supp_prox        = max(jungle_proximity_pct[teamPosition == "UTILITY"]),   
    
    # 6. Minutes 3-7 Roaming Pct
    mid_roam         = max(roaming_pct[teamPosition == "MIDDLE"]),
    adc_roam         = max(roaming_pct[teamPosition == "BOTTOM"]),             
    supp_roam        = max(roaming_pct[teamPosition == "UTILITY"]),            
    
    .groups = "drop"
  ) %>%
  mutate (
    top_eff          = top_gold - (top_cs * 20),
    jungle_eff       = jungle_gold - (jungle_cs * 20),
    mid_eff          = mid_gold - (mid_cs * 20),
    adc_eff          = adc_gold - (adc_cs * 20),                              
    supp_eff         = supp_gold - (supp_cs * 20), 

  )

blue_side <- team_level_features %>% 
  filter(teamId == 100) %>% 
  select(
    matchId, teamId, blue_drag = firstDragon, initialCrabCount, blue_globals = team_global_ults,
    blue_top_g = top_gold, blue_jng_g = jungle_gold, blue_mid_g = mid_gold, blue_adc_g = adc_gold, blue_supp_gold = supp_gold,
    blue_top_cs = top_cs, blue_jng_cs = jungle_cs, blue_mid_cs = mid_cs, blue_adc_cs = adc_cs, blue_supp_cs = supp_cs,
    blue_top_eff = top_eff, blue_jng_eff = jungle_eff, blue_mid_eff = mid_eff, blue_adc_eff = adc_eff, blue_supp_eff = supp_eff,
    blue_top_stomp = top_stomp, blue_jng_stomp = jungle_stomp, blue_mid_stomp = mid_stomp, blue_adc_stomp = adc_stomp, blue_supp_stomp = supp_stomp,
    blue_top_prox = top_prox, blue_mid_prox = mid_prox, blue_adc_prox = adc_prox, blue_supp_prox = supp_prox,
    blue_mid_roam = mid_roam, blue_adc_roam = adc_roam, blue_supp_roam = supp_roam
  )

red_side <- team_level_features %>% 
  filter(teamId == 200) %>% 
  select(
    matchId, teamId, red_drag = firstDragon, initialCrabCount, red_globals = team_global_ults,
    red_top_g = top_gold, red_jng_g = jungle_gold, red_mid_g = mid_gold, red_adc_g = adc_gold, red_supp_gold = supp_gold,
    red_top_cs = top_cs, red_jng_cs = jungle_cs, red_mid_cs = mid_cs, red_adc_cs = adc_cs, red_supp_cs = supp_cs,
    red_top_eff = top_eff, red_jng_eff = jungle_eff, red_mid_eff = mid_eff, red_adc_eff = adc_eff, red_supp_eff = supp_eff,
    red_top_stomp = top_stomp, red_jng_stomp = jungle_stomp, red_mid_stomp = mid_stomp, red_adc_stomp = adc_stomp, red_supp_stomp = supp_stomp,
    red_top_prox = top_prox, red_mid_prox = mid_prox, red_adc_prox = adc_prox, red_supp_prox = supp_prox,
    red_mid_roam = mid_roam, red_adc_roam = adc_roam, red_supp_roam = supp_roam
  )


match_level_training_set <- blue_side %>%
  # inner_join ensures we only keep matches where we have data for both sides
  inner_join(
    red_side %>% filter(teamId == 200), # Ensure this is your Red team ID!
    by = "matchId", 
    suffix = c("_blue", "_red")
  ) %>%
  
  # 2. Compute the exact positional gaps for your XGBoost model
  mutate(
    # TARGET VARIABLE (Factor for classification)
    blue_drag_secured = as.factor(blue_drag),
    
    # Global Mechanics
    initialCrabCount = initialCrabCount_blue, # Or use crab_differential if tracking team takes
    global_ult_gap   = blue_globals - red_globals,
    
    # Raw Gold Leads (Blue - Red)
    top_gold_lead    = blue_top_g - red_top_g,
    jungle_gold_lead = blue_jng_g - red_jng_g,
    mid_gold_lead    = blue_mid_g - red_mid_g,
    adc_gold_lead    = blue_adc_g - red_adc_g,
    supp_gold_lead   = blue_supp_gold - red_supp_gold,
    
    # Efficiency Leads (Blue_Eff - Red_Eff)
    top_gold_eff_lead    = blue_top_eff - red_top_eff,
    jungle_gold_eff_lead = blue_jng_eff - red_jng_eff,
    mid_gold_eff_lead    = blue_mid_eff - red_mid_eff,
    adc_gold_eff_lead    = blue_adc_eff - red_adc_eff,
    supp_gold_eff_lead   = blue_supp_eff - red_supp_eff,
    
    # Lane Stomp Gaps
    top_stomp_gap    = blue_top_stomp - red_top_stomp,
    jungle_stomp_gap = blue_jng_stomp - red_jng_stomp,
    mid_stomp_gap    = blue_mid_stomp - red_mid_stomp,
    adc_stomp_gap    = blue_adc_stomp - red_adc_stomp,
    
    # Jungle Hover Attention Gaps
    top_prox_gap  = blue_top_prox - red_top_prox,
    mid_prox_gap  = blue_mid_prox - red_mid_prox,
    adc_prox_gap  = blue_adc_prox - red_adc_prox,
    supp_prox_gap = blue_supp_prox - red_supp_prox,
    
    # Pure Roaming Gaps
    mid_roam_gap  = blue_mid_roam - red_mid_roam,
    adc_roam_gap  = blue_adc_roam - red_adc_roam,
    supp_roam_gap = blue_supp_roam - red_supp_roam
  ) %>%
  
  # 3. Strip away the raw team metrics to prevent data leakage/cheating
  select(
    matchId, 
    blue_drag_secured, 
    initialCrabCount,
    global_ult_gap,
    ends_with("_lead"), 
    ends_with("_gap")
  ) %>%
  mutate(
    who_won_drag = factor(blue_drag_secured, levels = c(0, 1), labels = c("Lost_Drag", "Won_Drag"))
  ) %>%
  select(-blue_drag_secured, -supp_gold_eff_lead)

#Partition data into training and testing
set.seed(4)
split <- initial_split(match_level_training_set, prop = 0.80, strata = who_won_drag)
training_set <- training(split)
testing_set <- testing(split)

modelrecipe <- recipe(who_won_drag ~ ., data = training_set) %>%
update_role(matchId, new_role = 'id') 

xgb_tune_spec <- boost_tree(
  trees = 500,
  tree_depth = tune(), 
  learn_rate = tune(), 
  min_n = tune()
) %>%
  set_engine("xgboost", importance = "impurity") %>%
  set_mode("classification")

xgb_grid <- grid_regular(
  tree_depth(range = c(3, 6)),
  learn_rate(range = c(-2, -1)), # Explores 0.01 to 0.1 on a log scale
  min_n(range = c(5, 20)),
  levels = 3                     # Tests 3 variations of each (27 combinations total)
)

# Put it in a workflow
xgb_wflow <- workflow() %>%
  add_recipe(modelrecipe) %>% # Replace with your actual recipe name
  add_model(xgb_tune_spec)

match_folds <- vfold_cv(training_set, v = 5, strata = who_won_drag)
# Run the grid search over your cross-validation folds
tune_results <- tune_grid(
  xgb_wflow,
  resamples = match_folds,        # Replace with your actual rsample object
  grid = xgb_grid,
  metrics = metric_set(accuracy, kap)
)

show_best(tune_results, metric = "accuracy")

best_params <- select_best(tune_results, metric = "accuracy")

final_xgb_wflow <- finalize_workflow(xgb_wflow, best_params)

model <- fit(final_xgb_wflow, data = training_set)

final_xgb_model <- extract_fit_parsnip(model)



performance_metrics <- predict(model, testing_set) %>%
  bind_cols(testing_set) %>% 
  metrics(truth = who_won_drag, estimate = .pred_class) %>%
  filter(.metric %in% c("accuracy", "kap")) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)


print(performance_metrics)


model %>%
  extract_fit_parsnip() %>%
  vip(num_features = 23, geom = "col", aesthetics = list(fill = "steelblue")) +
  theme_minimal() +
  labs(
    title = "XGBoost Feature Importance",
    subtitle = "Which early metrics best predict if the economic leader wins First Dragon?",
    x = "Features",
    y = "Importance (Impurity)"
  )
team_kill_totals <- processed_data %>%
  group_by(matchId, teamId) %>%
  summarise(team_kills = sum(kills, na.rm = TRUE), .groups = "drop")

# Step 2: join team kill totals back to player level and compute a
# per-game kill_participation for each player row. Guard against the
# rare case where team_kills == 0 (surrender/remake) to avoid Inf/NaN.
processed_data_with_kp <- processed_data %>%
  left_join(team_kill_totals, by = c("matchId", "teamId")) %>%
  mutate(
    kill_participation = if_else(
      team_kills > 0,
      (kills + assists) / team_kills,
      NA_real_   # exclude 0-kill games from the KP average rather than pulling it toward 0
    )
  )


champion_profiles <- processed_data_with_kp %>%
  group_by(championName, teamPosition) %>%
  summarise(
    # 1 & 2. Gold and Gold Efficiency Baselines
    base_gold_pm    = mean(early_gold_per_min, na.rm = TRUE),
    base_cs_pm      = mean(early_cs_per_min, na.rm = TRUE),
    base_efficiency = mean(early_gold_per_min, na.rm = TRUE) - (mean(early_cs_per_min) * 20),
    
    # 3. Lane Stomp Baseline
    base_stomp      = mean(earlyLaningPhaseGoldExpAdvantage, na.rm = TRUE),
    
    # 4. Proximity / Jungle Attention Baseline
    base_proximity  = mean(jungle_proximity_pct, na.rm = TRUE),
    
    # 5. Roaming Baseline
    base_roaming    = mean(roaming_pct, na.rm = TRUE),
    
    # NEW FEATURE: Baseline Scuttle Crab control (Only calculated for Junglers)
    # Laners are assigned a 0 baseline since they aren't the primary killers of the initial crabs
    base_crab_count = if (unique(teamPosition) == "JUNGLE") mean(initialCrabCount, na.rm = TRUE) else 0,
    
    games_played    = n(),
    win_rate        = mean(win, na.rm = TRUE),
    kill_participation = mean(kill_participation, na.rm = TRUE), 
    
    .groups = "drop"
  ) %>%
  filter(games_played >= 75) %>%
  select(
    championName, teamPosition, base_gold_pm, base_efficiency, 
    base_stomp, base_proximity, base_roaming, base_crab_count, games_played, win_rate, kill_participation
  )

saveRDS(champion_profiles, "data/data2/champion_profiles.rds")
saveRDS(model, "data/data2/model.rds")
saveRDS(final_xgb_model, "data/data2/final_xgb_model.rds")



smooth_xgb_spec <- boost_tree(
  trees = 500,                         # Keep your high tree count
  learn_rate = 0.01,                   # Keep your conservative step size
  tree_depth = 3,                      # LOWER DEPTH (Drop from 6 to 3 or 4)
  min_n = 20,                          # Keep your minimum node size
  mtry = 4,                            # Limit features per split (e.g., ~60% of your total columns)
  sample_size = 0.75                   # Subsample rows to add structural noise
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")


# Put it in a workflow
smooth_wflow <- workflow() %>%
  add_recipe(modelrecipe) %>% # Replace with your actual recipe name
  add_model(smooth_xgb_spec)

smooth_fit <- fit(smooth_wflow, data = training_set)

smooth_metrics <- predict(smooth_fit, testing_set) %>%
  bind_cols(testing_set) %>% 
  metrics(truth = who_won_drag, estimate = .pred_class) %>%
  filter(.metric %in% c("accuracy", "kap")) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)

print(smooth_metrics)

final_smooth_model %>%
  vip(num_features = 23, geom = "col", aesthetics = list(fill = "steelblue")) +
  theme_minimal() +
  labs(
    title = "XGBoost Feature Importance",
    subtitle = "Which early metrics best predict if the economic leader wins First Dragon?",
    x = "Features",
    y = "Importance (Impurity)"
  )

final_smooth_model <- extract_fit_parsnip(smooth_fit)

saveRDS(final_smooth_model, "app/final_smooth_model.rds")



test_predictions <- predict(final_smooth_model, testing_set, type = "prob")

results_df <- testing_set %>%
  bind_cols(test_predictions)

ggplot(results_df, aes(x = .pred_Won_Drag)) +
  geom_histogram(binwidth = 0.01, fill = "#375a7f", color = "white", alpha = 0.9) +
  geom_vline(xintercept = 0.5, color = "#e74c3c", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(
    title = "Distribution of Model Confidence (First Dragon)",
    subtitle = "Checking for the 'Marginal Win' clustering effect",
    x = "Predicted Probability (Blue Secures Dragon)",
    y = "Number of Simulated Matches"
  ) +
  theme_minimal()
