source("functionstesting.R")

team_level_features <- processed_training_set %>% 
  # Explicitly select only the columns you need for aggregation first
  select(matchId, teamId, firstDragon, early_cs_per_min, early_gold_per_min, earlyLaningPhaseGoldExpAdvantage, 
         jungle_proximity_pct, roaming_pct, has_global_ult, initialCrabCount) %>% 
  group_by(matchId, teamId) %>% 
  summarise(
    firstDragon          = max(firstDragon),
    team_cs_per_min      = sum(early_cs_per_min, na.rm = TRUE),
    team_gold_per_min    = sum(early_gold_per_min, na.rm = TRUE),
    avg_jungle_proximity = mean(jungle_proximity_pct, na.rm = TRUE),
    avg_roaming_pct      = mean(roaming_pct, na.rm = TRUE),
    team_global_ults     = sum(has_global_ult, na.rm = TRUE),
    # If initialCrabCount varies by player, use mean or max instead of unique
    initialCrabCount     = max(initialCrabCount, na.rm = TRUE), 
    .groups = "drop"
  )

# Pivot to Wide Head-to-Head Format (Blue side vs. Red side)
blue_side <- team_level_features %>% 
  filter(teamId == 100) %>% 
  select(matchId, blue_cs = team_cs_per_min, blue_gold = team_gold_per_min, 
         blue_prox = avg_jungle_proximity, blue_roam = avg_roaming_pct,
         blue_globals = team_global_ults, blue_drag = firstDragon, initialCrabCount, 
         blue_efficiency = team_gold_efficiency)

red_side <- team_level_features %>% 
  filter(teamId == 200) %>% 
  select(matchId, red_cs = team_cs_per_min, red_gold = team_gold_per_min, 
         red_prox = avg_jungle_proximity, red_roam = avg_roaming_pct,
         red_globals = team_global_ults, 
         red_efficiency = team_gold_efficiency)

match_level_training_set <- blue_side %>% 
  inner_join(red_side, by = "matchId") %>% 
  mutate(blue_won_dragon = as.factor(if_else(blue_drag == 1, 1, 0))) %>% 
  select(-matchId, -blue_drag)

champion_profiles <- processed_training_set %>% 
  group_by(championName) %>% 
  summarise(
    base_cs_per_min   = mean(early_cs_per_min, na.rm = TRUE),
    base_gold_per_min = mean(early_gold_per_min, na.rm = TRUE),
    base_proximity    = mean(jungle_proximity_pct, na.rm = TRUE),
    base_roaming      = mean(roaming_pct, na.rm = TRUE),
    
    # Keeps the static 1 or 0 designation per champion profile
    has_global_ult    = unique(has_global_ult), 
    .groups = "drop"
  )

set.seed(123)
match_split <- initial_split(match_level_training_set, prop = 0.80, strata = blue_won_dragon)
m_train     <- training(match_split)
m_test      <- testing(match_split)

match_recipe <- recipe(blue_won_dragon ~ ., data = m_train) %>% 
  step_normalize(all_numeric_predictors())
log_reg_spec <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

rf_spec <- rand_forest(trees = 500) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")


rf_workflow <- workflow() %>% 
  add_recipe(match_recipe) %>% 
  add_model(rf_spec)
h2h_rf_fit <- fit(rf_workflow, data = m_train)

log_workflow <- workflow() %>% 
  add_recipe(match_recipe) %>% 
  add_model(log_reg_spec)

h2h_log_fit <- fit(log_workflow, data = m_train)

# Generate Random Forest Test Predictions
rf_preds <- predict(h2h_rf_fit, m_test) %>% 
  bind_cols(m_test) %>% 
  metrics(truth = blue_won_dragon, estimate = .pred_class) %>% 
  mutate(model = "Random Forest")

# Generate Logistic Regression Test Predictions
log_preds <- predict(h2h_log_fit, m_test) %>% 
  bind_cols(m_test) %>% 
  metrics(truth = blue_won_dragon, estimate = .pred_class) %>% 
  mutate(model = "Logistic Regression")

# Bind metrics together for a side-by-side comparison matrix
performance_comparison <- bind_rows(rf_preds, log_preds) %>% 
  select(model, .metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate)

xgb_spec <- boost_tree(
  trees = 500, 
  tree_depth = 4,        # Shallow trees prevent overfitting in boosting
  learn_rate = 0.05      # Step size shrinkage
) %>% 
  set_engine("xgboost", importance = "impurity") %>% 
  set_mode("classification")

# 2. Fit the XGBoost Workflow
xgb_workflow <- workflow() %>% 
  add_recipe(match_recipe) %>% # Reuses your standardized recipe
  add_model(xgb_spec)

h2h_xgb_fit <- fit(xgb_workflow, data = m_train)

# 3. Generate Test Predictions for XGBoost
xgb_preds <- predict(h2h_xgb_fit, m_test) %>% 
  bind_cols(m_test) %>% 
  metrics(truth = blue_won_dragon, estimate = .pred_class) %>% 
  mutate(model = "XGBoost")

# 4. Update your side-by-side performance comparison matrix
final_performance_comparison <- bind_rows(rf_preds, log_preds, xgb_preds) %>% 
  select(model, .metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate)

print(final_performance_comparison)

processed_training_set %>% 
  distinct(teamId) %>% 
  pull(teamId)


