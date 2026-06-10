team_level_features <- processed_data %>% 
  group_by(matchId, teamId) %>% 
  summarise(
    firstDragon      = max(firstDragon),
    initialCrabCount = max(initialCrabCount, na.rm = TRUE),
    team_global_ults = sum(has_global_ult, na.rm = TRUE),
    
    # 1. Early Resource Generation (Raw Gold)
    top_gold         = max(early_gold_per_min[teamPosition == "TOP"]),
    jungle_gold      = max(early_gold_per_min[teamPosition == "JUNGLE"]),
    mid_gold         = max(early_gold_per_min[teamPosition == "MIDDLE"]),
    adc_gold         = max(early_gold_per_min[teamPosition == "BOTTOM"]),    
    supp_gold        = max(early_gold_per_min[teamPosition == "UTILITY"]),    
    
    # 2. Early Resource Generation (Raw CS)
    top_cs           = max(early_cs_per_min[teamPosition == "TOP"]),
    jungle_cs        = max(early_cs_per_min[teamPosition == "JUNGLE"]),
    mid_cs           = max(early_cs_per_min[teamPosition == "MIDDLE"]),
    adc_cs           = max(early_cs_per_min[teamPosition == "BOTTOM"]),      
    supp_cs          = max(early_cs_per_min[teamPosition == "UTILITY"]),      
    
    # 3. Resource Efficiency Metrics (Gold per CS)
    top_eff          = top_gold / max(top_cs, 1),
    jungle_eff       = jungle_gold / max(jungle_cs, 1),
    mid_eff          = mid_gold / max(mid_cs, 1),
    adc_eff          = adc_gold / max(adc_cs, 1),                             
    supp_eff         = supp_gold / max(supp_cs, 1),                           
    
    # 4. Zero-Sum Lane Stomp Matchups
    top_stomp        = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "TOP"]),
    jungle_stomp     = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "JUNGLE"]),
    mid_stomp        = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "MIDDLE"]),
    adc_stomp        = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "BOTTOM"]),    
    supp_stomp       = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "UTILITY"]),   
    # 5. Minutes 3-7 Jungle Attention / Hover Pct per Lane
    top_prox         = max(jungle_proximity_pct[teamPosition == "TOP"]),
    mid_prox         = max(jungle_proximity_pct[teamPosition == "MIDDLE"]),
    adc_prox         = max(jungle_proximity_pct[teamPosition == "BOTTOM"]),    
    supp_prox        = max(jungle_proximity_pct[teamPosition == "UTILITY"]),   
    
    # 6. Minutes 3-7 Roaming Pct
    mid_roam         = max(roaming_pct[teamPosition == "MIDDLE"]),
    adc_roam         = max(roaming_pct[teamPosition == "BOTTOM"]),             
    supp_roam        = max(roaming_pct[teamPosition == "UTILITY"]),            
    
    .groups = "drop"
  )

blue_side <- team_level_features %>% 
  filter(teamId == 100) %>% 
  select(
    matchId, teamId, blue_drag = firstDragon, initialCrabCount, blue_globals = team_global_ults,
    blue_top_g = top_gold, blue_jng_g = jungle_gold, blue_mid_g = mid_gold, blue_adc_g = adc_gold, blue_supp_gold = supp_gold,
    blue_top_cs = top_cs, blue_jng_cs = jungle_cs, blue_mid_cs = mid_cs, blue_adc_cs = adc_cs, blue_supp_cs = supp_cs,
    blue_top_eff = top_eff, blue_jng_eff = jungle_eff, blue_mid_eff = mid_eff, blue_adc_eff = adc_eff, blue_supp_eff = supp_eff,
    blue_top_stomp = top_stomp, blue_jng_stomp = jungle_stomp, blue_mid_stomp = mid_stomp, blue_adc_stomp = adc_stomp, blue_supp_stomp = supp_stomp,
    blue_top_prox = top_prox, blue_mid_prox = mid_prox, blue_adc_prox = adc_prox, blue_sup_prox = supp_prox,
    blue_mid_roam = mid_roam, blue_adc_roam = adc_roam, blue_sup_roam = supp_roam
  )

red_side <- team_level_features %>% 
  filter(teamId == 200) %>% 
  select(
    matchId, teamId, red_drag = firstDragon, initialCrabCount, red_globals = team_global_ults,
    red_top_g = top_gold, red_jng_g = jungle_gold, red_mid_g = mid_gold, red_adc_g = adc_gold, red_supp_gold = supp_gold,
    red_top_cs = top_cs, red_jng_cs = jungle_cs, red_mid_cs = mid_cs, red_adc_cs = adc_cs, red_supp_cs = supp_cs,
    red_top_eff = top_eff, red_jng_eff = jungle_eff, red_mid_eff = mid_eff, red_adc_eff = adc_eff, red_supp_eff = supp_eff,
    red_top_stomp = top_stomp, red_jng_stomp = jungle_stomp, red_mid_stomp = mid_stomp, red_adc_stomp = adc_stomp, red_supp_stomp = supp_stomp,
    red_top_prox = top_prox, red_mid_prox = mid_prox, red_adc_prox = adc_prox, red_sup_prox = supp_prox,
    red_mid_roam = mid_roam, red_adc_roam = adc_roam, red_sup_roam = supp_roam
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
    supp_prox_gap = blue_sup_prox - red_sup_prox,
    
    # Pure Roaming Gaps
    mid_roam_gap  = blue_mid_roam - red_mid_roam,
    adc_roam_gap  = blue_adc_roam - red_adc_roam,
    supp_roam_gap = blue_sup_roam - red_sup_roam
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
    who_won_drag = factor(blue_drag_secured, levels = c(0, 1), labels = c("Red_Drag", "Blue_Drag"))
  ) %>%
  select(-blue_drag_secured)

cat("--- MATCH LEVEL TRAINING SET COMPLETED ---\n")
print(glimpse(match_level_training_set))

set.seed(42)
dragon_split <- initial_split(match_level_training_set, prop = 0.80, strata = who_won_drag)
d_train      <- training(dragon_split)
d_test       <- testing(dragon_split)

# 2. Recipe Specification
dragon_recipe <- recipe(who_won_drag ~ ., data = d_train) %>% 
  update_role(matchId, new_role = "id")

# 3. Model Engine Specification (Using your locked-in hyperparameter anchors)
dragon_xgb_spec <- boost_tree(
  trees = 500,          
  tree_depth = 3,       
  learn_rate = 0.01,    
  min_n = 40           
) %>%
  set_engine("xgboost", importance = "impurity") %>%
  set_mode("classification")

# 4. Pipeline Workflow Consolidation
dragon_workflow <- workflow() %>%
  add_recipe(dragon_recipe) %>%
  add_model(dragon_xgb_spec)

# 5. Fit the Final Model
cat("Fitting position-separated macro model on training data...\n")
final_dragon_fit <- fit(dragon_workflow, data = d_train)

# 6. Performance Evaluation
final_performance <- predict(final_dragon_fit, d_test) %>%
  bind_cols(d_test) %>% 
  metrics(truth = who_won_drag, estimate = .pred_class) %>%
  filter(.metric %in% c("accuracy", "kap")) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)

cat("\n--- FINAL POSITION-SEPARATED MODEL PERFORMANCE ---\n")
print(final_performance)


final_dragon_fit %>%
  extract_fit_parsnip() %>%
  # Let's look at the top 12 features to see where the split lanes land
  vip(num_features = 12, geom = "col", aesthetics = list(fill = "midnightblue")) +
  theme_minimal() +
  labs(
    title = "XGBoost Structural Feature Importance",
    subtitle = "Which granular lane dynamics dictate First Dragon control?",
    x = "Granular Match Gaps",
    y = "Importance (Node Impurity)"
  )

# Assumes your 9,950 player-level dataset is named 'processed_data'
champion_profiles <- processed_data %>%
  group_by(championName) %>%
  summarise(
    # 1 & 2. Gold and Gold Efficiency Baselines
    base_gold_pm    = mean(early_gold_per_min, na.rm = TRUE),
    base_cs_pm      = mean(early_cs_per_min, na.rm = TRUE),
    base_efficiency = mean(early_gold_per_min, na.rm = TRUE) / max(mean(early_cs_per_min, na.rm = TRUE), 1),
    
    # 3. Lane Stomp Baseline (Inherent gold/exp advantage tendencies)
    base_stomp      = mean(earlyLaningPhaseGoldExpAdvantage, na.rm = TRUE),
    
    # 4. Proximity / Jungle Attention Baseline
    base_proximity  = mean(jungle_proximity_pct, na.rm = TRUE),
    
    # 5. Roaming Baseline
    base_roaming    = mean(roaming_pct, na.rm = TRUE),
    
    # Meta tracking to filter out ultra-rare or troll picks
    games_played    = n(),
    .groups = "drop"
  ) %>%
  # Keep only champions with at least 5 games for statistical stability
  filter(games_played >= 5) %>%
  # Keep exactly what the simulator needs to map to the model
  select(championName, base_gold_pm, base_efficiency, base_stomp, base_proximity, base_roaming)

cat("--- FINALIZED 5-DIMENSION CHAMPION PROFILES REBUILT ---\n")
print(head(champion_profiles))



dragon_recipe <- recipe(blue_drag_secured ~ ., data = training_set) %>% 
  update_role(matchId, new_role = "id") %>%
  # This is the magic bullet. It balances the classes to 50/50.
  step_downsample(blue_drag_secured) 

# 2. Re-bundle the workflow
dragon_workflow <- workflow() %>%
  add_recipe(dragon_recipe) %>%
  add_model(dragon_xgb_spec) # (Whatever you named your parsnip model)

# 3. Re-train the model
final_dragon_fit <- dragon_workflow %>% fit(data = training_set)

final_dragon_fit %>%
  extract_fit_parsnip() %>%
  # Let's look at the top 12 features to see where the split lanes land
  vip(num_features = 12, geom = "col", aesthetics = list(fill = "midnightblue")) +
  theme_minimal() +
  labs(
    title = "XGBoost Structural Feature Importance",
    subtitle = "Which granular lane dynamics dictate First Dragon control?",
    x = "Granular Match Gaps",
    y = "Importance (Node Impurity)"
  )

# 6. Performance Evaluation
final_performance <- predict(final_dragon_fit, testing_set) %>%
  bind_cols(testing_set) %>% 
  metrics(truth = blue_drag_secured, estimate = .pred_class) %>%
  filter(.metric %in% c("accuracy", "kap")) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)

cat("\n--- FINAL POSITION-SEPARATED MODEL PERFORMANCE ---\n")
print(final_performance)

