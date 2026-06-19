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
  inner_join(
    red_side %>% filter(teamId == 200),
    by = "matchId", 
    suffix = c("_blue", "_red")
  ) %>%
  mutate(
    # 1. Determine who is the overall "Leader" based on raw total early gold
    blue_total_gold = blue_top_g + blue_jng_g + blue_mid_g + blue_adc_g + blue_supp_gold,
    red_total_gold  = red_top_g + red_jng_g + red_mid_g + red_adc_g + red_supp_gold,
    
    # Absolute total gold gap (so the model knows the overall magnitude of the lead)
    total_gold_advantage = abs(blue_total_gold - red_total_gold),
    
    # Boolean flag to track which side is the Leader
    blue_is_leader = blue_total_gold >= red_total_gold,
    
    # 2. NEW TARGET VARIABLE: Did the "Leader" secure the dragon?
    leader_won_drag = as.factor(if_else(
      (blue_is_leader & blue_drag == 1) | (!blue_is_leader & red_drag == 1), 
      "Leader_Drag", "Trailer_Drag"
    )),
    
    # 3. LEADER-CENTRIC DELTAS (Leader Stats minus Trailer Stats)
    global_ult_gap   = if_else(blue_is_leader, blue_globals - red_globals, red_globals - blue_globals),
    
    top_gold_gap     = if_else(blue_is_leader, blue_top_g - red_top_g, red_top_g - blue_top_g),
    jungle_gold_gap  = if_else(blue_is_leader, blue_jng_g - red_jng_g, red_jng_g - blue_jng_g),
    mid_gold_gap     = if_else(blue_is_leader, blue_mid_g - red_mid_g, red_mid_g - blue_mid_g),
    adc_gold_gap     = if_else(blue_is_leader, blue_adc_g - red_adc_g, red_adc_g - blue_adc_g),
    supp_gold_gap    = if_else(blue_is_leader, blue_supp_gold - red_supp_gold, red_supp_gold - blue_supp_gold),
    
    top_gold_eff_gap     = if_else(blue_is_leader, blue_top_eff - red_top_eff, red_top_eff - blue_top_eff),
    jungle_gold_eff_gap  = if_else(blue_is_leader, blue_jng_eff - red_jng_eff, red_jng_eff - blue_jng_eff),
    mid_gold_eff_gap     = if_else(blue_is_leader, blue_mid_eff - red_mid_eff, red_mid_eff - blue_mid_eff),
    adc_gold_eff_gap     = if_else(blue_is_leader, blue_adc_eff - red_adc_eff, red_adc_eff - blue_adc_eff),
    supp_gold_eff_gap    = if_else(blue_is_leader, blue_supp_eff - red_supp_eff, red_supp_eff - blue_supp_eff),
    
    top_stomp_gap    = if_else(blue_is_leader, blue_top_stomp - red_top_stomp, red_top_stomp - blue_top_stomp),
    jungle_stomp_gap = if_else(blue_is_leader, blue_jng_stomp - red_jng_stomp, red_jng_stomp - blue_jng_stomp),
    mid_stomp_gap    = if_else(blue_is_leader, blue_mid_stomp - red_mid_stomp, red_mid_stomp - blue_mid_stomp),
    adc_stomp_gap    = if_else(blue_is_leader, blue_adc_stomp - red_adc_stomp, red_adc_stomp - blue_adc_stomp),
    supp_stomp_gap   = if_else(blue_is_leader, blue_supp_stomp - red_supp_stomp, red_supp_stomp - blue_supp_stomp),
    
    top_prox_gap     = if_else(blue_is_leader, blue_top_prox - red_top_prox, red_top_prox - blue_top_prox),
    mid_prox_gap     = if_else(blue_is_leader, blue_mid_prox - red_mid_prox, red_mid_prox - blue_mid_prox),
    adc_prox_gap     = if_else(blue_is_leader, blue_adc_prox - red_adc_prox, red_adc_prox - blue_adc_prox),
    supp_prox_gap    = if_else(blue_is_leader, blue_sup_prox - red_sup_prox, red_sup_prox - blue_sup_prox),
    
    mid_roam_gap     = if_else(blue_is_leader, blue_mid_roam - red_mid_roam, red_mid_roam - blue_mid_roam),
    adc_roam_gap     = if_else(blue_is_leader, blue_adc_roam - red_adc_roam, red_adc_roam - blue_adc_roam),
    supp_roam_gap    = if_else(blue_is_leader, blue_sup_roam - red_sup_roam, red_sup_roam - blue_sup_roam)
  ) %>%
  select(
    matchId,
    leader_won_drag,
    initialCrabCount = initialCrabCount_blue,
    global_ult_gap,
    ends_with("_gap")
  )

#Saving the "blind" training set
saveRDS(match_level_training_set, "data/data2/match_level_training_set.rds")

set.seed(42)
dragon_split <- initial_split(match_level_training_set, prop = 0.80, strata = leader_won_drag)
d_train      <- training(dragon_split)
d_test       <- testing(dragon_split)


# 1. The "Blind Leader" Recipe
# We no longer need themis::step_downsample because the relationship 
# between 'Lead' and 'Dragon' is a real, inherent game property.
dragon_recipe <- recipe(leader_won_drag ~ ., data = match_level_training_set) %>% 
  # We still keep normalization out! 
  # This recipe is now purely a structural definition for the workflow.
  step_rm(matchId) # Remove the match identifier so the model doesn't overfit to it

# 2. The Regularized Engine
# These parameters keep the model general and prevent it from memorizing "zero" states.
xgboost_blind_spec <- boost_tree(
  trees = 500,
  tree_depth = 3,      
  min_n = 40,          
  learn_rate = 0.01    
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# 3. Create and Fit the Blind Workflow
dragon_workflow_blind <- workflow() %>%
  add_recipe(dragon_recipe) %>%
  add_model(xgboost_blind_spec)

# Fit on your new 'leader_won_drag' dataset
final_dragon_fit <- dragon_workflow_blind %>% fit(data = match_level_training_set)


# 6. Performance Evaluation
final_performance <- predict(final_dragon_fit, d_test) %>%
  bind_cols(d_test) %>% 
  metrics(truth = leader_won_drag, estimate = .pred_class) %>%
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


champion_profiles <- processed_data %>% 
  group_by(championName) %>% 
  summarise(
    base_cs_per_min   = mean(early_cs_per_min, na.rm = TRUE),
    base_gold_pm      = mean(early_gold_per_min, na.rm = TRUE),
    base_proximity    = mean(jungle_proximity_pct, na.rm = TRUE),
    base_roaming      = mean(roaming_pct, na.rm = TRUE),
    base_efficiency   = base_gold_pm / base_cs_per_min,
    base_stomp        = mean(earlyLaningPhaseGoldExpAdvantage, na.rm = TRUE),
    
    # Keeps the static 1 or 0 designation per champion profile
    has_global_ult    = unique(has_global_ult), 
    .groups = "drop")

unique(champion_profiles$championName)

table(match_level_training_set$leader_won_drag)
