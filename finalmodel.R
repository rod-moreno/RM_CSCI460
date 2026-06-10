processed_data <- processed_data %>% 
  mutate(teamPosition = if_else(teamPosition == "", "MIDDLE", teamPosition))

team_level_features <- processed_data %>% 
  group_by(matchId, teamId) %>% 
  summarise(
    firstDragon      = max(firstDragon),
    initialCrabCount = max(initialCrabCount, na.rm = TRUE),
    team_global_ults = sum(has_global_ult, na.rm = TRUE),
    
    # 1. Early Resource Generation
    top_gold         = max(early_gold_per_min[teamPosition == "TOP"]),
    jungle_gold      = max(early_gold_per_min[teamPosition == "JUNGLE"]),
    mid_gold         = max(early_gold_per_min[teamPosition == "MIDDLE"]),
    bot_gold         = sum(early_gold_per_min[teamPosition %in% c("BOTTOM", "UTILITY")]),
    
    top_cs           = max(early_cs_per_min[teamPosition == "TOP"]),
    jungle_cs        = max(early_cs_per_min[teamPosition == "JUNGLE"]),
    mid_cs           = max(early_cs_per_min[teamPosition == "MIDDLE"]),
    bot_cs           = sum(early_cs_per_min[teamPosition %in% c("BOTTOM", "UTILITY")]),
    
    # 2. Zero-Sum Lane Stomp Matchups (1 or 0)
    top_stomp        = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "TOP"]),
    jungle_stomp     = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "JUNGLE"]),
    mid_stomp        = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "MIDDLE"]),
    bot_stomp        = max(earlyLaningPhaseGoldExpAdvantage[teamPosition %in% c("BOTTOM", "UTILITY")]),
    
    # 3. Minutes 3-7 Jungle Attention / Hover Pct per Lane
    top_prox         = max(jungle_proximity_pct[teamPosition == "TOP"]),
    mid_prox         = max(jungle_proximity_pct[teamPosition == "MIDDLE"]),
    bot_prox         = max(jungle_proximity_pct[teamPosition == "BOTTOM"]),
    supp_prox        = max(jungle_proximity_pct[teamPosition == "UTILITY"]),
    
    # 4. Minutes 3-7 Roaming Pct
    mid_roam         = max(roaming_pct[teamPosition == "MIDDLE"]),
    bot_roam         = max(roaming_pct[teamPosition == "BOTTOM"]),
    supp_roam        = max(roaming_pct[teamPosition == "UTILITY"]),
    
    .groups = "drop"
  )

blue_side <- team_level_features %>% 
  filter(teamId == 100) %>% 
  select(
    matchId, blue_drag = firstDragon, initialCrabCount, blue_globals = team_global_ults,
    blue_top_g = top_gold, blue_jng_g = jungle_gold, blue_mid_g = mid_gold, blue_bot_g = bot_gold,
    blue_top_cs = top_cs, blue_jng_cs = jungle_cs, blue_mid_cs = mid_cs, blue_bot_cs = bot_cs,
    blue_top_stomp = top_stomp, blue_jng_stomp = jungle_stomp, blue_mid_stomp = mid_stomp, blue_bot_stomp = bot_stomp,
    blue_top_prox = top_prox, blue_mid_prox = mid_prox, blue_bot_prox = bot_prox, blue_sup_prox = supp_prox,
    blue_mid_roam = mid_roam, blue_bot_roam = bot_roam, blue_sup_roam = supp_roam
  )

red_side <- team_level_features %>% 
  filter(teamId == 200) %>% 
  select(
    matchId, red_globals = team_global_ults,
    red_top_g = top_gold, red_jng_g = jungle_gold, red_mid_g = mid_gold, red_bot_g = bot_gold,
    red_top_cs = top_cs, red_jng_cs = jungle_cs, red_mid_cs = mid_cs, red_bot_cs = bot_cs,
    red_top_stomp = top_stomp, red_jng_stomp = jungle_stomp, red_mid_stomp = mid_stomp, red_bot_stomp = bot_stomp,
    red_top_prox = top_prox, red_mid_prox = mid_prox, red_bot_prox = bot_prox, red_sup_prox = supp_prox,
    red_mid_roam = mid_roam, red_bot_roam = bot_roam, red_sup_roam = supp_roam
  )


match_level_training_set <- blue_side %>% 
  inner_join(red_side, by = "matchId") %>% 
  mutate(
    # Gold and CS Absolute Leads
    top_gold_lead    = abs(blue_top_g - red_top_g),
    jungle_gold_lead = abs(blue_jng_g - red_jng_g),
    mid_gold_lead    = abs(blue_mid_g - red_mid_g),
    bot_gold_lead    = abs(blue_bot_g - red_bot_g),
    
    top_cs_lead      = abs(blue_top_cs - red_top_cs),
    jungle_cs_lead   = abs(blue_jng_cs - red_jng_cs),
    mid_cs_lead      = abs(blue_mid_cs - red_mid_cs),
    bot_cs_lead      = abs(blue_bot_cs - red_bot_cs),
    
    # Jungle Hover Discrepancies 
    top_prox_lead    = abs(blue_top_prox - red_top_prox),
    mid_prox_lead    = abs(blue_mid_prox - red_mid_prox),
    bot_prox_lead    = abs(blue_bot_prox - red_bot_prox),
    supp_prox_lead   = abs(blue_sup_prox - red_sup_prox),
    
    # Roaming Gaps
    mid_roam_lead    = abs(blue_mid_roam - red_mid_roam),
    bot_roam_lead    = abs(blue_bot_roam - red_bot_roam),
    supp_roam_lead   = abs(blue_sup_roam - red_sup_roam),
    
    globals_lead     = abs(blue_globals - red_globals),
    
    # Binary Stomp Occurrences (0 = even, 1 = lane blowout)
    top_stomp_occurred    = abs(blue_top_stomp - red_top_stomp),
    jungle_stomp_occurred = abs(blue_jng_stomp - red_jng_stomp),
    mid_stomp_occurred    = abs(blue_mid_stomp - red_mid_stomp),
    bot_stomp_occurred    = abs(blue_bot_stomp - red_bot_stomp),
    
    # Global Gold Baseline to isolate the "Advantage Team"
    blue_total_early_gold = blue_top_g + blue_jng_g + blue_mid_g + blue_bot_g,
    red_total_early_gold  = red_top_g + red_jng_g + red_mid_g + red_bot_g,
    blue_has_lead         = if_else(blue_total_early_gold >= red_total_early_gold, 1, 0),
    
    # Target Alignment: Did the economic leader get the dragon?
    leader_won_dragon     = as.factor(if_else(blue_has_lead == blue_drag, 1, 0)),
    initialCrabCount      = initialCrabCount
  ) %>% 
  select(matchId, leader_won_dragon, ends_with("_lead"), ends_with("_occurred"), initialCrabCount) %>%
  select(-blue_has_lead)


set.seed(4)
split <- initial_split(match_level_training_set, prop = 0.80, strata = leader_won_dragon)
training_set <- training(split)
testing_set <- testing(split)

modelrecipe <- recipe(leader_won_dragon ~ ., data = training_set) %>%
update_role(matchId, new_role = 'id') %>%
  step_normalize(all_numeric_predictors())

xgb_spec <- boost_tree(
  trees = 150,           
  tree_depth = 6,       
  learn_rate = 0.05,    
  min_n = 10            
) %>%
  set_engine("xgboost", importance = "impurity") %>%
  set_mode("classification")

model_workflow <- workflow() %>%
  add_recipe(modelrecipe) %>%
  add_model(xgb_spec)


model <- fit(model_workflow, data = training_set)


performance_metrics <- predict(model, testing_set) %>%
  bind_cols(testing_set) %>% 
  metrics(truth = leader_won_dragon, estimate = .pred_class) %>%
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

