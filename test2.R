team_level_features <- processed_training_set |> 
  group_by(matchId, teamId) |> 
  summarise(
    firstDragon          = as.factor(unique(firstDragon)),
    initialCrabCount     = unique(initialCrabCount),
    
    # Keep total gold: tracks overall lane dominance
    team_gold_per_min    = sum(early_gold_per_min, na.rm = TRUE),
    
    # Calculate the Efficiency Ratio: captures farming vs fighting archetype
    # (Total Gold divided by Total Minions)
    team_gold_efficiency = sum(early_gold_per_min, na.rm = TRUE) / sum(early_cs_per_min, na.rm = TRUE),
    
    # Keep your high-performing spatial metrics
    avg_jungle_proximity = mean(jungle_proximity_pct, na.rm = TRUE),
    avg_roaming_pct      = mean(roaming_pct, na.rm = TRUE),
    team_global_ults     = sum(has_global_ult, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Pivot to Wide Head-to-Head Format (Blue side vs. Red side)
blue_side <- team_level_features |> 
  filter(teamId == 100) |> 
  select(matchId, blue_gold = team_gold_per_min, 
         blue_prox = avg_jungle_proximity, blue_roam = avg_roaming_pct,
         blue_globals = team_global_ults, blue_drag = firstDragon, initialCrabCount, 
         blue_efficiency = team_gold_efficiency)

red_side <- team_level_features |> 
  filter(teamId == 200) |> 
  select(matchId, red_gold = team_gold_per_min, 
         red_prox = avg_jungle_proximity, red_roam = avg_roaming_pct,
         red_globals = team_global_ults, 
         red_efficiency = team_gold_efficiency)

match_level_training_set <- blue_side |> 
  inner_join(red_side, by = "matchId") |> 
  mutate(blue_won_dragon = as.factor(if_else(blue_drag == 1, 1, 0))) |> 
  select(-matchId, -blue_drag)

champion_profiles <- processed_training_set |> 
  group_by(championName) |> 
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

match_recipe <- recipe(blue_won_dragon ~ ., data = m_train) |> 
  step_normalize(all_numeric_predictors())
log_reg_spec <- logistic_reg() |> 
  set_engine("glm") |> 
  set_mode("classification")

rf_spec <- rand_forest(trees = 500) |> 
  set_engine("ranger", importance = "impurity") |> 
  set_mode("classification")


rf_workflow <- workflow() |> 
  add_recipe(match_recipe) |> 
  add_model(rf_spec)
h2h_rf_fit <- fit(rf_workflow, data = m_train)

log_workflow <- workflow() |> 
  add_recipe(match_recipe) |> 
  add_model(log_reg_spec)

h2h_log_fit <- fit(log_workflow, data = m_train)

# Generate Random Forest Test Predictions
rf_preds <- predict(h2h_rf_fit, m_test) |> 
  bind_cols(m_test) |> 
  metrics(truth = blue_won_dragon, estimate = .pred_class) |> 
  mutate(model = "Random Forest")

# Generate Logistic Regression Test Predictions
log_preds <- predict(h2h_log_fit, m_test) |> 
  bind_cols(m_test) |> 
  metrics(truth = blue_won_dragon, estimate = .pred_class) |> 
  mutate(model = "Logistic Regression")

# Bind metrics together for a side-by-side comparison matrix
performance_comparison <- bind_rows(rf_preds, log_preds) |> 
  select(model, .metric, .estimate) |> 
  pivot_wider(names_from = .metric, values_from = .estimate)

print(performance_comparison)

log_importance <- h2h_log_fit |> 
  extract_fit_engine() |> 
  tidy() |> 
  filter(term != "(Intercept)") |> 
  mutate(
    # Use the absolute value of the statistic (Z-score) as the importance metric
    Importance = abs(statistic) 
  )
h2h_rf_fit |> 
  extract_fit_engine() |> 
  vip(num_features = 11, geom = "col", aesthetics = list(fill = "#3182bd")) +
  theme_minimal() +
  labs(
    title = "Which Early Game Stats Matter Most for First Dragon?",
    subtitle = "Random Forest Feature Importance (Head-to-Head Model)",
    x = "Predictors",
    y = "Importance (Impurity)"
  )

# 2. Plot it using the exact same style as your vip package output
ggplot(log_importance, aes(x = Importance, y = reorder(term, Importance))) +
  geom_col(fill = "#e6550d") + # Using a distinct orange color to tell it apart from RF
  theme_minimal() +
  labs(
    title = "Which Early Game Stats Matter Most for First Dragon?",
    subtitle = "Logistic Regression Feature Importance (Absolute Z-Statistic)",
    x = "Importance (Magnitude of Effect)",
    y = "Predictors"
  )

blue_comp <- c("K'Sante", "MonkeyKing", "Ryze", "Ezreal", "Neeko")
red_comp <- c("Rumble", "Nocturne", "Anivia", "Jhin", "Bard")
simulate_champion_draft(blue_comp, red_comp, scuttle_count = 0)

View(training_dataset %>%
       filter(teamPosition == "JUNGLE") %>%
       select(championName, initialCrabCount))
