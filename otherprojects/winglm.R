source("functionstesting.R")
source("readdata.R")

# --- Same cleaning steps as your latest finalmodel.R ---
processed_data <- processed_data %>% 
  mutate(championName = if_else(championName == "MonkeyKing", "Wukong", championName))

# Drop matches missing one of the 5 valid roles
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

# ======================================================
# 1. Team-level aggregation (5-way positional split)
# ======================================================
team_level_features_win <- processed_data %>% 
  group_by(matchId, teamId) %>% 
  summarise(
    team_won          = max(win),                # NEW target source
    firstDragon       = max(firstDragon),
    initialCrabCount  = max(initialCrabCount, na.rm = TRUE),
    team_global_ults  = sum(has_global_ult, na.rm = TRUE),
    
    # Raw Gold per lane (kept only as an input to eff, not exported)
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
    
    # Lane stomp flag
    top_stomp        = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "TOP"]),
    jungle_stomp     = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "JUNGLE"]),
    mid_stomp        = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "MIDDLE"]),
    adc_stomp        = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "BOTTOM"]),    
    supp_stomp       = max(earlyLaningPhaseGoldExpAdvantage[teamPosition == "UTILITY"]),   
    
    # Minutes 3-7 Jungle Attention
    top_prox         = max(jungle_proximity_pct[teamPosition == "TOP"]),
    mid_prox         = max(jungle_proximity_pct[teamPosition == "MIDDLE"]),
    adc_prox         = max(jungle_proximity_pct[teamPosition == "BOTTOM"]),    
    supp_prox        = max(jungle_proximity_pct[teamPosition == "UTILITY"]),   
    
    # Minutes 3-7 Roaming Pct
    mid_roam         = max(roaming_pct[teamPosition == "MIDDLE"]),
    adc_roam         = max(roaming_pct[teamPosition == "BOTTOM"]),             
    supp_roam        = max(roaming_pct[teamPosition == "UTILITY"]),            
    
    .groups = "drop"
  ) %>% 
  mutate(
    # Updated efficiency formula: gold surplus vs. expected (20g/cs) baseline
    top_eff          = top_gold - (top_cs * 20),
    jungle_eff       = jungle_gold - (jungle_cs * 20),
    mid_eff          = mid_gold - (mid_cs * 20),
    adc_eff          = adc_gold - (adc_cs * 20),                              
    supp_eff         = supp_gold - (supp_cs * 20)
  )

# ======================================================
# 2. Pivot to Blue vs Red head-to-head format
# ======================================================
blue_side_win <- team_level_features_win %>% 
  filter(teamId == 100) %>% 
  select(
    matchId, teamId, blue_won = team_won, blue_drag = firstDragon, initialCrabCount, blue_globals = team_global_ults,
    blue_top_cs = top_cs, blue_jng_cs = jungle_cs, blue_mid_cs = mid_cs, blue_adc_cs = adc_cs, blue_supp_cs = supp_cs,
    blue_top_eff = top_eff, blue_jng_eff = jungle_eff, blue_mid_eff = mid_eff, blue_adc_eff = adc_eff, blue_supp_eff = supp_eff,
    blue_top_stomp = top_stomp, blue_jng_stomp = jungle_stomp, blue_mid_stomp = mid_stomp, blue_adc_stomp = adc_stomp, blue_supp_stomp = supp_stomp,
    blue_top_prox = top_prox, blue_mid_prox = mid_prox, blue_adc_prox = adc_prox, blue_supp_prox = supp_prox,
    blue_mid_roam = mid_roam, blue_adc_roam = adc_roam, blue_supp_roam = supp_roam
  )

red_side_win <- team_level_features %>% 
  filter(teamId == 200) %>% 
  select(
    matchId, teamId, red_drag = firstDragon, red_globals = team_global_ults,
    red_top_cs = top_cs, red_jng_cs = jungle_cs, red_mid_cs = mid_cs, red_adc_cs = adc_cs, red_supp_cs = supp_cs,
    red_top_eff = top_eff, red_jng_eff = jungle_eff, red_mid_eff = mid_eff, red_adc_eff = adc_eff, red_supp_eff = supp_eff,
    red_top_stomp = top_stomp, red_jng_stomp = jungle_stomp, red_mid_stomp = mid_stomp, red_adc_stomp = adc_stomp, red_supp_stomp = supp_stomp,
    red_top_prox = top_prox, red_mid_prox = mid_prox, red_adc_prox = adc_prox, red_supp_prox = supp_prox,
    red_mid_roam = mid_roam, red_adc_roam = adc_roam, red_supp_roam = supp_roam
  )

# ======================================================
# 3. Join + compute leads/gaps (signed Blue - Red, since
#    direction is the whole point for "did blue win")
# ======================================================
win_training_set <- blue_side_win %>%
  inner_join(red_side_win, by = "matchId", suffix = c("_blue", "_red")) %>%
  mutate(
    # TARGET VARIABLE
    blue_won = as.factor(blue_won),
    
    # Was first dragon secured by blue? (kept as a predictor, per your request,
    # to see how much it actually matters for the GAME outcome)
    firstDragon_lead = blue_drag - red_drag,
    
    # Global Mechanics
    global_ult_lead  = blue_globals - red_globals,
    
    # CS Leads (kept, per your request)
    top_cs_lead    = blue_top_cs - red_top_cs,
    jungle_cs_lead = blue_jng_cs - red_jng_cs,
    mid_cs_lead    = blue_mid_cs - red_mid_cs,
    adc_cs_lead    = blue_adc_cs - red_adc_cs,
    supp_cs_lead   = blue_supp_cs - red_supp_cs,
    
    # Gold Efficiency Leads (Blue_Eff - Red_Eff) -- kept; raw gold leads dropped
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
    
    # Jungle Hover Attention Gaps -- central to the jungler-impact insight
    top_prox_gap  = blue_top_prox - red_top_prox,
    mid_prox_gap  = blue_mid_prox - red_mid_prox,
    adc_prox_gap  = blue_adc_prox - red_adc_prox,
    supp_prox_gap = blue_supp_prox - red_supp_prox,
    
    # Pure Roaming Gaps
    mid_roam_gap  = blue_mid_roam - red_mid_roam,
    adc_roam_gap  = blue_adc_roam - red_adc_roam,
    supp_roam_gap = blue_supp_roam - red_supp_roam
  ) %>%
  
  # Strip raw per-side metrics to prevent leakage; keep only leads/gaps + target
  select(
    matchId,
    blue_won,
    initialCrabCount,
    global_ult_lead,
    firstDragon_lead,
    ends_with("_gold_eff_lead"),
    ends_with("_gap"),
    ends_with("cs_lead")
  ) %>%
  select(-supp_gold_eff_lead) 
cat("--- WIN-PREDICTION TRAINING SET ---\n")
print(glimpse(win_training_set))

# ======================================================
# 4. Split
# ======================================================
set.seed(4)
split <- initial_split(win_training_set, prop = 0.80, strata = blue_won)
training_set_win <- training(split)
testing_set_win  <- testing(split)

win_recipe <- recipe(blue_won ~ ., data = training_set_win) %>%
  update_role(matchId, new_role = "id")


# ======================================================
# 6. Smoothed variant (lower depth, mtry/sample_size noise)
#    -- same anchors as your latest smooth_xgb_spec
# ======================================================
smooth_xgb_win_spec <- boost_tree(
  trees = 500,
  learn_rate = 0.01,
  tree_depth = 3,
  min_n = 20,
  mtry = 4,
  sample_size = 0.75
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

smooth_wflow <- workflow() %>%
  add_recipe(win_recipe) %>%
  add_model(smooth_xgb_win_spec)

smooth_fit_win <- fit(smooth_wflow, data = training_set_win)

smooth_metrics_win <- predict(smooth_fit_win, testing_set_win) %>%
  bind_cols(testing_set_win) %>%
  metrics(truth = blue_won, estimate = .pred_class) %>%
  filter(.metric %in% c("accuracy", "kap")) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)

cat("\n--- GAME WINNER MODEL PERFORMANCE (smoothed) ---\n")
print(smooth_metrics_win)

final_smooth_win_model <- extract_fit_parsnip(smooth_fit_win)

final_smooth_win_model %>%
  vip(num_features = 23, geom = "col", aesthetics = list(fill = "darkgreen")) +
  theme_minimal() +
  labs(
    title = "XGBoost Feature Importance (Smoothed Model)",
    subtitle = "Where does jungler proximity/CS/efficiency rank for predicting who wins the match?",
    x = "Features",
    y = "Importance (Impurity)"
  )
pred_wrapper <- function(object, newdata) {
  predict(object, new_data = newdata)$.pred_class
}


perm_importance <- vip::vi_permute(
  smooth_fit_win,                       # the fitted WORKFLOW (recipe + model)
  train        = testing_set_win,       # permute on held-out data, not training data
  target       = "blue_won",
  metric       = "accuracy",
  pred_wrapper = pred_wrapper,
  nsim         = 10                 # average over 10 shuffles per feature for stability
)

cat("\n--- PERMUTATION IMPORTANCE (Smoothed Model) ---\n")
print(perm_importance, n = 23)

perm_importance %>%
  vip(num_features = 23, geom = "col", aesthetics = list(fill = "firebrick")) +
  theme_minimal() +
  labs(
    title = "Permutation Importance (Smoothed Model)",
    subtitle = "Drop in test accuracy when each feature is shuffled -- a fairer test of whether jungler proximity actually matters",
    x = "Features",
    y = "Importance (Accuracy Drop)"
  )

library(corrr)
correlation_data <- training_set_win %>%
  select(-matchId, -blue_won)   # numeric predictors only

corr_matrix <- correlate(correlation_data, method = "pearson", quiet = TRUE)

corr_matrix


# Pull out just the jungle <-> everything-else relationships,
# sorted by strength, to directly test the "junglers who
# support their ADC" hypothesis
cat("\n--- JUNGLE METRICS: CORRELATION WITH ALL OTHER FEATURES ---\n")
jungle_corrs <- corr_matrix %>%
  focus(jungle_gold_eff_lead, jungle_cs_lead, jungle_stomp_gap) %>%
  arrange(desc(abs(jungle_gold_eff_lead)))
print(jungle_corrs, n = Inf)

# Specifically: does jungle efficiency correlate with ADC
# efficiency/proximity, or are they unrelated?
cat("\n--- DIRECT TEST: Jungle Efficiency vs ADC Metrics ---\n")
cat("Correlation (jungle_gold_eff_lead, adc_gold_eff_lead): ",
    round(cor(correlation_data$jungle_gold_eff_lead, correlation_data$adc_gold_eff_lead, use = "complete.obs"), 3), "\n")
cat("Correlation (jungle_gold_eff_lead, adc_prox_gap):      ",
    round(cor(correlation_data$jungle_gold_eff_lead, correlation_data$adc_prox_gap, use = "complete.obs"), 3), "\n")
cat("Correlation (top_prox_gap (jungler-top), top_gold_eff_lead): ",
    round(cor(correlation_data$top_prox_gap, correlation_data$top_gold_eff_lead, use = "complete.obs"), 3), "\n")

# Full heatmap visualization
corr_matrix %>%
  rearrange(method = "MDS", absolute = FALSE) %>%
  shave() %>%
  rplot(print_cor = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Correlation Matrix: All Lead/Gap Features")

# ======================================================
# 9. Logistic Regression with Interaction Term
#    -- Direct test of "does jungler efficiency's effect on
#    win probability depend on jungler-ADC interaction?"
#    blue_won is binary, so this is the appropriate analog
#    to a two-way ANOVA for interaction effects.
# ======================================================

# Note: win_training_set was built from a 0.80 split earlier (training_set/
# testing_set); refit the GLM on the full set here since we're doing
# inference (testing significance), not held-out prediction.
win_training_set_glm <- training_set_win %>%
  mutate(blue_won_num = as.numeric(as.character(blue_won)))  # glm needs 0/1, not factor levels for binomial response with formula interface; factor also works directly

# --- Model 1: Jungle Efficiency x ADC Proximity ---
jungle_adc_interaction_model <- glm(
  blue_won ~ jungle_gold_eff_lead * adc_prox_gap,
  family = binomial,
  data = win_training_set
)

cat("\n=== MODEL 1: Jungle Efficiency x ADC Proximity ===\n")
print(summary(jungle_adc_interaction_model))

cat("\n--- ANOVA Deviance Table (Model 1) ---\n")
print(anova(jungle_adc_interaction_model, test = "Chisq"))

# --- Model 2: Jungle Efficiency x ADC Efficiency (the direct "do they move together" test) ---
jungle_adc_eff_model <- glm(
  blue_won ~ jungle_gold_eff_lead * adc_gold_eff_lead,
  family = binomial,
  data = win_training_set
)

cat("\n=== MODEL 2: Jungle Efficiency x ADC Efficiency ===\n")
print(summary(jungle_adc_eff_model))

cat("\n--- ANOVA Deviance Table (Model 2) ---\n")
print(anova(jungle_adc_eff_model, test = "Chisq"))

# --- Model 3 (comparison): Jungle Efficiency x Mid Stomp ---
# Included because your correlation matrix suggested jungle <-> mid
# is a stronger relationship than jungle <-> adc -- useful contrast.
jungle_mid_model <- glm(
  blue_won ~ jungle_gold_eff_lead * mid_stomp_gap,
  family = binomial,
  data = win_training_set
)

cat("\n=== MODEL 3 (comparison): Jungle Efficiency x Mid Stomp ===\n")
print(summary(jungle_mid_model))

cat("\n--- ANOVA Deviance Table (Model 3) ---\n")
print(anova(jungle_mid_model, test = "Chisq"))

cat("\n--- HOW TO READ THESE ---\n")
cat("Look at the p-value (Pr(>Chi) or Pr(>|z|)) on the INTERACTION row\n")
cat("(e.g. 'jungle_gold_eff_lead:adc_prox_gap'). If it's < 0.05, the two\n")
cat("variables interact significantly in predicting blue_won. If it's\n")
cat("large, the two features act independently -- consistent with what\n")
cat("the correlation matrix already suggested.\n")

# ======================================================
# 10. Jungle Impact on Top/Mid/ADC -- Summary Visualization
#    -- Correlation strength between jungle's 3 metrics
#    (efficiency, CS, stomp) and each lane's own metrics.
#    Three formats of the same underlying data.
# ======================================================
library(tidyr)

jungle_vars <- c("jungle_gold_eff_lead", "jungle_cs_lead", "jungle_stomp_gap")
lane_vars <- list(
  Top = c("top_gold_eff_lead", "top_cs_lead", "top_stomp_gap", "top_prox_gap"),
  Mid = c("mid_gold_eff_lead", "mid_cs_lead", "mid_stomp_gap", "mid_prox_gap", "mid_roam_gap"),
  ADC = c("adc_gold_eff_lead", "adc_cs_lead", "adc_stomp_gap", "adc_prox_gap", "adc_roam_gap")
)

# Build a tidy long-format table: jungle_var x lane_var x correlation
jungle_lane_corrs <- map_df(names(lane_vars), function(lane_name) {
  map_df(jungle_vars, function(j_var) {
    map_df(lane_vars[[lane_name]], function(l_var) {
      tibble(
        jungle_metric = j_var,
        lane = lane_name,
        lane_metric = l_var,
        correlation = cor(correlation_data[[j_var]], correlation_data[[l_var]], use = "complete.obs")
      )
    })
  })
})

# Summary: average absolute correlation of jungle with each lane
# (single clean number per lane for the headline visual)
jungle_lane_summary <- jungle_lane_corrs %>%
  group_by(lane) %>%
  summarise(avg_abs_corr = mean(abs(correlation)), .groups = "drop") %>%
  arrange(desc(avg_abs_corr))

cat("\n--- JUNGLE IMPACT SUMMARY: Avg |correlation| with each lane ---\n")
print(jungle_lane_summary)

# ---------------------------------------------------------
# FORMAT 1: Single bar chart, one bar per lane
# ---------------------------------------------------------
ggplot(jungle_lane_summary, aes(x = reorder(lane, avg_abs_corr), y = avg_abs_corr, fill = lane)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", avg_abs_corr)), hjust = -0.2) +
  coord_flip() +
  scale_fill_manual(values = c(Top = "#2c7fb8", Mid = "#41ab5d", ADC = "#e34a33")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "How Much Does the Jungler Affect Each Lane?",
    subtitle = "Average absolute correlation between jungle metrics and each lane's early-game performance",
    x = NULL,
    y = "Average |Correlation| with Jungle Metrics"
  )

# ---------------------------------------------------------
# FORMAT 2: Small multiples -- 3 panels, one per lane,
# showing each jungle metric's correlation with that lane's
# individual metrics (more granular than Format 1)
# ---------------------------------------------------------
ggplot(jungle_lane_corrs, aes(x = lane_metric, y = jungle_metric, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", correlation)), size = 3) +
  facet_wrap(~ lane, scales = "free_x", ncol = 1) +
  scale_fill_gradient2(low = "#d7301f", mid = "white", high = "#2c7fb8", midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Jungle Metrics vs Each Lane's Performance",
    subtitle = "Correlation between jungle efficiency/CS/stomp and each lane's own early-game metrics",
    x = "Lane Metric",
    y = "Jungle Metric",
    fill = "Corr"
  )

# ---------------------------------------------------------
# FORMAT 3: Heatmap-style grid, same layout family as your
# existing corrr::rplot() heatmap but isolated to jungle row
# vs top/mid/adc columns only
# ---------------------------------------------------------
jungle_lane_corrs %>%
  mutate(lane_metric = factor(lane_metric, levels = unique(lane_metric))) %>%
  ggplot(aes(x = lane_metric, y = jungle_metric)) +
  geom_point(aes(size = abs(correlation), color = correlation)) +
  scale_color_gradient2(low = "#d7301f", mid = "grey90", high = "#2c7fb8", midpoint = 0, limits = c(-1, 1)) +
  scale_size(range = c(1, 14), limits = c(0, 1)) +
  facet_grid(~ lane, scales = "free_x", space = "free_x") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Jungle Impact Across Lanes",
    subtitle = "Bubble size and color both reflect correlation strength -- bigger/bluer means stronger jungle relationship",
    x = NULL,
    y = NULL,
    color = "Correlation",
    size = "|Correlation|"
  )
