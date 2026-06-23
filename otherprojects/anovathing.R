source("functionstesting.R")
library(supernova)

levels(match_level_training_set$leader_won_drag)
table(match_level_training_set$initialCrabCount)

match_level_training_set %>%
  group_by(leader_won_drag) %>%
  summarise(across(ends_with("_gap"), mean, na.rm = TRUE))


gap_cols <- match_level_training_set %>%
  select(ends_with("_gap")) %>%
  names()


walk(gap_cols, function(col) {
  formula <- as.formula(paste(col, "~ leader_won_drag"))
  fit     <- lm(formula, data = match_level_training_set)
  
  cat("\n========================================\n")
  cat("Feature:", col, "\n")
  cat("========================================\n")
  print(supernova(fit))
})


# =======================================================
# 2. Collect p-values for multiple comparison correction
# =======================================================
anova_summary <- map_df(gap_cols, function(col) {
  formula <- as.formula(paste(col, "~ leader_won_drag"))
  fit     <- lm(formula, data = match_level_training_set)
  tbl     <- supernova(fit)$tbl
  
  # First row is always the model/between-groups term
  model_row <- tbl[1, ]
  
  tibble(
    feature = col,
    SS      = model_row$SS,
    df      = model_row$df,
    MS      = model_row$MS,
    F_stat  = model_row$F,
    p_value = model_row$p
  )
}) %>%
  arrange(p_value) %>%
  mutate(
    p_adjusted   = p.adjust(p_value, method = "BH"),
    significant  = p_value    < 0.05,
    sig_adjusted = p_adjusted < 0.05
  )

cat("\n--- Summary Table (sorted by p-value) ---\n")
print(anova_summary, n = Inf)



test_fit <- lm(jungle_gold_gap ~ leader_won_drag, data = match_level_training_set)
supernova(test_fit)$tbl %>% names()
# =======================================================
# 3. Visualize F-statistics
# =======================================================
anova_summary %>%
  mutate(feature = fct_reorder(feature, F_stat)) %>%
  ggplot(aes(x = feature, y = F_stat, fill = sig_adjusted)) +
  geom_col() +
  scale_fill_manual(
    values = c("TRUE" = "steelblue", "FALSE" = "grey70"),
    labels = c("TRUE" = "Significant", "FALSE" = "Not Significant")
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    title    = "One-Way ANOVA: F-Statistics by Gap Feature",
    subtitle = "Significance after Benjamini-Hochberg correction",
    x        = NULL,
    y        = "F-Statistic",
    fill     = NULL
  )

# =======================================================
# 4. Two-Way ANOVA on significant features
# =======================================================
top_features <- anova_summary %>%
  filter(sig_adjusted) %>%
  pull(feature)

cat("\n--- Two-Way ANOVA (significant features x initialCrabCount) ---\n")

walk(top_features, function(col) {
  formula <- as.formula(paste(col, "~ leader_won_drag * factor(initialCrabCount)"))
  fit     <- lm(formula, data = match_level_training_set)
  
  cat("\n========================================\n")
  cat("Feature:", col, "\n")
  cat("========================================\n")
  print(supernova(fit, type = 3))  # Type III SS for interaction models
})

# =======================================================
# 5. Deeper dive — supernova table for your most 
#    significant feature with group means and plot
# =======================================================
top_feature <- anova_summary$feature[1]
cat(paste0("\n--- Deep Dive: ", top_feature, " ---\n"))

top_formula <- as.formula(paste(top_feature, "~ leader_won_drag"))
top_fit     <- lm(top_formula, data = match_level_training_set)

# Full supernova table
print(supernova(top_fit))

# Group means
match_level_training_set %>%
  group_by(leader_won_drag) %>%
  summarise(
    n    = n(),
    mean = mean(.data[[top_feature]], na.rm = TRUE),
    sd   = sd(.data[[top_feature]],  na.rm = TRUE),
    se   = sd / sqrt(n)
  ) %>%
  print()

# Violin + boxplot for the top feature
match_level_training_set %>%
  ggplot(aes(x = leader_won_drag, y = .data[[top_feature]], fill = leader_won_drag)) +
  geom_violin(alpha = 0.4, trim = FALSE) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.7) +
  scale_fill_manual(values = c("Leader_Drag" = "steelblue", "Trailer_Drag" = "tomato")) +
  theme_minimal() +
  labs(
    title = paste("Distribution of", top_feature, "by Dragon Outcome"),
    x     = NULL,
    y     = top_feature,
    fill  = NULL
  )




eta_results <- map_df(gap_cols, function(col) {
  formula <- as.formula(paste(col, "~ leader_won_drag"))
  fit     <- lm(formula, data = match_level_training_set)
  tbl     <- supernova(fit)$tbl
  
  # Eta-squared = SS_model / SS_total
  ss_model <- tbl[1, "SS"]
  ss_total <- tbl[nrow(tbl), "SS"]
  eta_sq   <- ss_model / ss_total
  
  tibble(
    feature    = col,
    F_stat     = tbl[1, "F"],
    p_value    = tbl[1, "p"],
    eta_sq     = eta_sq,
    p_adjusted = p.adjust(tbl[1, "p"], method = "BH")
  )
}) %>%
  arrange(desc(eta_sq))

print(eta_results, n = Inf)


sig_features <- anova_summary %>% filter(sig_adjusted) %>% pull(feature)

assumption_results <- map_df(sig_features, function(col) {
  formula <- as.formula(paste(col, "~ leader_won_drag"))
  fit     <- aov(formula, data = match_level_training_set)
  
  # Shapiro-Wilk on residuals (normality)
  sw  <- shapiro.test(residuals(fit))
  
  # Levene's test (homogeneity of variance)
  lev <- car::leveneTest(formula, data = match_level_training_set)
  
  tibble(
    feature        = col,
    shapiro_W      = round(sw$statistic, 4),
    shapiro_p      = round(sw$p.value,   4),
    levene_F       = round(lev[1, "F value"], 4),
    levene_p       = round(lev[1, "Pr(>F)"],  4),
    normality_ok   = sw$p.value  > 0.05,
    equal_var_ok   = lev[1, "Pr(>F)"] > 0.05
  )
})

print(assumption_results, n = Inf)

# For any feature that fails equal variance, run Welch ANOVA as correction
welch_needed <- assumption_results %>% filter(!equal_var_ok) %>% pull(feature)

walk(welch_needed, function(col) {
  formula <- as.formula(paste(col, "~ leader_won_drag"))
  result  <- oneway.test(formula, data = match_level_training_set, var.equal = FALSE)
  cat("\nWelch ANOVA for", col, "— F =", round(result$statistic, 4),
      ", p =", round(result$p.value, 4), "\n")
})


match_level_training_set %>%
  mutate(crab = factor(initialCrabCount)) %>%
  group_by(leader_won_drag, crab) %>%
  summarise(mean_val = mean(.data[[top_feature]], na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = crab, y = mean_val,
             color = leader_won_drag, group = leader_won_drag)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Leader_Drag" = "steelblue",
                                "Trailer_Drag" = "tomato")) +
  theme_minimal() +
  labs(
    title    = paste("Interaction Plot:", top_feature),
    subtitle = "Non-parallel lines indicate a significant interaction effect",
    x        = "Initial Crab Count",
    y        = paste("Mean", top_feature),
    color    = NULL
  )
