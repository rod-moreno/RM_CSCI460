# =======================================================
# 1. Subset and reframe around crab count as the factor
# =======================================================
crab_analysis <- match_level_training_set %>%
  mutate(crab_group = factor(initialCrabCount, 
                             levels = c(0, 1, 2),
                             labels = c("No Scuttle", "One Scuttle", "Full Control")))

# Quick sample size check per group
table(crab_analysis$crab_group)

# =======================================================
# 2. One-Way ANOVA — do lane stats differ by crab dominance?
# =======================================================
# These are the features most relevant to your hypothesis
lane_cols <- c(
  "jungle_gold_gap", "jungle_stomp_gap",   # direct jungler impact
  "top_gold_gap",    "top_stomp_gap",       # top lane pressure
  "mid_gold_gap",    "mid_stomp_gap",       # mid lane pressure
  "adc_gold_gap",    "adc_stomp_gap",       # bot lane pressure
  "top_prox_gap",    "mid_prox_gap",        # jungle proximity by lane
  "adc_prox_gap",    "supp_prox_gap"        # bot side proximity
)

crab_anova_results <- map_df(lane_cols, function(col) {
  formula <- as.formula(paste(col, "~ crab_group"))
  fit     <- lm(formula, data = crab_analysis)
  tbl     <- supernova(fit)$tbl
  
  ss_model <- tbl[1, "SS"]
  ss_total <- tbl[nrow(tbl), "SS"]
  
  tibble(
    feature    = col,
    F_stat     = tbl[1, "F"],
    p_value    = tbl[1, "p"],
    eta_sq     = ss_model / ss_total,
    p_adjusted = p.adjust(tbl[1, "p"], method = "BH")
  )
}) %>%
  arrange(p_value) %>%
  mutate(
    significant  = p_value   < 0.05,
    sig_adjusted = p_adjusted < 0.05
  )

cat("--- ANOVA: Lane Stats by Scuttle Dominance ---\n")
print(crab_anova_results, n = Inf)

# =======================================================
# 3. Group means — where does the crab advantage show up?
# =======================================================
crab_means <- crab_analysis %>%
  group_by(crab_group) %>%
  summarise(across(all_of(lane_cols), mean, na.rm = TRUE), .groups = "drop")

cat("\n--- Group Means by Scuttle Dominance ---\n")
print(crab_means %>% pivot_longer(-crab_group, names_to = "feature", values_to = "mean") %>%
        pivot_wider(names_from = crab_group, values_from = mean), n = Inf)

# =======================================================
# 4. Post-hoc Tukey HSD — which groups actually differ?
# =======================================================
# With 3 levels (0/1/2 crabs) ANOVA tells you SOMETHING differs
# but Tukey tells you WHERE — is it 0 vs 2? 1 vs 2? All three?

cat("\n--- Tukey HSD Post-Hoc Tests ---\n")

sig_lane_cols <- crab_anova_results %>% 
  filter(sig_adjusted) %>% 
  pull(feature)

walk(sig_lane_cols, function(col) {
  formula <- as.formula(paste(col, "~ crab_group"))
  fit     <- aov(formula, data = crab_analysis)
  tukey   <- TukeyHSD(fit)
  
  cat("\n---", col, "---\n")
  print(tukey)
})

# =======================================================
# 5. Visualize — boxplots per lane stat by crab group
# =======================================================
crab_analysis %>%
  select(crab_group, all_of(lane_cols)) %>%
  pivot_longer(-crab_group, names_to = "feature", values_to = "value") %>%
  ggplot(aes(x = crab_group, y = value, fill = crab_group)) +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_fill_manual(values = c(
    "No Scuttle"   = "grey70",
    "One Scuttle"  = "steelblue",
    "Full Control" = "midnightblue"
  )) +
  facet_wrap(~ feature, scales = "free_y", ncol = 3) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title    = "Lane & Jungle Statistics by Scuttle Dominance",
    subtitle = "Positive values = gold leader advantage; Full Control = leader took both scuttles",
    x        = NULL,
    y        = "Gap Value (Leader - Trailer)",
    fill     = NULL
  )

# =======================================================
# 6. Proximity deep dive — your apprehension hypothesis
# =======================================================
# If the enemy jungler is more apprehensive when crab = 2,
# you should see higher proximity gaps (leader's jungler hovering
# more) AND lower stomp gaps for the enemy (they're avoiding fights)

cat("\n--- Proximity & Stomp Means by Crab Group ---\n")
crab_analysis %>%
  group_by(crab_group) %>%
  summarise(
    n                  = n(),
    mean_jng_stomp     = mean(jungle_stomp_gap, na.rm = TRUE),
    mean_top_prox      = mean(top_prox_gap,     na.rm = TRUE),
    mean_mid_prox      = mean(mid_prox_gap,     na.rm = TRUE),
    mean_adc_prox      = mean(adc_prox_gap,     na.rm = TRUE),
    mean_supp_prox     = mean(supp_prox_gap,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# Interaction plot — proximity gap by crab group and dragon outcome
# Tests whether jungler apprehension (low proximity gap) is specifically
# associated with trailer wins in full control games
crab_analysis %>%
  group_by(crab_group, leader_won_drag) %>%
  summarise(
    mean_jng_stomp = mean(jungle_stomp_gap, na.rm = TRUE),
    mean_top_prox  = mean(top_prox_gap,     na.rm = TRUE),
    mean_mid_prox  = mean(mid_prox_gap,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = starts_with("mean_"), 
               names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = crab_group, y = value,
             color = leader_won_drag, group = leader_won_drag)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Leader_Drag" = "steelblue", 
                                "Trailer_Drag" = "tomato")) +
  facet_wrap(~ metric, scales = "free_y") +
  theme_minimal() +
  labs(
    title    = "Jungle Pressure Metrics by Scuttle Dominance and Dragon Outcome",
    subtitle = "Tests whether scuttle dominance translates into lane pressure",
    x        = "Scuttle Dominance",
    y        = "Mean Gap Value",
    color    = NULL
  )


# =======================================================
# 1. Isolate the specific subset of interest
# =======================================================
full_control_trailer <- match_level_training_set %>%
  filter(initialCrabCount == 2, leader_won_drag == "Trailer_Drag")

full_control_leader <- match_level_training_set %>%
  filter(initialCrabCount == 2, leader_won_drag == "Leader_Drag")

full_control <- match_level_training_set %>%
  filter(initialCrabCount == 2)

cat("Full Control games:", nrow(full_control), "\n")
cat("  Leader wins dragon:", nrow(full_control_leader), "\n")
cat("  Trailer wins dragon:", nrow(full_control_trailer), "\n")

# =======================================================
# 2. ANOVA within Full Control games only
# — does top side pressure differentiate outcomes?
# =======================================================
top_side_cols <- c(
  "top_gold_gap", "top_stomp_gap", "top_prox_gap",  # direct top side
  "jungle_stomp_gap", "jungle_gold_gap",              # jungler strength
  "mid_prox_gap", "mid_gold_gap", "mid_stomp_gap",   # mid response
  "adc_roam_gap", "supp_roam_gap"                     # bot rotation
)

cat("\n--- ANOVA Within Full Control Games ---\n")
full_control_anova <- map_df(top_side_cols, function(col) {
  formula <- as.formula(paste(col, "~ leader_won_drag"))
  fit     <- lm(formula, data = full_control)
  tbl     <- supernova(fit)$tbl
  
  ss_model <- tbl[1, "SS"]
  ss_total <- tbl[nrow(tbl), "SS"]
  
  tibble(
    feature    = col,
    F_stat     = round(tbl[1, "F"],  4),
    p_value    = round(tbl[1, "p"],  4),
    eta_sq     = round(ss_model / ss_total, 4),
    p_adjusted = round(p.adjust(tbl[1, "p"], method = "BH"), 4)
  )
}) %>%
  arrange(p_value) %>%
  mutate(
    significant  = p_value   < 0.05,
    sig_adjusted = p_adjusted < 0.05
  )

print(full_control_anova, n = Inf)

# =======================================================
# 3. Means comparison — Full Control Leader vs Trailer wins
# =======================================================
cat("\n--- Mean Gaps: Full Control Games by Dragon Outcome ---\n")
full_control %>%
  group_by(leader_won_drag) %>%
  summarise(
    n                  = n(),
    # Top side
    mean_top_gold      = mean(top_gold_gap,      na.rm = TRUE),
    mean_top_stomp     = mean(top_stomp_gap,     na.rm = TRUE),
    mean_top_prox      = mean(top_prox_gap,      na.rm = TRUE),
    # Jungle
    mean_jng_stomp     = mean(jungle_stomp_gap,  na.rm = TRUE),
    mean_jng_gold      = mean(jungle_gold_gap,   na.rm = TRUE),
    # Mid response
    mean_mid_prox      = mean(mid_prox_gap,      na.rm = TRUE),
    mean_mid_gold      = mean(mid_gold_gap,      na.rm = TRUE),
    # Bot rotation
    mean_adc_roam      = mean(adc_roam_gap,      na.rm = TRUE),
    mean_supp_roam     = mean(supp_roam_gap,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-c(leader_won_drag, n), 
               names_to = "metric", values_to = "mean") %>%
  pivot_wider(names_from = leader_won_drag, values_from = mean) %>%
  mutate(difference = Leader_Drag - Trailer_Drag) %>%
  arrange(desc(abs(difference))) %>%
  print(n = Inf)

# =======================================================
# 4. Correlation structure within Trailer wins only
# — does top stomp correlate with mid prox loss?
# This tests the misdirection mechanism directly
# =======================================================
cat("\n--- Correlations Within Full Control Trailer_Drag Games ---\n")
cor_matrix <- full_control_trailer %>%
  select(all_of(top_side_cols)) %>%
  cor(use = "complete.obs")

# Focus on top side correlations specifically
cat("\nCorrelations with top_prox_gap in Trailer wins:\n")
cor_matrix["top_prox_gap", ] %>%
  sort(decreasing = TRUE) %>%
  round(3) %>%
  print()

cat("\nCorrelations with top_stomp_gap in Trailer wins:\n")
cor_matrix["top_stomp_gap", ] %>%
  sort(decreasing = TRUE) %>%
  round(3) %>%
  print()

# =======================================================
# 5. Does top side pressure predict dragon outcome
#    specifically in Full Control games?
#    Logistic regression to quantify the effect
# =======================================================
cat("\n--- Logistic Regression: Full Control Games ---\n")
full_control_lr <- glm(
  leader_won_drag ~ top_gold_gap + top_stomp_gap + top_prox_gap +
    jungle_stomp_gap + mid_prox_gap + adc_roam_gap + supp_roam_gap,
  data   = full_control %>%
    mutate(leader_won_drag = as.numeric(leader_won_drag == "Leader_Drag")),
  family = binomial(link = "logit")
)

summary(full_control_lr)

# Odds ratios — more interpretable than raw coefficients
cat("\n--- Odds Ratios: Full Control Games ---\n")
exp(cbind(OR = coef(full_control_lr), confint(full_control_lr))) %>%
  round(3) %>%
  print()

# =======================================================
# 6. Visualize — top side vs jungle stomp in Full Control
#    scatter colored by dragon outcome
# =======================================================
full_control %>%
  ggplot(aes(x = top_stomp_gap, y = jungle_stomp_gap, 
             color = leader_won_drag)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("Leader_Drag"  = "steelblue",
                                "Trailer_Drag" = "tomato")) +
  theme_minimal() +
  labs(
    title    = "Top Side vs Jungle Stomp Gap in Full Control Games",
    subtitle = "Quadrant II (top right, bottom left) = misdirection pattern",
    x        = "Top Lane Stomp Gap (Leader - Trailer)",
    y        = "Jungle Stomp Gap (Leader - Trailer)",
    color    = NULL
  )

# =======================================================
# 7. Quadrant analysis — quantify the misdirection pattern
# =======================================================
# If the misdirection hypothesis is correct, Trailer_Drag games
# should cluster in the quadrant where:
# top_stomp_gap is NEGATIVE (trailer winning top)
# jungle_stomp_gap is POSITIVE (leader still winning jungle overall)
# This would confirm top pressure is compensating for jungle disadvantage

cat("\n--- Quadrant Analysis: Full Control Games ---\n")
full_control %>%
  mutate(
    top_quadrant = if_else(top_stomp_gap    < 0, "Trailer Top Advantage", "Leader Top Advantage"),
    jng_quadrant = if_else(jungle_stomp_gap > 0, "Leader Jng Advantage",  "Trailer Jng Advantage")
  ) %>%
  count(leader_won_drag, top_quadrant, jng_quadrant) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  arrange(leader_won_drag, desc(n)) %>%
  print(n = Inf)


# =======================================================
# 1. Reframe stomp gaps as categorical
# =======================================================
full_control_cat <- full_control %>%
  mutate(
    top_stomp_cat = case_when(
      top_stomp_gap >  0 ~ "Leader Advantage",
      top_stomp_gap == 0 ~ "Even",
      top_stomp_gap <  0 ~ "Trailer Advantage"
    ),
    jungle_stomp_cat = case_when(
      jungle_stomp_gap >  0 ~ "Leader Advantage",
      jungle_stomp_gap == 0 ~ "Even",
      jungle_stomp_gap <  0 ~ "Trailer Advantage"
    ),
    mid_stomp_cat = case_when(
      mid_stomp_gap >  0 ~ "Leader Advantage",
      mid_stomp_gap == 0 ~ "Even",
      mid_stomp_gap <  0 ~ "Trailer Advantage"
    )
  )

# =======================================================
# 2. Chi-Square — does stomp category predict dragon outcome
#    in Full Control games?
# =======================================================
cat("\n--- Chi-Square: Top Stomp vs Dragon Outcome (Full Control) ---\n")
top_stomp_table <- table(full_control_cat$top_stomp_cat, 
                         full_control_cat$leader_won_drag)
print(top_stomp_table)
print(chisq.test(top_stomp_table))

cat("\n--- Chi-Square: Jungle Stomp vs Dragon Outcome (Full Control) ---\n")
jng_stomp_table <- table(full_control_cat$jungle_stomp_cat,
                         full_control_cat$leader_won_drag)
print(jng_stomp_table)
print(chisq.test(jng_stomp_table))

cat("\n--- Chi-Square: Mid Stomp vs Dragon Outcome (Full Control) ---\n")
mid_stomp_table <- table(full_control_cat$mid_stomp_cat,
                         full_control_cat$leader_won_drag)
print(mid_stomp_table)
print(chisq.test(mid_stomp_table))

# =======================================================
# 3. Contingency heatmap — joint distribution of
#    top stomp vs jungle stomp by dragon outcome
#    This is the clean replacement for the scatter plot
# =======================================================
full_control_cat %>%
  count(top_stomp_cat, jungle_stomp_cat, leader_won_drag) %>%
  group_by(leader_won_drag) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ggplot(aes(x = top_stomp_cat, y = jungle_stomp_cat, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(n, "\n(", pct, "%)")), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  facet_wrap(~ leader_won_drag) +
  theme_minimal() +
  labs(
    title    = "Joint Distribution: Top vs Jungle Stomp Advantage in Full Control Games",
    subtitle = "Cell values show count and % within each dragon outcome",
    x        = "Top Lane Stomp",
    y        = "Jungle Stomp",
    fill     = "% of Outcome"
  )

# =======================================================
# 4. Misdirection pattern quantified directly
#    Trailer advantage top + Leader advantage jungle
#    = the specific quadrant your hypothesis predicts
# =======================================================
cat("\n--- Misdirection Pattern Frequency (Full Control Games) ---\n")
full_control_cat %>%
  mutate(
    misdirection = top_stomp_cat    == "Trailer Advantage" & 
      jungle_stomp_cat == "Leader Advantage"
  ) %>%
  count(leader_won_drag, misdirection) %>%
  group_by(leader_won_drag) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

# =======================================================
# 5. Replace scatter with boxplots for the continuous
#    features (gold and proximity) that ARE continuous
# =======================================================
full_control %>%
  select(leader_won_drag, top_gold_gap, top_prox_gap, 
         jungle_gold_gap, mid_prox_gap) %>%
  pivot_longer(-leader_won_drag, names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = leader_won_drag, y = value, fill = leader_won_drag)) +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_fill_manual(values = c("Leader_Drag"  = "steelblue",
                               "Trailer_Drag" = "tomato")) +
  facet_wrap(~ metric, scales = "free_y") +
  theme_minimal() +
  labs(
    title    = "Continuous Gap Features by Dragon Outcome in Full Control Games",
    subtitle = "Gold and proximity gaps are genuinely continuous unlike stomp flags",
    x        = NULL,
    y        = "Gap Value",
    fill     = NULL
  )