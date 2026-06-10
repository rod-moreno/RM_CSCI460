# Define your 5v5 team drafts
blue_picks <- c(top="Nidalee", jng="Aatrox", mid="Akali", adc="Varus", sup="Poppy")
red_picks  <- c(top="Kled", jng="Nasus", mid="Anivia", adc="Senna", sup="Alistar")

# Run it!
testmatch <- simulate_final_draft(red_picks, blue_picks, scuttle_count = 1)

zero_match[1, ] <- 0
zero_match$initialCrabCount <- 1L
baseline_prob <- predict(final_dragon_fit, zero_match, type = "prob")

baseline_prob
