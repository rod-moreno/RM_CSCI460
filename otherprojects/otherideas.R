library(supernova)
library(tidyverse)
library(httr2)
library(jsonlite)
library(tidyverse)
library(tidymodels)
library(xgboost)
library(vip)
library(LiblineaR)
library(themis)
library(caret)
champion_profiles %>%
  filter(teamPosition == "MIDDLE") %>%
  ggplot() + 
  geom_point(aes(x = games_played, y = base_stomp, shape = teamPosition, color = teamPosition)) + 
  theme_classic()

testlm <- lm(base_stomp ~ games_played * teamPosition, data = champion_profiles)

summary(testlm)

supernova(testlm)



champion_profiles %>%
  ggplot(aes(x = games_played, y = base_stomp)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE) +  # mean trend
  facet_wrap(~ teamPosition) +
  theme_minimal() +
  labs(title = "Base Stomp vs Games Played by Role",
       x = "Games Played", y = "Lane Stomp Rate")

champion_profiles %>%
  filter(teamPosition == "UTILITY") %>%
  arrange(desc(games_played))


prox_model <- lm(jungle_proximity_pct ~ teamId * teamPosition, 
                 data = processed_data %>% 
                   mutate(teamId = as.factor(teamId)))
supernova(prox_model)

processed_data %>%
  mutate(teamId = as.factor(teamId)) %>%
  group_by(teamId, teamPosition) %>%
  summarise(
    mean_proximity = mean(jungle_proximity_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = teamId, 
              values_from = mean_proximity,
              names_prefix = "team_") %>%
  mutate(blue_minus_red = team_100 - team_200)


roam_model <- lm(roaming_pct ~ teamId * teamPosition,
                 data = processed_data %>%
                   mutate(teamId = as.factor(teamId)))
supernova(roam_model)

#Keep
processed_data %>%
  filter(earliestDragonTakedown > 0) %>%
  ggplot() + 
  geom_histogram(aes(x = earliestDragonTakedown), color = "black", fill = "#c2dbff") + 
  facet_wrap(~teamId) + 
  theme_classic() + 
  labs(title = "Average First Dragon Takedown Time ", 
       subtitle = "Data split into Blue(100) and Red(200) Teams")

#Keep 
champion_profiles %>%
  group_by(teamPosition) %>%
  summarise(avg_games = mean(games_played, na.rm = TRUE)) %>%
  ggplot(aes(x = teamPosition, y = avg_games, fill = teamPosition)) + 
  geom_col() + 
  theme_classic()
#Keep
processed_data %>% 
  filter(earliestDragonTakedown > 0) %>%
  ggplot() + 
  geom_boxplot(aes(y = earliestDragonTakedown)) + 
  facet_grid(~teamId) + 
  theme_classic()

processed_data %>%
  ggplot() + 
  geom_col(aes(x = as.factor(teamId), y = teamRiftHeraldKills, fill = as.factor(teamId))) + 
  theme_classic()


rm(test)

#Keep
cor.test(processed_data$early_cs_per_min, processed_data$teamRiftHeraldKills)
