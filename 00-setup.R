
library(dplyr)
library(readr)
library(tibble)
library(stringr)
library(tidyr)
library(ggplot2)

library(misterchiefdata) # pak::pak('tonyelhabr/misterchiefdata')

brackets <- read_halo_brackets()
players <- read_halo_players() %>% 
  filter(!is.na(.data$player_url)) %>% 
  mutate(across(id, ~str_replace_all(.x, ' ', '_')))
player_tourneys <- players %>%
  unnest(tournaments)
teams <- read_halo_teams()

brackets %>% 
  select(prize_pool) %>% 
  mutate(
    z = prize_pool %>% 
      str_replace_all('(^.*\\$)(.*)(\\sUSD.*$)', '\\2') %>% 
      str_remove_all('\\,') %>% 
      as.integer()
  )

brackets_augmented <- brackets %>%
  filter(tier %>% str_detect('FFA|Show Match', negate = TRUE)) %>% 
  transmute(
    url,
    date = start_date,
    ## todo: player age at time of tournament
    game = game_version,
    n_teams = number_of_teams,
    log_prize = prize_pool %>% 
      str_replace_all('(^.*\\$)(.*)(\\sUSD.*$)', '\\2') %>% 
      str_remove_all('\\,') %>% 
      as.integer() %>% 
      log(),
    is_qualifier = tier %>% str_detect('Qualifier'),
    is_weekly = tier %>% str_detect('Weekly'),
    tier = tier %>% str_remove_all('(Qualifier|Weekly)\\s\\(|\\)'),
    bracket_series_results,
    pool_series_results
  ) %>% 
  mutate(
    across(tier, ~factor(.x, c('S', 'A', 'B', 'C')))
  ) %>% 
  filter(!is.na(log_prize))

# rec_prize <- brackets_augmented %>% 
#   recipe(log_prize ~ game + n_teams + is_qualifier + is_weekly + tier, data = .) %>% 
#   step_impute_linear(n_teams, impute_with = all_predictors())
# wf_prize <- workflow(
#   rec_prize,
#   rand_forest(mode = 'regression', engine = 'ranger')
# )
# fit_prize <- wf_prize %>% fit(brackets_augmented)
# preds_prize <- fit_prize %>% 
#   broom::augment(brackets_augmented)
# preds_prize %>% 
#   mutate(
#     .resid = .pred - log_prize
#   ) %>% 
#   arrange(desc(abs(.resid)))
# 
# preds_prize %>% 
#   ggplot() +
#   aes(x = log_prize, y = .pred) +
#   geom_point(aes(color = game, size = lubridate::year(date))) +
#   geom_abline(aes(intercept = 0, slope = 1)) +
#   coord_cartesian(xlim = c(0, 15), y = c(0, 15))

series <- brackets_augmented %>% 
  pivot_longer(
    matches('_series_results')
  ) %>% 
  unnest(value)
