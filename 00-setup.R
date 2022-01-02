
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(tidyr)
library(ggplot2)
# pak::pak('tonyelhabr/misterchiefdata')
library(misterchiefdata) 

.clean_team_url <- function(team_url, team) {
  coalesce(
    team_url, 
    sprintf('https://liquipedia.net/halo/%s', team %>% tolower() %>% str_replace_all(' ', '_'))
  )
}

.clean_team <- function(team) {
  team %>% tolower() %>% str_replace_all(' ', '')
}

.add_team_cols <- function(data) {
  data %>% 
    mutate(
      team_url_is_guess = is.na(team_url),
      across(team_url, ~.clean_team_url(.x, team)),
      across(team, .clean_team)
    )
}

brackets <- read_halo_brackets()

teams <- read_halo_teams() %>% .add_team_cols()

bracket_teams <- brackets %>% 
  select(url, teams) %>% 
  filter(
    map_int(teams, nrow) > 0
  ) %>% 
  unnest(teams) %>% 
  .add_team_cols()
bracket_teams

brackets_augmented <- brackets %>%
  filter(tier %>% str_detect('FFA|Show Match', negate = TRUE)) %>% 
  transmute(
    url,
    event_name,
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

players <- read_halo_players() %>% 
  filter(!is.na(.data$player_url)) %>% 
  mutate(across(id, ~str_replace_all(.x, ' ', '_')))

player_tourneys <- players %>%
  unnest(tournaments) %>% 
  mutate(
    across(team, .clean_team)
  ) %>% 
  left_join(
    bracket_teams,
    by = c('url', 'team')
  )

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

teams_from_brackets <- bracket_teams %>% 
  distinct(team, team_url) %>% 
  arrange(team)

# teams_from_brackets %>% 
#   count(team, sort = TRUE) %>% 
#   count(n)
# teams_from_brackets %>% 
#   filter(team == 'denialesports')

.join_bracket_teams <- function(data, .side) {
  team_col <- sprintf('%s_team', .side)
  team_url_1_sym <- sym(sprintf('%s_team_url_1', .side))
  team_url_2_sym <- sym(sprintf('%s_team_url_2', .side))
  data %>% 
    mutate(
      across(all_of(team_col), .clean_team)
    ) %>% 
    left_join(
      bracket_teams %>%
        select(url, !!sym(team_col) := team, !!team_url_1_sym := team_url),
      by = c('url', team_col)
    ) %>%
    left_join(
      teams_from_brackets %>% 
        distinct(!!sym(team_col) := team, !!team_url_2_sym := team_url),
      by = team_col
    ) %>% 
    mutate(
      !!sym(sprintf('%s_team_url', .side)) := coalesce(!!team_url_1_sym, !!team_url_2_sym)
    ) %>% 
    select(-c(!!team_url_1_sym, !!team_url_2_sym)) %>% 
    group_by(url, series_type, series_index) %>% 
    slice_head(n = 1) %>% 
    ungroup()
}

series <- brackets_augmented %>% 
  pivot_longer(
    matches('_series_results')
  ) %>% 
  unnest(value) %>% 
  .join_bracket_teams('home') %>% 
  .join_bracket_teams('away') %>% 
  arrange(desc(date), series_type, series_index)
series
