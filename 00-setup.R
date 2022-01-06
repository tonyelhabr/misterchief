
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
  ## there's 2 series without results, causing the warnings
  ## actually, idk why there's still warnings. these case whens capture all cases
  mutate(
    across(
      c(home_w, away_w),
      ~case_when(
        .x == '' ~ NA_integer_,
        .x == 'W' ~ 3L,
        .x == 'L' | .x == 'FF' ~ 0L,
        TRUE ~ as.integer(.x)
      )
    ),
    across(series_type, ~factor(.x, c('pool', 'bracket')))
  ) %>% 
  arrange(desc(date), series_type, series_index)

.grid <- series %>% 
  distinct(url, series_type, series_index)

.team_idx <- 1L:4L
.select_series_side <- function(.side, expand = TRUE) {
  team_col <- sprintf('%s_team', .side)
  team_sym <- sym(team_col)
  id_sym <- sym(sprintf('%s_id', .side))
  place_sym <- sym(sprintf('%s_place', .side))
  idx_col <- sprintf('%s_idx', .side)
  idx_sym <- sym(idx_col)
  res <- series %>% 
    # filter(url == 'https://liquipedia.net/halo/Halo_Championship_Series/2021/Kickoff_Major') %>% 
    select(
      url,
      date,
      n_teams,
      series_type,
      series_index,
      home_team,
      away_team,
      home_w,
      away_w
    ) %>% 
    left_join(
      player_tourneys %>% 
        select(url, !!place_sym := place, !!team_sym := team, !!id_sym := id),
      by = c('url', team_col)
    )
  
  if(!expand) {
    return(res)
  }
  
  res %>% 
    group_by(url, series_type, series_index, !!team_sym) %>% 
    mutate(
      !!idx_sym := row_number(!!id_sym)
    ) %>% 
    ungroup() %>% 
    right_join(
      .grid %>% 
        # filter(url == 'https://liquipedia.net/halo/Halo_Championship_Series/2021/Kickoff_Major') %>% 
        crossing(!!idx_sym := .team_idx),
      by = c('url', 'series_type', 'series_index', idx_col)
    )
}

## Don't expand so we can come up with an estimate of the team's place.
## (The expanded df includes a lot of extra NAs that messes things up.)
series_players <- full_join(
  .select_series_side('home', expand = FALSE),
  .select_series_side('away', expand = FALSE)
)

.convert_group_place <- function(x) {
  if(is.na(x)) {
    return(NA_real_)
  }
  if(!str_detect(x, '[-]')) {
    return(as.double(x))
  }
  x1 <- x %>% str_remove('([-].*$)') %>% as.integer()
  x2 <- x %>% str_remove('(^.*[-])') %>% as.integer()
  (x1 + x2) / 2
}

emperical_places <- series_players %>% 
  filter(url == 'https://liquipedia.net/halo/Halo_Championship_Series/2021/Kickoff_Major') %>% 
  filter(series_type == 'bracket') %>% 
  distinct(url, series_index, n_teams, home_place, away_place, home = home_team, away = away_team) %>% 
  pivot_longer(
    c(home, away),
    names_to = 'team_side',
    values_to = 'team'
  ) %>% 
  rename(home = home_place, away = away_place) %>% 
  pivot_longer(
    c(home, away),
    names_to = 'place_side',
    values_to = 'place'
  ) %>% 
  filter(team_side == place_side) %>% 
  distinct(url, team, place, n_teams) %>% 
  mutate(
    place = map_dbl(place, .convert_group_place)
    # across(place, .convert_group_place)
  ) %>% 
  arrange(url, place)

places <- emperical_places %>% 
  mutate(place_is_guess = is.na(place)) %>% 
  group_by(url) %>% 
  mutate(
    n_places = n(),
    n_guess = sum(place_is_guess),
    place_sum = sum(place, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  # nest(data = c(team, place, place_is_guess)) %>% 
  mutate(
    theoretical_place_sum = map_int(n_places, ~reduce(1:.x, sum))
  ) %>% 
  # unnest(data) %>% 
  mutate(
    across(
      place,
      ~ifelse(
        is.na(.x),
        (theoretical_place_sum - place_sum) / n_guess,
        .x
      )
    )
  ) %>% 
  select(url, team, place, place_is_guess)

