
library(dplyr)
library(readr)
library(tibble)
library(stringr)
library(tidyr)
library(ggplot2)

library(rsample)
library(recipes)
library(parsnip)
library(workflows)
library(tune)
library(dials)
library(yardstick)

library(misterchiefdata) # pak::pak('tonyelhabr/misterchiefdata')

brackets <- read_halo_brackets()
players <- read_halo_players() %>% 
  filter(!is.na(.data$player_url)) %>% 
  mutate(across(id, ~str_replace_all(.x, ' ', '_')))
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

.select_series_side <- function(.side = c('home', 'away')) {
  
  other_side <- switch(
    .side,
    'home' = 'away',
    'away' = 'home'
  )
  
  series %>% 
    transmute(
      url,
      date,
      game,
      n_teams,
      prize,
      tier,
      series_type,
      series_index,
      side = !!.side,
      team = !!sym(sprintf('%s_team', .side)),
      opponent = !!sym(sprintf('%s_team', other_side)),
      w = !!sym(sprintf('%s_w', .side)),
      l = !!sym(sprintf('%s_w', other_side))
    )
}

long_series <- bind_rows(
  .select_series_side('home'),
  .select_series_side('away')
) %>% 
  arrange(date, series_type, series_index, side) %>% 
  mutate(
    across(c(w, l), as.integer),
    series_w = ifelse(w > l, TRUE, FALSE)
  ) %>% 
  filter(!(is.na(w) | is.na(l)))
long_series

long_series %>% 
  filter(url == 'https://liquipedia.net/halo/Halo_Championship_Series/2021/Kickoff_Major')
long_series %>% 
  filter(tier == 'A') %>% 
  slice_max(date, n = 1) %>% 
  pull(url)

df <- long_series %>% 
  inner_join(
    teams %>% distinct(team),
    by = 'team'
  ) %>% 
  inner_join(
    players %>%
      unnest(tournaments) %>% 
      select(id, url, continent, team),
    by = c('url', 'team')
  ) %>%
  select(url, series_type, series_index, continent, side, id, series_w, w, l) %>% 
  mutate(
    value = w - l,
    indicator = ifelse(side == 'home', 1, -1)
    # indicator = ifelse(value > 0, 1, -1)
  ) %>% 
  select(url, series_type, series_index, continent, id, indicator, value)

ns <- df %>% 
  count(id, sort = TRUE)
ns

n_players_in_series <- df %>% 
  count(url, series_type, series_index)

## should at most be 8 here
n_players_in_series %>% 
  count(n, sort = TRUE)

rates <- df %>% 
  filter(continent == 'North_America') %>% 
  group_by(id) %>% 
  summarize(
    n = n(),
    value = sum(value)
  ) %>% 
  ungroup() %>% 
  mutate(rate = value / n) %>% 
  arrange(desc(rate))
rates

df_filt <- df %>% 
  semi_join(
    ns %>% 
      filter(n >= 5L),
    by = 'id'
  ) %>% 
  filter(continent == 'North_America')

net_players_by_series <- df_filt %>% 
  group_by(url, series_type, series_index) %>% 
  summarize(
    across(indicator, sum)
  ) %>% 
  ungroup()
net_players_by_series

## ideally it would be 0, but otherwise, we would hope for a symmetrical distribution
net_players %>% 
  count(indicator_by_series)



df_wide <- df_filt %>% 
  pivot_wider(
    names_from = id,
    values_from = indicator,
    values_fill = 0L
  )
df_wide

## check
extra_cols <- c('url', 'series_type', 'series_index', 'continent')
target_col <- 'value'
lethul <- df_wide %>% 
  # select(value, LethuL) %>% 
  filter(LethuL != 0) %>% 
  # filter(value < 0) %>%
  pivot_longer(
    -all_of(c(extra_cols, target_col)),
    names_to = 'id',
    values_to = 'indicator'
  ) %>% 
  filter(indicator != 0)


set.seed(42)
split <- df_wide %>% initial_split(strata = value)
df_trn <- split %>% training()
df_tst <- split %>% testing()
folds <- df_trn %>% vfold_cv(10)

rec <- df_wide %>% 
  recipe(value ~ .) %>% 
  step_rm(extra_cols)
rec

metset <- metric_set(rmse)
ctrl <- control_grid(
  verbose = TRUE,
  save_pred = FALSE,
  save_workflow = FALSE
)

wf_lr <- rec %>% 
  workflow(
    linear_reg()
  )
wf_lr %>% fit(df_trn)

wf_lr <- rec %>% 
  workflow(
    linear_reg(
      penalty = tune(),
      mixture = tune()
    ) %>% 
      set_engine('glmnet')
  )

grid_lr <- crossing(
  mixture = 0,
  # mixture = 0.5,
  penalty = 10 ^ seq(-6, 0.0001, length.out = 100)
)

set.seed(42)
tune_lr <- wf_lr %>% 
  tune_grid(
    folds,
    metrics = metset,
    control = ctrl,
    grid = grid_lr
  )
tune_lr %>% autoplot()

mets_lr <- tune_lr %>% collect_metrics()

params_best_lr <- tune_lr %>% select_best()
params_best_lr
# params_best_lr <- tibble(
#   penalty = 0.1,
#   mixture = 0.25,
#   .config = 'model'
# )
wf_best_lr <- wf_lr %>% finalize_workflow(params_best_lr)

wf_lr_fix <- rec %>%
  workflow(
    linear_reg(
      engine = 'glmnet',
      penalty = params_best_lr$penalty, 
      mixture = params_best_lr$mixture
    )
  )

fit_trn_lr <- wf_lr_fix %>% fit(df_trn) # use this to evaluate validation set.
fit_lr <- wf_lr_fix %>% fit(df_wide) # use this for true holdout data

# Note that this is the same thing
coefs_lr <- fit_lr %>% 
  tidy(
    penalty = params_best_lr$penalty, 
    mixture = params_best_lr$mixture
  ) %>% 
  transmute(
    id = term,
    rnk = row_number(desc(estimate)),
    prnk = percent_rank(estimate),
    estimate
  ) %>% 
  arrange(desc(estimate))
coefs_lr

coefs_lr %>% 
  inner_join(rates) %>% 
  ggplot() +
  aes(x = prnk, y = value) +
  geom_point(aes(size = n))
