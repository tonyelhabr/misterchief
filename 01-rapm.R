
source('00-setup.R')
library(rsample)
library(recipes)
library(parsnip)
library(workflows)
library(tune)
library(dials)
library(yardstick)

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
      log_prize,
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

long_series_players <- long_series %>% 
  filter(url == 'https://liquipedia.net/halo/Halo_Championship_Series/2021/Kickoff_Major') %>% 
  inner_join(
    teams %>% distinct(team),
    by = 'team'
  ) %>% 
  inner_join(
    player_tourneys %>% 
      select(id, url, continent, team),
    by = c('url', 'team')
  ) %>%
  select(url, series_type, series_index, continent, side, id, series_w, w, l) %>% 
  mutate(
    value = w - l,
    # indicator = ifelse(side == 'home', 1, -1)
    indicator = ifelse(value > 0, 1, -1)
  ) %>% 
  select(url, series_type, series_index, continent, id, indicator, value)


n_by_id <- long_series_players %>% 
  count(id, sort = TRUE)
n_by_id

n_players_in_series <- long_series_players %>% 
  count(url, series_type, series_index)

## should at most be 8 here
n_players_in_series %>% 
  count(n, sort = TRUE)

long_series_players_na <- long_series_players %>% 
  filter(continent == 'North_America')

compute_rates <- function(data) {
  data %>% 
    group_by(id) %>% 
    summarize(
      n = n(),
      value = sum(value)
    ) %>% 
    ungroup() %>% 
    mutate(rate = value / n) %>% 
    arrange(desc(rate))
}

na_rates <- long_series_players_na %>% 
  compute_rates()
na_rates

net_players_by_series <- long_series_players_na %>% 
  group_by(url, series_type, series_index) %>% 
  summarize(
    indicator_sum = sum(indicator)
  ) %>% 
  ungroup()
net_players_by_series

## ideally it would be 0, but otherwise, we would hope for a symmetrical distribution
net_players_by_series %>% 
  count(indicator_sum)

long_series_players_net <- long_series_players_na # %>% 
  # semi_join(net_players_by_series %>% filter(indicator_sum == 0))

net_rates <- long_series_players_net %>% 
  compute_rates()

long_series_players_wide <- long_series_players_net %>% 
  pivot_wider(
    names_from = id,
    values_from = indicator,
    values_fill = 0L
  )

extra_cols <- c('url', 'series_type', 'series_index', 'continent')
target_col <- 'value'
## check
lethul <- long_series_players_wide %>% 
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
split <- long_series_players_wide %>% initial_split(strata = value)
long_series_players_trn <- split %>% training()
long_series_players_tst <- split %>% testing()
folds <- long_series_players_trn %>% vfold_cv(10)

rec <- long_series_players_wide %>% 
  recipe(value ~ .) %>% 
  step_rm(extra_cols)
rec

metset <- metric_set(rmse)
ctrl <- control_grid(
  verbose = TRUE,
  save_pred = FALSE,
  save_workflow = FALSE
)

rec %>% 
  workflow(
    linear_reg()
  ) %>% 
  fit(long_series_players_trn) %>% 
  broom::tidy() %>% 
  ## i assume NAs are due to collinearity?
  filter(!is.na(estimate)) %>% 
  transmute(
    id = term,
    rnk = row_number(desc(estimate)),
    prnk = percent_rank(estimate),
    estimate
  ) %>% 
  arrange(desc(estimate)) %>% 
  inner_join(net_rates) %>% 
  ggplot() +
  aes(x = prnk, y = value) +
  geom_point(aes(size = n))

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

fit_trn_lr <- wf_lr_fix %>% fit(long_series_players_trn) # use this to evaluate validation set.
fit_lr <- wf_lr_fix %>% fit(long_series_players_wide) # use this for true holdout data

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
  inner_join(net_rates) %>% 
  ggplot() +
  aes(x = prnk, y = value) +
  geom_point(aes(size = n))
