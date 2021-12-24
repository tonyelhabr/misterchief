
library(dplyr)
library(readr)
library(tibble)

library(arrow)
library(tidyr)
library(recipes)
library(parsnip)
library(tune)
library(dials)

# Reference: https://github.com/nflverse/nflreadr/blob/main/R/from_url.R
rds_from_url <- function(url) {
  con <- base::url(url)
  on.exit(close(con))
  load <- try(readRDS(con), silent = TRUE)
  
  if (inherits(load, 'try-error')) {
    warning(paste0('Failed to `readRDS()` from <', url, '>'), call. = FALSE)
    return(tibble::tibble())
  }
  tibble::as_tibble(load)
}


parquet_from_url <- function(url){
  load <- try(curl::curl_fetch_memory(url), silent = TRUE)
  
  if (load$status_code != 200) {
    warning(
      paste0(
        'HTTP error',
        load$status_code,
        ' while retrieving data from <',
        url,
        '> \n Returning request payload.'
      ),
      call. = FALSE
    )
    return(tibble::tibble())
  }
  
  arrow::read_parquet(load$content)
}

base_path <- 'https://github.com/tonyelhabr/halo-data/raw/master/data'
read_brackets <- function() {
  sprintf('%s/brackets.rds', base_path) %>% rds_from_url()
}
read_parquet <- function(x) {
  sprintf('%s/%s.parquet', base_path, x) %>% parquet_from_url()
}

read_players <- function() {
  read_parquet('players')
}

read_rosters <- function() {
  read_parquet('rosters')
}

read_teams <- function() {
  read_parquet('teams')
}

brackets <- read_brackets()
players <- read_players()
rosters <- read_rosters()
teams <- read_teams()

brackets_augmented <- brackets %>%
  mutate(
    across(date, ~ coalesce(.x, start_date))
  ) %>%
  select(
    url,
    date,
    ## todo: player age at time of tournament
    game_version,
    number_of_teams,
    liquipedia_tier,
    series,
    series_results
  )

.select_unnest <- function(col) {
  col <- enquo(col)
  brackets_augmented %>% 
    filter(date >= lubridate::ymd('2015-01-01')) %>% 
    filter(
      liquipedia_tier %in% c(
        sprintf('%s-Tier', c('S', 'A', 'B', 'C')),
        'Qualifier (C-Tier)'
      )
    ) %>% 
    select(url, date, !!col) %>% 
    unnest(!!col)
}

series <- .select_unnest(series_results)

long_series <- bind_rows(
  series %>% 
    transmute(
      url,
      game_version,
      date,
      series_index,
      side = 'home',
      team = home_team,
      opponent = away_team,
      w = home_w,
      l = away_w
    ),
  series %>% 
    transmute(
      url,
      game_version,
      date,
      series_index,
      side = 'away',
      team = away_team,
      opponent = home_team,
      w = away_w,
      l = home_w
    )
) %>% 
  arrange(date, series_index, side) %>% 
  mutate(
    across(c(w, l), as.integer),
    series_w = ifelse(w > l, TRUE, FALSE)
  ) %>% 
  filter(!(is.na(w) | is.na(l)))
long_series

df <- long_series %>% 
  # filter(side == 'home') %>% 
  inner_join(
    teams %>% distinct(team),
    by = 'team'
  ) %>% 
  inner_join(
    players %>% select(id, url, team),
    by = c('url', 'team')
  ) %>%
  select(url, series_index, side, id, series_w, w, l) %>% 
  mutate(
    indicator = ifelse(side == 'home', 1, -1),
    value = w - l
  ) %>% 
  select(url, series_index, id, indicator, value)

ns <- df %>% 
  count(id, sort = TRUE)
ns
# series %>% 
#   filter(url == 'https://liquipedia.net/halo/Halo_Championship_Series/2021/Kickoff_Major/North_America')

df %>% 
  count(url, series_index) %>% 
  count(n, sort = TRUE)

df_filt <- df %>% 
  semi_join(
    ns %>% 
      filter(n >= 5L),
    by = 'id'
  )

df_filt %>% 
  group_by(id) %>% 
  summarize(
    n = n(),
    value = sum(value)
  ) %>% 
  ungroup() %>% 
  mutate(rate = value / n) %>% 
  arrange(desc(rate))

df_wide <- df_filt %>% 
  pivot_wider(
    names_from = id,
    values_from = indicator,
    values_fill = 0L
  )

## check
df_wide %>% 
  # select(value, LethuL) %>% 
  filter(LethuL != 0) %>% 
  filter(value < 0) %>% 
  slice(2) %>% 
  pivot_longer(
    -c(url, series_index, value),
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
  select(-c(url, series_index)) %>% 
  recipe(value ~ .)
rec

metset <- metric_set(rmse)
ctrl <- control_grid(
  verbose = TRUE,
  save_pred = FALSE,
  save_workflow = FALSE
)

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

#+ mets-lin ----
mets_lr <- tune_lr %>% collect_metrics()

#+ finalize-lin ----
params_best_lr <- tune_lr %>% select_best()
params_best_lr
# params_best_lr <- tibble(
#   penalty = 0.001,
#   mixture = 0.25,
#   .config = 'model'
# )
wf_best_lr <- wf_lr %>% finalize_workflow(params_best_lr)
# Ugh, not working for some reason...
# fit_trn_lr <- wf_lr %>% fit(df_trn)
# fit_lr <- wf_lr %>% fit(df_wide)

# Do this instead as a fix
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

#+ imp-lin-2, echo=F, include=F, eval=F
# Note that this is the same thing
coefs_lr <- fit_lr %>% 
  tidy(
    penalty = params_best_lr$penalty, 
    mixture = params_best_lr$mixture
  ) %>% 
  arrange(desc(abs(estimate)))

