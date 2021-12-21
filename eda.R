
library(tidyverse)
library(nflreadr)
library(arrow)
library(tidyr)

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
    across(date, ~coalesce(.x, start_date))
  ) %>% 
  select(url, date, number_of_teams, liquipedia_tier, series, series_results)
one_bracket <- brackets_augmented %>% 
  filter(url == 'https://liquipedia.net/halo/Halo_Championship_Series/2021/Kickoff_Major/North_America')

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
series
long_series <- bind_rows(
  series %>% 
    transmute(
      url,
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
  filter(side == 'home') %>% 
  inner_join(
    teams %>% distinct(team),
    by = 'team'
  ) %>% 
  inner_join(
    players %>% select(id, url, team),
    by = c('url', 'team')
  ) %>%
  select(url, series_index, id, series_w, w, l) %>% 
  mutate(
    value = w - l,
    indicator = ifelse(series_w, 1, -1)
  ) %>% 
  select(url, series_index, id, indicator, value)

df %>% 
  filter(url == 'https://liquipedia.net/halo/Halo_Championship_Series/2021/Kickoff_Major/North_America')

df %>% 
  pivot_wider(
    names_from = id,
    values_from = indicator,
    values_fill = 0L
  )

series_players <- long_series %>% 
  left_join(teams %>% select(team, team_url), by = 'team') %>% 
  select(-team_url) %>% 
  left_join(
    players %>% select(id, url, team),
    by = c('url', 'team')
  ) %>% 
  group_by(url, team, series_index)
series_players %>% 
  group_by(id) %>% 
  summarize(
    across(c(w, l), sum)
  ) %>% 
  ungroup() %>% 
  arrange(desc(w))

## Todo
