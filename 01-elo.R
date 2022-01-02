
source('00-setup.R')
# teams_by_tournament <- series %>% 
#   # filter(date == '2021-12-17') %>% 
#   filter(series_type == 'bracket') %>% 
#   distinct(url, series_index, home_team, home_url = home_team_url, away_team, away_url = away_team_url) %>% 
#   pivot_longer(
#     -c(url, series_index),
#     names_pattern = '(home|away)_(team|url)',
#     names_to = c('side', 'entity'),
#     values_to = 'value'
#   ) %>% 
#   mutate(
#     across(entity, ~ifelse(.x == 'url', 'team_url', .x))
#   ) %>% 
#   pivot_wider(
#     names_from = entity,
#     values_from = value
#   )
# 
# ## this is ok for the top 3 or so, but it gets inaccurate for losers bracket scenarios
# heuristic_places <- teams_by_tournament %>% 
#   group_by(url, team_url) %>% 
#   slice_max(series_index, n = 1, with_ties = FALSE) %>% 
#   ungroup() %>% 
#   arrange(url, desc(series_index)) %>% 
#   group_by(url) %>% 
#   mutate(
#     heuristic_place = rank(desc(series_index), ties.method = 'max')
#   ) %>% 
#   ungroup()

grid <- series %>% 
  distinct(url, series_type, series_index)
.select_series_side <- function(.side, expand = TRUE) {
  team_col <- sprintf('%s_team', .side)
  team_sym <- sym(team_col)
  id_sym <- sym(sprintf('%s_id', .side))
  place_sym <- sym(sprintf('%s_place', .side))
  idx_sym <- sym(sprintf('%s_idx', .side))
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
      grid %>% 
        # filter(url == 'https://liquipedia.net/halo/Halo_Championship_Series/2021/Kickoff_Major') %>% 
        crossing(!!idx_sym := 1L:4L)
    )
}

series_players <- full_join(
  .select_series_side('home', expand = FALSE),
  .select_series_side('away', expand = FALSE)
)

series_players_expanded <- full_join(
  .select_series_side('home', expand = TRUE),
  .select_series_side('away', expand = TRUE)
) %>% 
  right_join(
    grid %>% 
      inner_join(
        series %>% 
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
          ) 
      ) %>% 
      crossing(home_idx = 1L:4L, away_idx = 1L:4L)
  ) %>% 
  arrange(date, desc(series_type), series_index, home_idx, away_idx)

series_players_expanded_filt <- series_players_expanded %>% 
  filter(!is.na(home_id) | !is.na(away_id))

# series_players_expanded %>% 
#   filter(url == 'https://liquipedia.net/halo/Halo_Championship_Series/2021/Kickoff_Major') -> z # %>% skimr::skim()

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
  select(url, n_teams, n_places, team, place, place_is_guess)

# places <- emperical_places %>% 
#   filter(url == 'https://liquipedia.net/halo/Halo_Championship_Series/2021/Kickoff_Major') %>% 
#   left_join(
#     heuristic_places,
#     by = c('url', 'team')
#   ) %>% 
#   mutate(
#     place_is_guess = is.na(place),
#     across(place, ~coalesce(.x, heuristic_place))
#   ) %>% 
#   select(url, n_teams, team, team_url, place, place_is_guess)
# places
places %>% 
  filter(url == 'https://liquipedia.net/halo/Halo_Championship_Series/2021/Kickoff_Major') -> z

p_ij <- function(r_i, r_j, xi = 20) {
  1 / (1 + 10 ^ ((r_j - r_i) / xi))
}

compute_elo <- function(r_i, ..., k = 20, w_ij = 1) {
  p <- p_ij(r_i = r_i, ...)
  r_i + k * (w_ij - p)
}

do_elo <- function(r_0 = 1500, k = 20, xi = 20, .verbose = TRUE, path = glue::glue('elo-r_0={r_0}-k={k}-xi={xi}.rds')) {
  
  # r_0 <- 1500
  # k <- 20
  # xi <- 20
  if(file.exists(path)) {
    return(read_rds(path))
  }
  
  ids <- bind_rows(
    series_players_expanded_filt %>% 
      distinct(id = home_id),
    series_players_expanded_filt %>% 
      distinct(id = away_id)
  ) %>% 
    filter(!is.na(id)) %>% 
    distinct(id) %>% 
    arrange(id)
  
  elos_init <- ids %>% 
    mutate(elo = !!r_0, prev_elo = !!r_0)
  
  for(i in seq_len(nrow(series_players_expanded_filt))) {
  # for(i in 1:100) {
    # i <- 1
    series_players_i <- series_players_expanded_filt %>% slice(i)
    slice_elo_id <- function(.side) {
      col <- sprintf('%s_id', .side)
      if(is.na(series_players_i[[col]])) {
        tibble(
          id = 'missing',
          elo = !!r_0,
          prev_elo = !!r_0
        )
      } else {
        elos_init %>% filter(id == series_players_i[[col]])
      }
    }
    elo_id_1 <- slice_elo_id('home')
    elo_id_2 <- slice_elo_id('away')
    compute_elo_p <- partial(
      compute_elo,
      xi = xi,
      k = k,
      ... = 
    )
    
    elo_1 <- ifelse(is.na(elo_id_1$elo), r_0, elo_id_1$elo)
    elo_2 <- ifelse(is.na(elo_id_2$elo), r_0, elo_id_2$elo)
    p_1 <- p_ij(r_i = elo_1, r_j = elo_2, xi = xi)
    w <- ifelse(
      series_players_i$home_w > series_players_i$away_w,
      1,
      0
    )
    res_1 <- compute_elo_p(
      r_i = elo_1,
      r_j = elo_2,
      w_ij = w
    )
    
    res_2 <- compute_elo_p(
      r_j = elo_1,
      r_i = elo_2,
      w_ij = 1 - w
    )
    
    if(.verbose & (i %% 100) == 0) {
      cat(glue::glue('{i}: Post-match elo for {elo_id_1$id}: {round(elo_id_1$elo, 0)} -> {round(res_1, 0)} ({sprintf("%+d", round(elo_id_1$elo - res_1, 0))})'), sep = '\n')
      cat(glue::glue('{i}: Post-match elo for {elo_id_2$id}: {round(elo_id_2$elo, 0)} -> {round(res_2, 0)} ({sprintf("%+d", round(elo_id_2$elo - res_2, 0))})'), sep = '\n')
    }
    
    id_1 <- ifelse(
      !is.na(elo_id_1$id),
      elo_id_1$id,
      ''
    )
    
    id_2 <- ifelse(
      !is.na(elo_id_2$id),
      elo_id_2$id,
      ''
    )
    
    elos_init <- elos_init %>% 
      mutate(
        across(
          prev_elo,
          ~case_when(
            id %in% c(elo_id_1$id, elo_id_2$id) ~ elo,
            TRUE ~ .x
          )
        ),
        across(
          elo,
          ~case_when(
            id == elo_id_1$id ~ res_1,
            id == elo_id_2$id ~ res_2,
            TRUE ~ .x
          )
        ),
        url = series_players_i$url,
        series_type = series_players_i$series_type,
        series_index = series_players_i$series_index,
        home_idx = series_players_i$home_idx,
        away_idx = series_players_i$away_idx,
        p = case_when(
          id == elo_id_1$id ~ p_1,
          id == elo_id_2$id ~  1 - p_1,
          TRUE ~ NA_real_
        )
      )
    
    if(i == 1) {
      all_elos <- elos_init
    } else {
      all_elos <- bind_rows(
        all_elos,
        elos_init
      )
    }
    
  }
  
  elos_wide <- all_elos %>%
    filter(!is.na(p)) %>% 
    group_by(url, series_type, series_index, home_idx, away_idx) %>% 
    mutate(rn = row_number()) %>% 
    ungroup() %>% 
    pivot_wider(
      names_from = rn,
      values_from = c(id, elo, prev_elo, w, p)
    ) %>% 
    mutate(
      across(matches('id'), ~replace_na(.x, 'missing'))
    )
  
  rename_elo <- function(.i) {
    suffix <- sprintf('_%s$', .i)
    side_opp <- ifelse(.i == 1, 2, 1)
    suffix_opp <- sprintf('_%s$', side_opp)
    elos_wide %>% 
      rename_with(~str_replace(.x, suffix, ''), c(matches(suffix), -matches('^id_'))) %>% 
      rename_with(~str_replace(.x, suffix_opp, '_opp'), c(matches(suffix_opp), -matches('^id_')))
  }
  elos <- bind_rows(rename_elo(1), rename_elo(2))
}

elos <- do_elo(
  r_0 = 1500,
  k = 20,
  xi = 20
)
path_elos <- file.path('elos.rds')
write_rds(elos, path_elos)
elos %>% 
  slice_max(elo)
elos %>% 
  group_by(id = id_1) %>% 
  slice_max(elo, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  slice_max(elo, n = 10, with_ties = FALSE) %>% 
  select(id, elo, url)

elos %>% 
  rename(id = id_1) %>%
  filter(id == 'LethuL' | id_2 == 'LethuL') %>% 
  filter(url == 'https://liquipedia.net/halo/MLG/2011/Raleigh') %>% 
  arrange(desc(series_type), series_index, home_idx, away_idx) %>% 
  mutate(
    across(c(home_idx, away_idx), as.integer)
  ) %>% 
  inner_join(series_players_expanded_filt) %>% 
  arrange(date, desc(series_type), series_index, home_idx, away_idx) %>% 
  group_by(id) %>% 
  mutate(
    rn = row_number()
  ) %>% 
  ungroup() %>% 
  ggplot() +
  aes(x = rn, y = elo) +
  geom_point() +
  geom_line(aes(group = id))
