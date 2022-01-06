
source('00-setup.R')

## Need to expand to find all hypothetical matchups
series_players_expanded <- full_join(
  .select_series_side('home', expand = TRUE) %>% select(url, series_type, series_index, home_id, home_idx),
  .select_series_side('away', expand = TRUE) %>% select(url, series_type, series_index,away_id, away_idx), 
  by = c('url', 'series_type', 'series_index')
) %>%
  select(
    url,
    series_type,
    series_index,
    home_id,
    away_id,
    home_idx,
    away_idx
  ) %>% 
  right_join(
    .grid %>%
      inner_join(
        series %>%
          select(
            url,
            date,
            series_type,
            series_index,
            home_team,
            away_team,
            home_w,
            away_w
          ),
        by = c('url', 'series_index', 'series_type')
      ) %>% 
      crossing(home_idx = .team_idx, away_idx = .team_idx),
    , by = c('url', 'series_type', 'series_index', 'home_idx', 'away_idx')
  ) %>% 
  select(
    url,
    date,
    series_type,
    series_index,
    home_team,
    away_team,
    home_w,
    away_w,
    home_id,
    away_id,
    home_idx,
    away_idx
  ) %>% 
  arrange(date, series_type, series_index, home_idx, away_idx)
series_players_expanded

## Need at least on player to be non-NA in order for us to compute a useful elo
series_players_expanded_filt <- series_players_expanded %>%
  filter(url == 'https://liquipedia.net/halo/Halo_Championship_Series/2021/Kickoff_Major') %>% 
  filter(!(is.na(home_id) & is.na(away_id)))
# series_players_expanded %>% view() # %>% skimr::skim()

p_ij <- function(r_i, r_j, xi = 20) {
  1 / (1 + 10 ^ ((r_j - r_i) / xi))
}

compute_elo <- function(r_i, ..., k = 20, w_ij = 1) {
  p <- p_ij(r_i = r_i, ...)
  r_i + k * (w_ij - p)
}

do_elo <- function(r_0 = 1500, k = 20, xi = 20, .verbose = TRUE, path = glue::glue('elo-r_0={r_0}-k={k}-xi={xi}.rds')) {
  
  r_0 <- 1500; k <- 20; xi <- 20; .verbose <- TRUE; path = glue::glue('elo-r_0={r_0}-k={k}-xi={xi}.rds')
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
    mutate(elo = !!r_0, prev_elo = !!r_0, i = 0)
  
  .grid_filt <- .grid %>% filter(url == 'https://liquipedia.net/halo/Halo_Championship_Series/2021/Kickoff_Major')
  # for(i in seq_len(nrow(series_players_expanded_filt))) {
  # for(i in 1:100) {
  for(i in seq_len(nrow(.grid_filt))) {
    i <- 1
    grid_i <- .grid_filt %>% slice(i)
    series_players_i <- series_players_expanded_filt %>% 
      semi_join(grid_i, by = c('url', 'series_type', 'series_index'))
    
    for(j in seq_len(nrow(series_players_i))) {
      # j <- 1

      series_players_ij <- series_players_i %>% slice(j)
      
      series_elos_init <- elos_init %>% 
        semi_join(series_players_ij %>% distinct(id), by = 'id') %>% 
        group_by(id) %>% 
        slice_max(i, n = 1) %>% 
        ungroup()
      
      slice_elo_id <- function(.side) {
        col <- sprintf('%s_id', .side)
        if(is.na(series_players_ij[[col]])) {
          tibble(
            id = 'missing',
            elo = !!r_0,
            prev_elo = !!r_0
          )
        } else {
          series_elos_init %>% filter(id == series_players_ij[[col]])
          # ids <- series_players_i %>% distinct(!!sym(col))
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
        series_players_ij$home_w > series_players_ij$away_w,
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
      
      if(.verbose) { #  & (i %% 100) == 0) {
        cat(glue::glue('{i}: Post-match elo for {elo_id_1$id}: {round(elo_id_1$elo, 0)} -> {round(res_1, 0)} ({sprintf("%+d", round(res_1 - elo_id_1$elo, 0))})'), sep = '\n')
        cat(glue::glue('{i}: Post-match elo for {elo_id_2$id}: {round(elo_id_2$elo, 0)} -> {round(res_2, 0)} ({sprintf("%+d", round(res_2 - elo_id_2$elo, 0))})'), sep = '\n')
      }

      series_elos_ij <- tibble(
        j = rep(j, 2),
        id = c(elo_id_1$id, elo_id_2$id),
        elo = c(res_1, res_2),
        prev_elo = c(elo_id_1$elo, elo_id_2$elo),
        p = c(p, 1 - p)
      )

      if(j == 1) {
        series_elos_i <- series_elos_ij
      } else {
        series_elos_i <- bind_rows(
          series_elos_i,
          series_elos_ij
        )
      }
      series_elos_i_agg <- series_elos_i %>% 
        group_by(id) %>% 
        summarize(across(c(elo, prev_elo), mean)) %>% 
        ungroup()
    }
    elos_init <- 
    
    # elos_init <- elos_init %>% 
    #   mutate(
    #     across(
    #       prev_elo,
    #       ~case_when(
    #         id %in% c(elo_id_1$id, elo_id_2$id) ~ elo,
    #         TRUE ~ .x
    #       )
    #     ),
    #     across(
    #       elo,
    #       ~case_when(
    #         id == elo_id_1$id ~ res_1,
    #         id == elo_id_2$id ~ res_2,
    #         TRUE ~ .x
    #       )
    #     ),
    #     i = case_when(
    #       id %in% c(elo_id_1$id, elo_id_2$id) ~ as.integer(i),
    #       TRUE ~ NA_integer_
    #     ),
    #     url = series_players_i$url,
    #     series_type = series_players_i$series_type,
    #     series_index = series_players_i$series_index,
    #     home_idx = series_players_i$home_idx,
    #     away_idx = series_players_i$away_idx,
    #     p = case_when(
    #       id == elo_id_1$id ~ p_1,
    #       id == elo_id_2$id ~  1 - p_1,
    #       TRUE ~ NA_real_
    #     )
    #   )
    elos_init <- elos_init %>% 
      bind_rows(series_elos_i_agg)
    
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
path_elos <- file.path('elos_recent.rds')
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
  arrange(series_type, series_index, home_idx, away_idx) %>% 
  mutate(
    across(c(home_idx, away_idx), as.integer)
  ) %>% 
  inner_join(series_players_expanded_filt) %>% 
  arrange(date, series_type, series_index, home_idx, away_idx) %>% 
  group_by(id) %>% 
  mutate(
    rn = row_number()
  ) %>% 
  ungroup() %>% 
  ggplot() +
  aes(x = rn, y = elo) +
  geom_point() +
  geom_line(aes(group = id))
