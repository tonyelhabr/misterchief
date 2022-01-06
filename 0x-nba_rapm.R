
# some made up examples
df <- tibble::tribble(
  ~value, ~a, ~b,  ~c,  ~d,  ~e,  ~f,  ~g,  ~h,
      3L, 1L, NA,  1L,  NA,  NA,  NA,  NA,  NA,
      2L, NA, 1L,  NA,  1L,  NA,  NA,  NA,  NA,
     -3L, NA, NA,  NA,  NA,  1L,  NA,  1L,  NA,
     -2L, NA, NA,  NA,  NA,  NA,  1L,  NA,  1L,
      1L, 1L, NA,  NA,  NA,  NA,  NA,  NA, -1L,
      2L, NA, 1L,  NA,  NA, -1L, -1L,  NA,  NA,
     -3L, NA, NA, -1L,  1L,  NA,  NA,  1L,  NA,
     -1L, 1L, NA,  NA,  NA,  NA,  NA,  NA, -1L,
      2L, NA, 1L,  NA,  NA,  NA,  1L,  NA,  NA,
      1L, NA, NA, -1L,  NA,  1L,  NA,  NA,  NA,
      2L, NA, NA,  NA,  1L,  NA,  NA,  1L,  NA
  ) %>% 
  mutate(across(everything(), replace_na, 0L))

df <- tibble::tribble(
  ~value,  ~a,  ~b,  ~c,  ~d,  ~e,  ~f,  ~g,  ~h,
      3L,  1L,  NA,  1L,  NA,  NA,  NA,  NA,  NA,
      2L,  NA,  1L,  NA,  1L,  NA,  NA,  NA,  NA,
     -3L,  NA,  NA,  NA,  NA,  1L,  NA,  1L,  NA,
     -2L,  NA,  NA,  NA,  NA,  NA,  1L,  NA,  1L,
     -3L, -1L,  NA,  NA, -1L,  NA,  NA,  NA,  NA,
     -2L,  NA, -1L, -1L,  NA,  NA,  NA,  NA,  NA,
      3L,  NA,  NA,  NA,  NA, -1L,  NA,  NA, -1L,
      2L,  NA,  NA,  NA,  NA,  NA, -1L, -1L,  NA,
      2L,  NA,  1L,  NA,  1L,  NA,  1L,  NA,  1L,
      1L,  NA,  NA,  1L,  NA,  NA,  NA, -1L,  NA
  ) %>% 
  mutate(across(everything(), replace_na, 0L))

#A https://squared2020.com/2017/09/18/deep-dive-on-regularized-adjusted-plus-minus-i-introductory-example/
df <- tibble::tribble(
  ~a, ~b, ~c, ~d, ~e,  ~f,  ~g,  ~h,  ~i,  ~j, ~home, ~away, ~poss, ~netpp100,
  1L, 1L, 1L, NA, NA, -1L, -1L, -1L,  NA,  NA,   11L,    5L,   15L,        40,
  1L, 1L, 1L, NA, NA, -1L, -1L,  NA, -1L,  NA,   13L,    9L,    8L,       -25,
  1L, NA, NA, 1L, 1L, -1L,  NA,  NA, -1L, -1L,   20L,   10L,   10L,        60,
  1L, NA, NA, 1L, 1L,  NA,  NA, -1L, -1L, -1L,   26L,   16L,   17L,         0,
  NA, 1L, 1L, 1L, NA,  NA, -1L, -1L, -1L,  NA,   30L,   23L,   10L,       -30,
  NA, 1L, 1L, NA, 1L,  NA, -1L, -1L,  NA, -1L,   35L,   30L,   16L,     -12.5,
  1L, 1L, 1L, NA, NA,  NA, -1L,  NA, -1L, -1L,   39L,   35L,    8L,     -12.5,
  1L, 1L, 1L, NA, NA, -1L,  NA,  NA, -1L, -1L,   43L,   42L,   10L,       -30,
  NA, NA, 1L, 1L, 1L, -1L,  NA,  NA, -1L, -1L,   47L,   42L,    8L,        50,
  1L, NA, 1L, 1L, NA, -1L, -1L, -1L,  NA,  NA,   52L,   44L,   10L,        30,
  1L, NA, NA, 1L, 1L,  NA, -1L, -1L, -1L,  NA,   54L,   53L,   14L,       -50
  ) %>% 
  mutate(across(everything(), replace_na, 0L))
df %>% 
  mutate(
    net = home - away,
    across(net, ~coalesce(.x - lag(.x), .x)), 
    netpp100_check = (100 / poss) * net
  )


rates <- df %>% 
  pivot_longer(
    -value,
    names_to = 'id',
    values_to = 'indicator'
  ) %>% 
  filter(indicator != 0) %>% 
  mutate(
    across(value, ~.x * indicator)
  ) %>% 
  group_by(id) %>% 
  summarize(
    n = n(),
    value = sum(value)
  ) %>% 
  ungroup() %>% 
  mutate(rate = value / n) %>% 
  arrange(id)
rates

rec <- df %>% 
  recipe(value ~ .) %>% 
  step_intercept(value = 0) %>% 
  step_nzv(all_numeric_predictors())
rec %>% prep() %>% juice()

metset <- metric_set(rmse)
ctrl <- control_grid(
  verbose = TRUE,
  save_pred = FALSE,
  save_workflow = FALSE
)

naive_estimates <- rec %>% 
  workflow(
    linear_reg()
  ) %>% 
  fit(df) %>% 
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
  left_join(rates)
naive_estimates

