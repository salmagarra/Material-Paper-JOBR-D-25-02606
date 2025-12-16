library(tidyverse)
p_values <- c(0.001, 0.002)
m_values <- c(150, 200)
input_file <- "etf_clean_returns.csv"
output_dir <- "outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
df <- read.csv(input_file)
required_cols <- c("Ticker", "Sector", "log_ret")
if (!all(required_cols %in% names(df))) stop("Missing required columns")
if (!is.numeric(df$log_ret)) stop("log_ret must be numeric")
compute_evt <- function(returns, m, p_values) {
  losses <- -returns
  losses <- losses[losses > 0]
  n <- length(losses)
  if (n <= m + 1) return(NULL)
  sorted_losses <- sort(losses)
  X_nm <- sorted_losses[n - m]
  tail_extreme <- sorted_losses[(n - m + 1):n]
  alpha <- 1 / mean(log(tail_extreme / X_nm))
  x_p_vals <- sapply(p_values, function(p) X_nm * (m / (n * p))^(1 / alpha))
  ES_vals <- if (alpha > 1) x_p_vals / (alpha - 1) else rep(NA_real_, length(p_values))
  Q25 <- quantile(losses, 0.75)
  Q50 <- quantile(losses, 0.50)
  tibble(
    alpha = alpha,
    x_p_0.001 = x_p_vals[1],
    ES_xp_0.001 = ES_vals[1],
    x_p_0.002 = x_p_vals[2],
    ES_xp_0.002 = ES_vals[2],
    ES_25 = mean(losses[losses > Q25]),
    ES_50 = mean(losses[losses > Q50]),
    n_obs = n,
    m_used = m,
    m_over_n = round(m / n, 6)
  )
}

run_evt <- function(df, m) {
  evt_results <- df %>%
    group_by(Ticker, Sector) %>%
    summarise(evt = list(compute_evt(log_ret, m, p_values)), .groups = "drop") %>%
    filter(!sapply(evt, is.null)) %>%
    unnest_wider(evt) %>%
    mutate(across(where(is.numeric), ~ round(.x, 6)))
  sector_avg <- evt_results %>%
    group_by(Sector) %>%
    summarise(across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE), 6)),
              .groups = "drop") %>%
    mutate(Ticker = paste0(Sector, "_AVERAGE"))
  global_avg <- evt_results %>%
    summarise(across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE), 6))) %>%
    mutate(Ticker = "GLOBAL_AVERAGE", Sector = "All")

  bind_rows(evt_results, sector_avg, global_avg) %>%
    arrange(factor(Sector, levels = unique(evt_results$Sector)), Ticker)
}
for (m in m_values) {
  res <- run_evt(df, m)
  write.csv(
    res,
    file.path(output_dir, paste0("Final_EVT_Table_m", m, ".csv")),
    row.names = FALSE
  )
}
