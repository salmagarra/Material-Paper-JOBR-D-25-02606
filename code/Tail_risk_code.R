library(tidyverse)
etf  <- read.csv("etf_clean_returns.csv")
p_values <- c(0.001, 0.002)
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
  ES_vals  <- if (alpha > 1) x_p_vals / (alpha - 1) else rep(NA_real_, length(p_values))
  Q25 <- quantile(losses, 0.75)
  Q50 <- quantile(losses, 0.50)
  tibble(
    alpha = alpha,
    x_p_0.001 = x_p_vals[1],
    ES_xp_0.001 = ES_vals[1],
    x_p_0.002 = x_p_vals[2],
    ES_xp_0.002 = ES_vals[2],
    ES_25 = mean(losses[losses > Q25]),
    ES_50 = mean(losses[losses > Q50])
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
final_evt_m150 <- run_evt(df, m = 150)
final_evt_m200 <- run_evt(df, m = 200)
write.csv(final_evt_m150, "outputs/Final_EVT_Table_m150.csv", row.names = FALSE)
write.csv(final_evt_m200, "outputs/Final_EVT_Table_m200.csv", row.names = FALSE)
