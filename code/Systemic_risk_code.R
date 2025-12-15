library(dplyr)
library(tidyr)
library(writexl)
etf  <- read.csv("etf_clean_returns.csv")
cond <- read.csv("conditioning_factors.csv")
etf$Date  <- as.Date(etf$Date)
cond$Date <- as.Date(cond$Date)
cond_keep <- c("EZA", "IEMG", "XLY", "IYT", "PICK", "RTH")
cond <- cond %>% filter(Ticker %in% cond_keep)
etf <- etf %>%
  group_by(Date, Ticker) %>%
  summarise(log_ret = mean(log_ret), .groups = "drop")
cond <- cond %>%
  group_by(Date, Ticker) %>%
  summarise(log_ret = mean(log_ret), .groups = "drop")
etf_w  <- etf  %>% pivot_wider(names_from = Ticker, values_from = log_ret)
cond_w <- cond %>% pivot_wider(names_from = Ticker, values_from = log_ret)
data_all <- inner_join(etf_w, cond_w, by = "Date")
etf_names  <- colnames(etf_w)[-1]
cond_names <- colnames(cond_w)[-1]
tail_beta_exact <- function(X, Y, k) {
  df <- na.omit(data.frame(X = X, Y = Y))
  N  <- nrow(df)
  if (N <= k + 5) return(NA_real_)
  qx <- sort(df$X, decreasing = TRUE)[k]
  qy <- sort(df$Y, decreasing = TRUE)[k]
  U <- sum(df$X > qx & df$Y > qy)
  if (U == 0) return(NA_real_)
  (N * k) / U
}
etf_info <- read.csv("etf_clean_returns.csv") %>%
  select(Ticker, Sector) %>%
  distinct()
results <- list()
for (k in c(150, 200)) {
  systemic_mat <- matrix(
    NA,
    nrow = length(etf_names),
    ncol = length(cond_names),
    dimnames = list(etf_names, cond_names)
  )
  for (e in etf_names) {
    for (c in cond_names) {
      systemic_mat[e, c] <- tail_beta_exact(data_all[[e]], data_all[[c]], k)
    }
  }
  systemic_df <- as.data.frame(systemic_mat)
  final_table <- systemic_df %>%
    mutate(ETF = rownames(systemic_df)) %>%
    left_join(etf_info, by = c("ETF" = "Ticker")) %>%
    rowwise() %>%
    mutate(Average = mean(c_across(all_of(cond_names)), na.rm = TRUE)) %>%
    ungroup() %>%
    select(Sector, ETF, all_of(cond_names), Average)
  results[[paste0("Systemic_Risk_k", k)]] <- final_table
}
write_xlsx(results, "Systemic_Risk_TailBeta_k150_k200.xlsx")
