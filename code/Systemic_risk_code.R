library(tidyverse)
library(openxlsx)
etf <- read_csv("etf_clean_returns.csv")
factors <- read_csv("conditioning_factors.csv")
etf_clean <- etf %>%
  select(Date, Ticker, Sector, log_ret) %>%
  distinct()  # remove duplicate rows if any
factors_clean <- factors %>%
  select(Date, Ticker, log_ret) %>%
  distinct()
common_dates <- intersect(unique(etf_clean$Date),
                          unique(factors_clean$Date))
etf_clean <- etf_clean %>% filter(Date %in% common_dates)
factors_clean <- factors_clean %>% filter(Date %in% common_dates)
etf_clean <- etf_clean %>% distinct(Date, Ticker, .keep_all = TRUE)
factors_clean <- factors_clean %>% distinct(Date, Ticker, .keep_all = TRUE)
etf_wide <- etf_clean %>%
  pivot_wider(names_from = Ticker, values_from = log_ret)
factor_wide <- factors_clean %>%
  pivot_wider(names_from = Ticker, values_from = log_ret)
df <- etf_wide %>%
  inner_join(factor_wide, by = "Date") %>%
  mutate(across(-Date, ~ as.numeric(.)))
etf_list <- sort(unique(etf_clean$Ticker))
factor_list <- sort(unique(factors_clean$Ticker))
sector_map <- etf_clean %>% select(Ticker, Sector) %>% distinct()
tail_beta <- function(x, y, m = 200) {
  keep <- complete.cases(x, y)
  x <- x[keep]
  y <- y[keep]
  # need m+1 tail points
  if (length(x) < m + 1) return(NA_real_)
  # m-th worst losses (left tail)
  et <- sort(x)[m + 1]
  ft <- sort(y)[m + 1]
  et_idx <- x <= et
  ft_idx <- y <= ft
  fext <- sum(ft_idx)
  if (fext == 0) return(NA_real_)
  joint <- sum(et_idx & ft_idx)
  return(joint / fext)
}
compute_matrix <- function(df, etfs, factors, m) {
  out <- matrix(NA_real_,
                nrow = length(etfs),
                ncol = length(factors),
                dimnames = list(etfs, factors))
  for (e in etfs) {
    for (f in factors) {
      out[e,f] <- tail_beta(df[[e]], df[[f]], m = m)
    }
  }
  as.data.frame(out)
}
split_by_sector <- function(res, sector_map) {
  out <- list()
  for (sec in unique(sector_map$Sector)) {
    tickers <- sector_map %>% filter(Sector == sec) %>% pull(Ticker)
    out[[sec]] <- res[rownames(res) %in% tickers, ]
  }
  return(out)
}
sector_tables_m200 <- split_by_sector(res_m200, sector_map)
sector_tables_m150 <- split_by_sector(res_m150, sector_map)
wb <- createWorkbook()
for (sec in names(sector_tables_m200)) {
  addWorksheet(wb, paste0(sec, "_m200"))
  writeData(wb, paste0(sec, "_m200"),
            cbind(Ticker = rownames(sector_tables_m200[[sec]]),
                  sector_tables_m200[[sec]]),
            rowNames = FALSE)
}
for (sec in names(sector_tables_m150)) {
  addWorksheet(wb, paste0(sec, "_m150"))
  writeData(wb, paste0(sec, "_m150"),
            cbind(Ticker = rownames(sector_tables_m150[[sec]]),
                  sector_tables_m150[[sec]]),
            rowNames = FALSE)
}
saveWorkbook(wb, "tail_beta_results_with_tickers.xlsx", overwrite = TRUE)
