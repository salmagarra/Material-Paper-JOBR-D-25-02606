library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
etf <- read.csv("etf_clean_returns.csv")
cf  <- read.csv("conditioning_factors.csv")
etf$Date <- as.Date(etf$Date)
cf$Date  <- as.Date(cf$Date)
etf <- etf %>%
  group_by(Date, Ticker) %>%
  summarise(log_ret = mean(log_ret), .groups = "drop")
cf <- cf %>%
  group_by(Date, Ticker) %>%
  summarise(log_ret = mean(log_ret), .groups = "drop")
cf_w <- cf %>%
  pivot_wider(names_from = Ticker, values_from = log_ret)
window  <- 1764
p_tail  <- 0.01
tail_dep <- function(x, y, p = 0.01) {
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  if (length(x) < 50 || length(y) < 50) return(NA_real_)
  qx <- quantile(x, p, na.rm = TRUE)
  qy <- quantile(y, p, na.rm = TRUE)
  mean(x < qx & y < qy) / p
}
compute_sector_risk <- function(sector_etfs, sector_name, factors) {
  sector_df <- etf %>%
    filter(Ticker %in% sector_etfs) %>%
    group_by(Date) %>%
    summarise(SECTOR = mean(log_ret, na.rm = TRUE), .groups = "drop")
  df <- inner_join(sector_df, cf_w, by = "Date") %>% arrange(Date)
  res <- lapply(factors, function(f) {
    tb <- rollapply(
      df[, c("SECTOR", f)],
      width = window,
      by.column = FALSE,
      FUN = function(w) tail_dep(w[,1], w[,2], p_tail),
      fill = NA,
      align = "right"
    )
    data.frame(
      Date = df$Date,
      Sector = sector_name,
      Factor = f,
      TailBeta = tb
    )
  })
  bind_rows(res) %>%
    group_by(Factor) %>%
    mutate(
      TailBeta_smooth = rollapply(
        TailBeta, width = 60,
        FUN = median, align = "center", fill = NA
      )
    ) %>%
    ungroup()
}
esg_etfs       <- c("DSI","EAGG","ESGD","ESGE","ESGU","ESGV","SUSA","VSGX")
agrifood_etfs  <- c("MOO","VEGI","PBJ","FTXG","KROP","FTAG","IVEG","EATZ")
financial_etfs <- c("XLF","VFH","KBWB","IYF","EUFN","KRE","FAS","FNCL")
regional_factors <- c("EZA","IEMG")
slc_factors      <- c("XLY","IYT","PICK","RTH")

esg_regional       <- compute_sector_risk(esg_etfs,       "ESG",        regional_factors)
agrifood_regional  <- compute_sector_risk(agrifood_etfs,  "Agrifood",   regional_factors)
financial_regional <- compute_sector_risk(financial_etfs, "Financials", regional_factors)
esg_slc       <- compute_sector_risk(esg_etfs,       "ESG",        slc_factors)
agrifood_slc  <- compute_sector_risk(agrifood_etfs,  "Agrifood",   slc_factors)
financial_slc <- compute_sector_risk(financial_etfs, "Financials", slc_factors)
plot_sector <- function(df, title_text, colors) {
  first_valid <- min(df$Date[!is.na(df$TailBeta_smooth)], na.rm = TRUE)
  df_clean <- df %>% filter(Date >= first_valid)
  ggplot(df_clean, aes(Date, TailBeta_smooth, color = Factor)) +
    geom_smooth(method = "loess", span = 0.22, se = FALSE, linewidth = 1.6) +
    scale_color_manual(values = colors) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title = title_text,
      x = "Date",
      y = "Tail-β (Systemic Risk)"
    ) +
    theme_minimal(base_size = 18) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", size = 22)
    )
}
p1 <- plot_sector(
  esg_regional,
  "ESG – Systemic Risk (Regional Exposure: EZA vs IEMG)",
  c("EZA"="#1f77b4","IEMG"="#d62728")
)
p2 <- plot_sector(
  agrifood_regional,
  "Agrifood – Systemic Risk (Regional Exposure: EZA vs IEMG)",
  c("EZA"="#1f77b4","IEMG"="#d62728")
)
p3 <- plot_sector(
  financial_regional,
  "Financials – Systemic Risk (Regional Exposure: EZA vs IEMG)",
  c("EZA"="#1f77b4","IEMG"="#d62728")
)
p4 <- plot_sector(
  esg_slc,
  "ESG – Systemic Risk (High SLC Industries)",
  c("XLY"="#1f77b4","IYT"="#d62728","PICK"="#2ca02c","RTH"="#9467bd")
)
p5 <- plot_sector(
  agrifood_slc,
  "Agrifood – Systemic Risk (High SLC Industries)",
  c("XLY"="#1f77b4","IYT"="#d62728","PICK"="#2ca02c","RTH"="#9467bd")
)
p6 <- plot_sector(
  financial_slc,
  "Financials – Systemic Risk (High SLC Industries)",
  c("XLY"="#1f77b4","IYT"="#d62728","PICK"="#2ca02c","RTH"="#9467bd")
)
