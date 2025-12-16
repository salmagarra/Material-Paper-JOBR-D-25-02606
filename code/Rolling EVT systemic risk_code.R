library(data.table)
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)
library(tidyr)

etf <- fread("etf_clean_returns.csv")
cf  <- fread("conditioning_factors.csv")

etf$Date <- as.Date(etf$Date)
cf$Date  <- as.Date(cf$Date)

tail_beta <- function(x, y, p = 0.01) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  if (length(x) < 50 || length(y) < 50) return(NA_real_)
  qx <- quantile(x, p, na.rm = TRUE)
  qy <- quantile(y, p, na.rm = TRUE)
  mean(x <= qx & y <= qy) / p
}

window <- 1764

compute_sector_risk <- function(sector_etfs, sector_name, factors) {

  sector_df <- etf %>%
    filter(Ticker %in% sector_etfs) %>%
    group_by(Date) %>%
    summarise(SECTOR = mean(log_ret, na.rm = TRUE), .groups = "drop")

  cf_w <- cf %>%
    filter(Ticker %in% factors) %>%
    group_by(Date, Ticker) %>%
    summarise(log_ret = mean(log_ret), .groups = "drop") %>%
    pivot_wider(names_from = Ticker, values_from = log_ret)

  df <- inner_join(sector_df, cf_w, by = "Date") %>% arrange(Date)

  out <- list()

  for (f in factors) {
    beta <- rollapply(
      df[, c("SECTOR", f)],
      width = window,
      by.column = FALSE,
      FUN = function(w) tail_beta(w[,1], w[,2]),
      fill = NA,
      align = "right"
    )

    out[[f]] <- data.frame(
      Date = df$Date,
      Factor = f,
      TailBeta = beta,
      Sector = sector_name
    )
  }

  bind_rows(out) %>%
    group_by(Factor) %>%
    mutate(
      TailBeta_smooth = rollapply(
        TailBeta, width = 60, FUN = median,
        align = "center", fill = NA
      )
    ) %>%
    ungroup()
}

esg_etfs       <- c("DSI","EAGG","ESGD","ESGE","ESGU","ESGV","SUSA","VSGX")
agrifood_etfs  <- c("MOO","VEGI","PBJ","FTXG","KROP","FTAG","IVEG","EATZ")
financial_etfs <- c("XLF","VFH","KBWB","IYF","EUFN","KRE","FAS","FNCL")

factors_geo  <- c("EZA", "IEMG")
factors_comm <- c("PICK", "RTH", "IYT", "XLY")

ESG_geo  <- compute_sector_risk(esg_etfs,       "ESG",        factors_geo)
AGR_geo  <- compute_sector_risk(agrifood_etfs,  "Agrifood",   factors_geo)
FIN_geo  <- compute_sector_risk(financial_etfs, "Financials", factors_geo)

ESG_com  <- compute_sector_risk(esg_etfs,       "ESG",        factors_comm)
AGR_com  <- compute_sector_risk(agrifood_etfs,  "Agrifood",   factors_comm)
FIN_com  <- compute_sector_risk(financial_etfs, "Financials", factors_comm)

plot_sector <- function(df, title_txt, colors) {
  first_valid <- min(df$Date[!is.na(df$TailBeta_smooth)], na.rm = TRUE)
  ggplot(df %>% filter(Date >= first_valid),
         aes(Date, TailBeta_smooth, color = Factor)) +
    geom_smooth(method = "loess", span = 0.22, se = FALSE, linewidth = 1.8) +
    scale_color_manual(values = colors) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(title = title_txt, x = "Date", y = "Tail-β (Systemic Risk)") +
    theme_minimal(base_size = 20) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

p1 <- plot_sector(ESG_geo, "ESG – Systemic Risk (EZA vs IEMG)",
                  c("EZA"="#1f77b4","IEMG"="#d62728"))
p2 <- plot_sector(AGR_geo, "Agrifood – Systemic Risk (EZA vs IEMG)",
                  c("EZA"="#1f77b4","IEMG"="#d62728"))
p3 <- plot_sector(FIN_geo, "Financials – Systemic Risk (EZA vs IEMG)",
                  c("EZA"="#1f77b4","IEMG"="#d62728"))

p4 <- plot_sector(ESG_com, "ESG – Systemic Risk (PICK, RTH, IYT, XLY)",
                  c("IYT"="#1f77b4","PICK"="#d62728","RTH"="#2ca02c","XLY"="#9467bd"))
p5 <- plot_sector(AGR_com, "Agrifood – Systemic Risk (PICK, RTH, IYT, XLY)",
                  c("IYT"="#1f77b4","PICK"="#d62728","RTH"="#2ca02c","XLY"="#9467bd"))
p6 <- plot_sector(FIN_com, "Financials – Systemic Risk (PICK, RTH, IYT, XLY)",
                  c("IYT"="#1f77b4","PICK"="#d62728","RTH"="#2ca02c","XLY"="#9467bd"))

ggsave("1_ESG_EZA_IEMG.png",        p1, width=16, height=8, dpi=600)
ggsave("2_Agrifood_EZA_IEMG.png",   p2, width=16, height=8, dpi=600)
ggsave("3_Financials_EZA_IEMG.png", p3, width=16, height=8, dpi=600)
ggsave("4_ESG_PICK_RTH_IYT_XLY.png",        p4, width=16, height=8, dpi=600)
ggsave("5_Agrifood_PICK_RTH_IYT_XLY.png",   p5, width=16, height=8, dpi=600)
ggsave("6_Financials_PICK_RTH_IYT_XLY.png", p6, width=16, height=8, dpi=600)
