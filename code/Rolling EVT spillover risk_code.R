library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
df <- read.csv("etf_clean_returns.csv")
df$Date <- as.Date(df$Date)
df <- df %>% distinct(Date, Ticker, .keep_all = TRUE)
ESG <- c("DSI","EAGG","ESGD","ESGE","ESGU","ESGV","SUSA","VSGX")
Agrifood <- c("MOO","VEGI","PBJ","FTXG","KROP","FTAG","IVEG","EATZ")
Financial <- c("XLF","VFH","KBWB","IYF","EUFN","KRE","FAS","FNCL")
groups <- list(
  ESG = ESG,
  Agrifood = Agrifood,
  Financial = Financial
)
rolling_spillover <- function(df, tickers, window = 500, m = 200) {
  wide <- df %>%
    filter(Ticker %in% tickers) %>%
    select(Date, Ticker, log_ret) %>%
    pivot_wider(names_from = Ticker, values_from = log_ret) %>%
    arrange(Date) %>%
    drop_na()
  dates <- wide$Date
  mat <- as.matrix(wide[,-1])
  EJ <- rep(NA, nrow(mat))
  PALL <- rep(NA, nrow(mat))
  for (i in (window + 1):nrow(mat)) {
    losses <- -mat[(i - window):i, ]
    if (nrow(losses) <= m) next
    thresh <- apply(losses, 2, function(x) sort(x, decreasing = TRUE)[m])
    U <- sweep(losses, 2, thresh, ">=") * 1
    any_exc <- rowSums(U) > 0
    if (sum(any_exc) == 0) next
    EJ[i] <- mean(rowSums(U[any_exc, ]))
    PALL[i] <- mean(rowSums(U[any_exc, ]) == ncol(U))
  }
  data.frame(Date = dates, ExpectedJoint = EJ, ProbAll = PALL)
}
spill_data <- bind_rows(
  lapply(names(groups), function(s) {
    out <- rolling_spillover(df, groups[[s]])
    out$Sector <- s
    out
  })
)
sector_colors <- c(
  "Agrifood" = "#D55E00",
  "ESG" = "#009E73",
  "Financial" = "#0072B2"
)
theme_clean <- theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 14),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    panel.grid.minor = element_blank()
  )
plot_EJ <- ggplot(spill_data, aes(Date, ExpectedJoint, color = Sector)) +
  geom_smooth(method = "loess", span = 0.15, se = FALSE, linewidth = 1.6) +
  scale_color_manual(values = sector_colors) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Rolling Spillover Risk – Expected Joint Crashes",
    x = "Year",
    y = "Expected Joint Crashes"
  ) +
  theme_clean
plot_PALL <- ggplot(spill_data, aes(Date, ProbAll, color = Sector)) +
  geom_smooth(method = "loess", span = 0.15, se = FALSE, linewidth = 1.6) +
  scale_color_manual(values = sector_colors) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Rolling Spillover Risk – Probability All ETFs Crash",
    x = "Year",
    y = "Probability"
  ) +
  theme_clean
ggsave("spillover_expected_joint.png", plot_EJ, width = 12, height = 7, dpi = 600)
ggsave("spillover_expected_joint.pdf", plot_EJ, width = 12, height = 7)
ggsave("spillover_prob_all.png", plot_PALL, width = 12, height = 7, dpi = 600)
ggsave("spillover_prob_all.pdf", plot_PALL, width = 12, height = 7)
