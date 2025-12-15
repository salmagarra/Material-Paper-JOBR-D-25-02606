library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
df <- fread("etf_clean_returns.csv")
df$Date <- as.Date(df$Date)
df <- df %>% filter(Sector %in% c("Agrifood", "ESG", "Financial"))
hill_evt <- function(returns, m = 200, p = 0.001) {
  losses <- -returns
  losses <- losses[losses > 0]
  n <- length(losses)
  if (n <= m + 1) return(c(alpha = NA, xp = NA, ES = NA))
  losses <- sort(losses)
  x_nm <- losses[n - m]
  tail <- losses[(n - m + 1):n]
  alpha <- 1 / mean(log(tail / x_nm))
  xp <- x_nm * (m / (n * p))^(1 / alpha)
  ES <- ifelse(alpha > 1, xp / (alpha - 1), NA)
  c(alpha = alpha, xp = xp, ES = ES)
}
df <- df %>%
  mutate(Year = year(Date))
years <- 2007:2025
out <- list()
k <- 1
for (y in years) {
  win <- df %>% filter(Year >= y - 6, Year <= y)
  for (s in unique(win$Sector)) {
    tmp <- win %>% filter(Sector == s)
    est <- tmp %>%
      group_by(Ticker) %>%
      summarise(
        t(hill_evt(log_ret)),
        .groups = "drop"
      )
    out[[k]] <- data.frame(
      Year = y,
      Sector = s,
      alpha = mean(est$alpha, na.rm = TRUE),
      xp_001 = mean(est$xp, na.rm = TRUE),
      ES_001 = mean(est$ES, na.rm = TRUE)
    )
    k <- k + 1
  }
}
rolling_df <- bind_rows(out)
rolling_df$Sector <- factor(rolling_df$Sector)
p_alpha <- ggplot(rolling_df, aes(Year, alpha, color = Sector)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  theme_minimal()
p_xp <- ggplot(rolling_df, aes(Year, xp_001, color = Sector)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  theme_minimal()
p_ES <- ggplot(rolling_df, aes(Year, ES_001, color = Sector)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  theme_minimal()
ggsave("rolling_alpha.png", p_alpha, width = 9, height = 4, dpi = 300)
ggsave("rolling_xp001.png", p_xp, width = 9, height = 4, dpi = 300)
ggsave("rolling_ES001.png", p_ES, width = 9, height = 4, dpi = 300)
