library(dplyr)
library(lubridate)
library(ggplot2)
df <- read.csv("etf_clean_returns.csv")
df$Date <- as.Date(df$Date)
df <- df %>%
  filter(Sector %in% c("Agrifood", "ESG", "Financial"))
df_sector <- df %>%
  group_by(Date, Sector) %>%
  summarise(sector_ret = mean(log_ret, na.rm = TRUE), .groups = "drop") %>%
  mutate(Year = year(Date)) %>%
  arrange(Sector, Date)
hill_evt <- function(losses, m_max = 200, p = 0.001) {
  losses <- as.numeric(losses)
  losses <- losses[is.finite(losses) & losses > 0 & losses < 0.5]
  n <- length(losses)
  if (n < 50) return(NULL)
  losses <- sort(losses)
  m <- min(m_max, max(30, floor(0.1 * n)))
  if (n <= m + 5) return(NULL)
  tail <- losses[(n - m + 1):n]
  x_nm <- tail[1]
  alpha <- 1 / mean(log(tail / x_nm))
  if (!is.finite(alpha) || alpha <= 1.01) return(NULL)
  x_p <- x_nm * (m / (n * p))^(1 / alpha)
  ES_p <- x_p / (alpha - 1)
  data.frame(
    alpha = alpha,
    x_p_0.001 = x_p,
    ES_xp_0.001 = ES_p
  )
}
years <- sort(unique(df_sector$Year))
years <- years[years >= min(years) + 6]
res <- list()
k <- 1
for (y in years) {
  win <- df_sector %>%
    filter(Year >= y - 6, Year <= y)
  for (s in unique(win$Sector)) {
    losses <- -win$sector_ret[win$Sector == s]
    evt <- hill_evt(losses)

    if (is.null(evt)) next

    res[[k]] <- data.frame(
      Year = y,
      Sector = s,
      alpha = evt$alpha,
      x_p_0.001 = evt$x_p_0.001,
      ES_xp_0.001 = evt$ES_xp_0.001
    )
    k <- k + 1
  }
}
rolling_evt <- bind_rows(res) %>%
  filter(Year >= 2007, Year <= 2025)
rolling_evt$Sector <- factor(rolling_evt$Sector)
rolling_evt$Year <- as.integer(rolling_evt$Year)
p_alpha <- ggplot(rolling_evt, aes(Year, alpha, color = Sector)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1.1) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3, linewidth = 1) +
  scale_x_continuous(breaks = 2007:2025, limits = c(2007, 2025)) +
  labs(x = "Year", y = "Tail index (alpha)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p_xp <- ggplot(rolling_evt, aes(Year, x_p_0.001, color = Sector)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1.1) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3, linewidth = 1) +
  scale_x_continuous(breaks = 2007:2025, limits = c(2007, 2025)) +
  labs(
    x = "Year",
    y = expression("Tail quantile  x"[p] * "  (p = 0.001)")
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p_es <- ggplot(rolling_evt, aes(Year, ES_xp_0.001, color = Sector)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1.1) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3, linewidth = 1) +
  scale_x_continuous(breaks = 2007:2025, limits = c(2007, 2025)) +
  labs(
    x = "Year",
    y = expression("Expected Shortfall  ES"[p] * "  (p = 0.001)")
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("rolling_tail_alpha.png", p_alpha, width = 10, height = 5, dpi = 300)
ggsave("rolling_tail_quantile_xp001.png", p_xp, width = 10, height = 5, dpi = 300)
ggsave("rolling_tail_ES_p001.png", p_es, width = 10, height = 5, dpi = 300)
