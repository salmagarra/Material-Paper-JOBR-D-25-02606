library(dplyr)
library(tidyr)
df <- read.csv("etf_clean_returns.csv")
df$Date <- as.Date(df$Date)
df <- df %>% distinct(Date, Ticker, .keep_all = TRUE)
esg <- c("DSI","EAGG","ESGD","ESGE","ESGU","ESGV","SUSA","VSGX")
agr <- c("MOO","VEGI","PBJ","FTXG","KROP","FTAG","IVEG","EATZ")
fin <- c("XLF","VFH","KBWB","IYF","EUFN","KRE","FAS","FNCL")
groups <- list(
  ESG = esg,
  Agrifood = agr,
  Financial = fin
)
spillover_risk <- function(df, tickers, m){
  piv <- df %>%
    filter(Ticker %in% tickers) %>%
    select(Date, Ticker, log_ret) %>%
    pivot_wider(names_from = Ticker, values_from = log_ret) %>%
    drop_na()
  losses <- -as.matrix(piv[,-1])
  n <- nrow(losses)
  if (n <= m) return(c(NA, NA))
  thresh <- apply(losses, 2, function(x) sort(x, decreasing = TRUE)[m])
  U <- sweep(losses, 2, thresh, ">") * 1
  any_exc <- apply(U, 1, any)
  if (sum(any_exc) == 0) return(c(NA, NA))
  expected_joint <- mean(rowSums(U[any_exc, ]))
  prob_all <- mean(rowSums(U[any_exc, ]) == length(tickers))
  c(expected_joint, prob_all)
}
results <- data.frame()
for (sector in names(groups)) {
  for (m in c(150, 200)) {
    res <- spillover_risk(df, groups[[sector]], m)
    results <- rbind(
      results,
      data.frame(
        Sector = sector,
        m = m,
        ExpectedJointExceedances = res[1],
        ProbabilityAllCrash = res[2]
      )
    )
  }
}
print(results)
