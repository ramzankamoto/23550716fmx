# Compute Portfolio Statistics for Question 2
compute_portfolio_stats_q2 <- function(data) {
    data %>%
        summarise(
            `ZAR Correlation (Hedged)` = cor(Fully_Hedged, value_chg, use = "complete.obs") * 100,
            `ZAR Correlation (Unhedged)` = cor(Unhedged, value_chg, use = "complete.obs") * 100,
            `ZAR Correlation (Global)` = cor(global, value_chg, use = "complete.obs") * 100,
            `Returns (Ann., Hedged)` = mean(Fully_Hedged, na.rm = TRUE) * 12 * 100,
            `Returns (Ann., Unhedged)` = mean(Unhedged, na.rm = TRUE) * 12 * 100,
            `Returns (Ann., Global)` = mean(global, na.rm = TRUE) * 12 * 100,
            `Volatility (Ann., Hedged)` = sd(Fully_Hedged, na.rm = TRUE) * sqrt(12) * 100,
            `Volatility (Ann., Unhedged)` = sd(Unhedged, na.rm = TRUE) * sqrt(12) * 100,
            `Volatility (Ann., Global)` = sd(global, na.rm = TRUE) * sqrt(12) * 100
        )
}