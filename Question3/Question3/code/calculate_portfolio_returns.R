
calculate_portfolio_returns <- function(weights_data, returns_data) {
    library(tbl2xts)
    library(PerformanceAnalytics)

    weights_xts <- weights_data %>%
        tbl_xts(cols_to_xts = weight, spread_by = Tickers)

    returns_xts <- returns_data %>%
        filter(Tickers %in% unique(weights_data$Tickers)) %>%
        tbl_xts(cols_to_xts = Return, spread_by = Tickers)

    # Handle missing data
    weights_xts[is.na(weights_xts)] <- 0
    returns_xts[is.na(returns_xts)] <- 0

    rmsfuns::Safe_Return.portfolio(
        R = returns_xts,
        weights = weights_xts,
        lag_weights = TRUE
    ) %>%
        xts_tbl() %>%
        rename(portfolio_returns = portfolio.returns)
}