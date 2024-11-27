
# Generate Charts for Returns and Drawdowns
generate_charts_q4 <- function(xts_data) {
    # Cumulative Returns
    chart.CumReturns(
        xts_data[, c("Port_returns", "BM_returns")],
        main = "Cumulative Returns: BM vs SnakeOil",
        legend.loc = "bottomright",
        colorset = c("blue", "red") # Optional: Set colors for clarity
    )

    # Drawdowns
    chart.Drawdown(
        xts_data[, c("Port_returns", "BM_returns")],
        main = "Drawdowns: BM vs SnakeOil",
        legend.loc = "bottomright",
        colorset = c("blue", "red") # Optional: Set colors for clarity
    )

    # Rolling Cumulative Returns
    chart.RollingPerformance(
        xts_data["2019-10-31::", c("Port_returns", "BM_returns")],
        FUN = "Return.cumulative", # Use cumulative returns
        width = 12, # Rolling window size
        main = "Rolling Cumulative Returns: BM vs SnakeOil",
        legend.loc = "bottomright",
        colorset = c("blue", "red") # Optional: Set colors for clarity
    )
}