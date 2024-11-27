# Calculate Monthly Returns
calculate_monthly_returns_q6 <- function(data) {
    data %>%
        mutate(YM = format(date, "%Y-%m")) %>%
        arrange(Ticker, date) %>%
        group_by(Ticker, YM) %>%
        filter(date == max(date)) %>%
        group_by(Ticker) %>%
        mutate(MonthlyReturn = Price / lag(Price) - 1) %>%
        select(date, Ticker, MonthlyReturn) %>%
        ungroup() %>%
        filter(date > first(date))
}
