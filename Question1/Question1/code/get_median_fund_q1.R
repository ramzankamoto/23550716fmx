# Calculate Median Fund
get_median_fund_q1 <- function(data) {
    data %>%
        filter(Index != "Yes") %>% # Exclude index funds
        group_by(Fund) %>%
        summarise(Mean_Return = mean(Returns, na.rm = TRUE)) %>%
        arrange(Mean_Return) %>%
        slice(ceiling(n() / 2)) %>%
        pull(Fund)
}