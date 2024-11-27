# Plot Rolling Returns
plot_rolling_returns_q1 <- function(data, date_column, return_columns, rolling_window = 12) {
    rolling_data <- data %>%
        mutate(across(all_of(return_columns),
                      ~ rollapplyr(.x, width = rolling_window, mean, fill = NA, align = "right"),
                      .names = "Rolling_{.col}"))

    rolling_long <- rolling_data %>%
        pivot_longer(cols = starts_with("Rolling"),
                     names_to = "Fund",
                     names_prefix = "Rolling_",
                     values_to = "Rolling_Return")

    ggplot(rolling_long, aes_string(x = date_column, y = "Rolling_Return", color = "Fund")) +
        geom_line(size = 1.1) +
        labs(title = "12-Month Rolling Returns Comparison",
             x = "Date", y = "Rolling Returns (%)",
             color = "Fund") +
        theme_minimal() +
        theme(legend.position = "bottom")
}