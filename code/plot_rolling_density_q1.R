# Plot Density of Rolling Returns for Question 1
plot_rolling_density_q1 <- function(data, date_column, return_columns, rolling_window = 12) {
    rolling_data <- data %>%
        mutate(across(all_of(return_columns),
                      ~ rollapplyr(.x, width = rolling_window, mean, fill = NA, align = "right"),
                      .names = "Rolling_{.col}"))

    rolling_long <- rolling_data %>%
        pivot_longer(cols = starts_with("Rolling"),
                     names_to = "Fund",
                     names_prefix = "Rolling_",
                     values_to = "Rolling_Return")

    ggplot(rolling_long, aes(x = Rolling_Return, fill = Fund)) +
        geom_density(alpha = 0.5) +
        labs(title = "Density of 12-Month Rolling Returns",
             x = "Rolling Returns (%)",
             y = "Density",
             fill = "Fund") +
        theme_minimal() +
        theme(legend.position = "bottom")
}