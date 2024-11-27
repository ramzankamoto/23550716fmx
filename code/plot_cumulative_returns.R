
plot_cumulative_returns <- function(returns_data, index_name) {
    returns_data <- returns_data %>%
        mutate(
            Cumulative_Uncapped = cumprod(1 + Uncapped_Returns) - 1,
            Cumulative_Capped = cumprod(1 + Capped_Returns) - 1
        )

    ggplot(returns_data, aes(x = date)) +
        geom_line(aes(y = Cumulative_Uncapped, color = paste("Uncapped", index_name))) +
        geom_line(aes(y = Cumulative_Capped, color = paste("Capped (10%)", index_name))) +
        labs(
            title = paste("Cumulative Returns: Uncapped vs Capped (10%)", index_name),
            x = "Date",
            y = "Cumulative Returns",
            color = "Index Type"
        ) +
        theme_minimal() +
        scale_color_manual(values = c("blue", "red"))
}
