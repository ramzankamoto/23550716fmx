# Plot Sector Contributions Over Time
plot_sector_contributions_q4 <- function(sector_contributions) {
    ggplot(sector_contributions, aes(x = date, y = Sector_Contribution, color = Sector)) +
        geom_line() +
        labs(
            title = "Sector Contributions Over Time",
            x = "Date",
            y = "Contribution to Portfolio Returns",
            color = "Sector"
        ) +
        theme_minimal()
}