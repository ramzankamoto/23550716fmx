# Calculate Sector Contributions
calculate_sector_contributions_q4 <- function(port_holds_sector, port_rets) {
    portfolio_data <- port_holds_sector %>%
        left_join(port_rets, by = "date") %>%
        mutate(contribution = Weight * Returns)

    portfolio_data %>%
        group_by(date, Sector) %>%
        summarise(Sector_Contribution = sum(contribution, na.rm = TRUE)) %>%
        ungroup()
}