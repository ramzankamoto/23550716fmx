# Adjust Dates to End of Month for Question 2
adjust_dates_q2 <- function(data, date_column) {
    data %>%
        mutate({{date_column}} := ceiling_date({{date_column}}, "month") - days(1))
}

# Construct Fully Hedged Portfolio for Question 2
construct_hedged_portfolio_q2 <- function(indexes, zar_changes) {
    indexes %>%
        mutate(
            MSCI_ACWI_Hedged = MSCI_ACWI + zar_changes,
            Bbg_Agg_Hedged = Bbg_Agg + zar_changes,
            Fully_Hedged = (J433 * 0.7 * 0.6) +
                (ALBI * 0.7 * 0.4) +
                (MSCI_ACWI_Hedged * 0.3 * 0.6) +
                (Bbg_Agg_Hedged * 0.3 * 0.4)
        )
}










