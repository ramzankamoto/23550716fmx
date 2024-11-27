# Construct Unhedged Portfolio for Question 2
construct_unhedged_portfolio_q2 <- function(indexes) {
    indexes %>%
        mutate(
            Unhedged = (J433 * 0.7 * 0.6) +
                (ALBI * 0.7 * 0.4) +
                (MSCI_ACWI * 0.3 * 0.6) +
                (Bbg_Agg * 0.3 * 0.4)
        )
}
