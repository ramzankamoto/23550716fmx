# Construct Global Portfolio for Question 2
construct_global_portfolio_q2 <- function(indexes) {
    indexes %>%
        mutate(
            global = (MSCI_ACWI * 0.6) +
                (Bbg_Agg * 0.4)
        )
}