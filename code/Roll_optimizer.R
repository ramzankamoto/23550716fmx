# Rolling Optimizer Function
Roll_optimizer <- function(return_mat, EOM_date, LookBackSel = 36) {

    # Filter the data for the lookback period
    return_df_used <- return_mat %>% filter(date >= EOM_date %m-% months(LookBackSel) & date <= EOM_date)

    # Skip iteration if insufficient data
    if (nrow(return_df_used) < LookBackSel) return(NULL)

    # Remove the date column and convert to matrix
    return_mat_Nodate <- data.matrix(return_df_used[, -1])

    # Calculate covariance matrix and mean returns
    Sigma <- RiskPortfolios::covEstimation(return_mat_Nodate)
    Mu <- return_df_used %>% summarise(across(-date, ~prod(1 + .)^(1 / LookBackSel) - 1)) %>% purrr::as_vector()

    # Perform optimization using multiple methods
    My_Weights <-
        optim_foo(Type = "mv", mu = Mu, Sigma = Sigma, LB = LB, UB = UB, printmsg = FALSE) %>%
        left_join(
            optim_foo(Type = "minvol", mu = Mu, Sigma = Sigma, LB = LB, UB = UB, printmsg = FALSE),
            by = c("Tickers")
        ) %>%
        left_join(
            optim_foo(Type = "erc", mu = Mu, Sigma = Sigma, LB = LB, UB = UB, printmsg = FALSE),
            by = c("Tickers")
        ) %>%
        left_join(
            optim_foo(Type = "riskeff", mu = Mu, Sigma = Sigma, LB = LB, UB = UB, printmsg = FALSE),
            by = c("Tickers")
        ) %>%
        mutate(
            date = EOM_date,            # Add the current EOM date
            Look_Back_Period = LookBackSel  # Add the lookback period
        )

    return(My_Weights)
}
