# Impute Missing Returns
impute_missing_returns <- function(return_mat, impute_returns_method = "NONE") {
    if (!"date" %in% colnames(return_mat)) stop("No 'date' column provided in return_mat.")

    if (impute_returns_method %in% c("NONE", "None", "none")) {
        if (any(is.na(return_mat))) {
            warning("There are missing values in the return matrix. Consider using impute_returns_method = 'Drawn_Distribution_Own'.")
        }
        return(return_mat)
    }

    if (impute_returns_method == "Drawn_Distribution_Own") {
        N <- nrow(return_mat)
        return_mat <- return_mat %>%
            gather(Ticker, MonthlyReturn, -date) %>%
            left_join(
                return_mat %>%
                    gather(Ticker, MonthlyReturn, -date) %>%
                    group_by(Ticker) %>%
                    mutate(Dens = list(density(MonthlyReturn, na.rm = TRUE))) %>%
                    summarise(Random_Draws = list(sample(Dens[[1]]$x, N, replace = TRUE, prob = Dens[[1]]$y))),
                by = "Ticker"
            ) %>%
            group_by(Ticker) %>%
            mutate(MonthlyReturn = coalesce(MonthlyReturn, Random_Draws[[1]][row_number()])) %>%
            select(-Random_Draws) %>%
            ungroup() %>%
            spread(Ticker, MonthlyReturn)
        return(return_mat)
    } else {
        stop("Invalid impute_returns_method.")
    }
}
