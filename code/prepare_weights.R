prepare_weights <- function(ALSI, index_col, rebalance_dates) {
    ALSI %>%
        filter(date %in% rebalance_dates) %>%
        group_by(date) %>%
        mutate(weight = !!sym(index_col) / sum(!!sym(index_col), na.rm = TRUE)) %>%
        ungroup()
}

