
cap_weights <- function(weights_data, cap_level = 0.10) {
    Proportional_Cap <- function(df, cap) {
        while (any(df$weight > cap)) {
            df <- df %>%
                mutate(
                    excess_weight = ifelse(weight > cap, weight - cap, 0),
                    weight = ifelse(weight > cap, cap, weight)
                ) %>%
                mutate(weight = weight + excess_weight * weight / sum(weight))
        }
        df
    }

    weights_data %>%
        group_by(date) %>%
        group_split() %>%
        purrr::map_df(~ Proportional_Cap(.x, cap_level))
}