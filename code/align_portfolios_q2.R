# Align Portfolio Data for Question 2
align_portfolios_q2 <- function(hedged, unhedged, global, zar) {
    hedged %>%
        select(date, Fully_Hedged) %>%
        left_join(unhedged %>% select(date, Unhedged), by = "date") %>%
        left_join(global %>% select(date, global), by = "date") %>%
        left_join(zar %>% select(date, value_chg), by = "date")
}