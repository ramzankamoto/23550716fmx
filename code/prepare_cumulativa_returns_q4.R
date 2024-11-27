
# Prepare Cumulative Returns Data
prepare_cumulative_returns_q4 <- function(port_rets, bm_rets) {
    port_rets <- port_rets %>% rename(Port_returns = Returns)
    bm_rets <- bm_rets %>% rename(BM_returns = BM)

    port_rets %>%
        select(date, Port_returns) %>%
        left_join(bm_rets %>% select(date, BM_returns), by = "date")
}







