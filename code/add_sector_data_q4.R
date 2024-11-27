# Add Sector Data to Portfolio Holdings
add_sector_data_q4 <- function(port_holds, bm_holds) {
    port_holds %>%
        left_join(bm_holds %>% select(date, Tickers, Sector), by = c("date", "Tickers"))
}