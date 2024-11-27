# Combine MAA and MSCI Returns
combine_data_q6 <- function(maa_returns, msci_returns) {
    bind_rows(
        maa_returns %>% mutate(Instrument = if_else(str_detect(Ticker, "Index"), "credit", NA_character_)),
        msci_returns %>% mutate(Instrument = if_else(str_detect(Ticker, "MSCI"), "equity", NA_character_))
    )
}

# Convert Combined Data to Wide Format
make_data_wide_q6 <- function(data) {
    data %>%
        select(date, Ticker, MonthlyReturn) %>%
        spread(Ticker, MonthlyReturn)
}
