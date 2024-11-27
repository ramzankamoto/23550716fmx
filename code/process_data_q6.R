# Process MAA and MSCI Data
process_data_q6 <- function(maa, msci) {
    maa_processed <- maa %>%
        mutate(
            Ticker = str_replace_all(Ticker, " ", "_"),
            date = as.Date(date)
        ) %>%
        select(date, Ticker, Price)

    msci_processed <- msci %>%
        rename(Ticker = Name) %>%
        mutate(date = as.Date(date)) %>%
        select(date, Ticker, Price)

    list(maa = maa_processed, msci = msci_processed)
}













