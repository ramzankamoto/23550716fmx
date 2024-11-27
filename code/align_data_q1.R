# Align Data
align_data_q1 <- function(median_fund_data, ai_fund, benchmark) {
    median_fund_data %>%
        select(date, Net_Return) %>%
        rename(Median_Fund_Net = Net_Return) %>%
        left_join(ai_fund %>% rename(AI_Fund_Return = AI_Fund), by = "date") %>%
        left_join(benchmark %>% rename(BM_Return = Returns), by = "date") %>%
        select(-Tickers)
}