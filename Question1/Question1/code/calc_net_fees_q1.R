# Calculate Net of Fees Returns
calc_net_fees_q1 <- function(data, return_column, fee_bps = 200) {
    fee_converter <- function(fee_bps, annualization = 12) {
        (1 + fee_bps / 1e4)^(1 / annualization) - 1
    }
    data %>%
        mutate(Net_Return = !!sym(return_column) - fee_converter(fee_bps))
}