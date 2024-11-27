# Convert Data to xts
convert_to_xts_q4 <- function(cum_ret_df) {
    tbl_xts(tblData = cum_ret_df, cols_to_xts = c("Port_returns", "BM_returns"))
}