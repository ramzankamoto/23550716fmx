# Generate Summary Statistics and Create a Table
generate_summary_q4 <- function(xts_data) {
    # Compute summary statistics using PerformanceAnalytics
    sfm_summary <- table.SFM(
        Ra = xts_data$Port_returns, # Fund returns
        Rb = xts_data$BM_returns   # Benchmark returns
    )

    # Convert to a data frame for table creation
    sfm_summary_df <- as.data.frame(sfm_summary) %>%
        rownames_to_column(var = "Metric") %>%
        mutate_if(is.numeric, round, 2) # Round numeric values for presentation

    # Create a formatted flextable
    table <- flextable::flextable(sfm_summary_df) %>%
        flextable::set_header_labels(
            Metric = "Performance Metric",
            Ra = "Fund (SnakeOil)",
            Rb = "Benchmark"
        ) %>%
        flextable::theme_vanilla() %>%
        flextable::autofit()

    return(table)
}
