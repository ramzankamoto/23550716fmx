# Scatter Plot with Marginal Densities for Question 2
scatter_with_marginals_q2 <- function(data, x_var, y_var, title, x_label, y_label) {
    scatter_plot <- ggplot(data, aes(x = {{x_var}}, y = {{y_var}})) +
        geom_point(alpha = 0.6, color = "#2C7BB6") +
        geom_smooth(method = "lm", color = "grey", se = FALSE) +
        labs(title = title, x = x_label, y = y_label) +
        theme_minimal()

    ggMarginal(
        scatter_plot,
        type = "density",
        margins = "both",
        xparams = list(fill = "#FDAE61", alpha = 0.5),
        yparams = list(fill = "#D73027", alpha = 0.5)
    )
}