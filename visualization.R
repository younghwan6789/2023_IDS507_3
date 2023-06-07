################################################################################
# MSE, MAE, MAPE
################################################################################
view_residual_plot <- function(variable, prediction) {
    residuals <- variable - variable

    print("---- residuals ----")
    print(residuals)

    plot(prediction, residuals,
        main = "Residual Plot",
        xlab = "Predicted Values",
        ylab = "Residuals"
    )
}
