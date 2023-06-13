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



pie_df <- data.frame(labels = c("train", "test"),
                 data = c(1196, 532))

p <- ggplot(data = pie_df, aes(x = "", y = data, fill = labels)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("skyblue", "yellow")) +
  labs(title = "Data Split")

# 숫자 추가
p + geom_text(aes(label = data), position = position_stack(vjust = 0.5))
