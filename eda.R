library(ggplot2)
draw_rent_per_day_geom_line <- function(df) {
    ggplot(df, aes(x = datetime, y = rent)) +
        geom_line() +
        xlab("Date") +
        ylab("Rental") +
        ggtitle("Rental per Day")
}

draw_temper_per_day_geom_line <- function(df) {
    ggplot(df, aes(x = datetime)) +
        geom_line(aes(y = avg_temperature, color = "Average Temperature")) +
        geom_line(aes(y = low_temperature, color = "Low Temperature")) +
        geom_line(aes(y = high_temperature, color = "High Temperature")) +
        xlab("Date") +
        ylab("Temperature") +
        ggtitle("Temperature per Day") +
        scale_color_manual(values = c(
            "Average Temperature" = "yellow",
            "Low Temperature" = "blue",
            "High Temperature" = "red"
        ))
}

draw_temper_per_day_geom_histogram <- function(df) {
ggplot(df, aes(x = high_temperature)) +
  geom_histogram(fill = "#ff9999", color = "black", bins = 10) +
  labs(x = "최고 기온", y = "빈도") +
  ggtitle("최고 기온 히스토그램")
}

draw_rainy_per_day_geom_line <- function(df) {
    ggplot(df, aes(x = datetime, y = rainy)) +
        geom_line(color = "#9999ff") +
        xlab("Date") +
        ylab("Rain") +
        ggtitle("Rain per Day")
}

draw_rainy_per_day_geom_histogram <- function(df) {
ggplot(df, aes(x = rainy)) +
  geom_histogram(fill = "#9999ff", color = "black", bins = 10) +
  labs(x = "강수량", y = "빈도") +
  ggtitle("강수량 히스토그램")
}

draw_windy_per_day_geom_line <- function(df) {
    ggplot(df, aes(x = datetime, y = windy)) +
        geom_line(color = "#008800") +
        xlab("Date") +
        ylab("Wind") +
        ggtitle("Wind per Day")
}

draw_windy_per_day_geom_histogram <- function(df) {
ggplot(df, aes(x = windy)) +
  geom_histogram(fill = "#99ff99", color = "black", bins = 10) +
  labs(x = "풍속", y = "빈도") +
  ggtitle("풍속 히스토그램")
}

draw_part_matters_geom_line <- function(df) {
    ggplot(df, aes(x = datetime)) +
        geom_line(aes(y = part_matter, color = "a")) +
        geom_line(aes(y = ultra_part_matter, color = "b")) +
        xlab("Date") +
        ylab("Temperature") +
        ggtitle("Temperature per Day") +
        scale_color_manual(values = c(
            "a" = "blue",
            "b" = "red"
        ))
}

draw_vif <- function(df) {
    ggplot(df, aes(x = Variable, y = VIF)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        xlab("Variable") +
        ylab("VIF") +
        ggtitle("VIF Values") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# ggplot(eda[(eda$datetime > "2021-05-25" & eda$datetime < "2022-01-06"),], aes(x = datetime, y = windy)) +
# geom_line() +
# xlab("Date") +
# ylab("Wind") +
# ggtitle("Wind per Day")

# ggplot(eda[(eda$datetime > "2021-05-29" & eda$datetime < "2022-01-03"),], aes(x = datetime)) +
#   geom_line(aes(y = part_matter, color = "a")) +
#   geom_line(aes(y = ultra_part_matter, color = "b")) +
#   xlab("Date") +
#   ylab("Temperature") +
#   ggtitle("Temperature per Day") +
#   scale_color_manual(values = c("a" = "blue",
#                                 "b" = "red"))
