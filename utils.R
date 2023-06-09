################################################################################
# 날씨정보 결측치 처리 시 사용할 평균 전환 함수
################################################################################
replace_with_mean <- function(df, column) {
  df[column][is.na(df[column])] <- sapply(df[is.na(df[column]), "일시"], function(x) mean(df[df$일시 == x, column][[1]], na.rm = TRUE))
  df
}

################################################################################
# 날씨정보 결측치 처리 시 - 구로구 정보 추가를 위한 함수
################################################################################
create_weather_data <- function(df, date, branch_no) {
  date <- ymd(date)

  guro_data <- data.frame(
    지점 = branch_no,
    일시 = date,
    `평균기온(°C)` = (df[(df$일시 == date - 1 & df$지점 == branch_no), "평균기온(°C)"] + df[(df$일시 == date + 1 & df$지점 == branch_no), "평균기온(°C)"]) / 2,
    `최저기온(°C)` = (df[(df$일시 == date - 1 & df$지점 == branch_no), "최저기온(°C)"] + df[(df$일시 == date + 1 & df$지점 == branch_no), "최저기온(°C)"]) / 2,
    `최고기온(°C)` = (df[(df$일시 == date - 1 & df$지점 == branch_no), "최고기온(°C)"] + df[(df$일시 == date + 1 & df$지점 == branch_no), "최고기온(°C)"]) / 2,
    `일강수량(mm)` = (df[(df$일시 == date - 1 & df$지점 == branch_no), "일강수량(mm)"] + df[(df$일시 == date + 1 & df$지점 == branch_no), "일강수량(mm)"]) / 2,
    `평균 풍속(m/s)` = (df[(df$일시 == date - 1 & df$지점 == branch_no), "평균 풍속(m/s)"] + df[(df$일시 == date + 1 & df$지점 == branch_no), "평균 풍속(m/s)"]) / 2
  )

  colnames(guro_data) <- c("지점", "일시", "평균기온(°C)", "최저기온(°C)", "최고기온(°C)", "일강수량(mm)", "평균 풍속(m/s)")

  return(guro_data)
}

################################################################################
# Column 정규화
################################################################################
normalize_column <- function(col_data) {
  return(rescale(col_data))
}

################################################################################
# Column 로그화
################################################################################
log_transform <- function(col_data) {
  return(log(col_data))
}