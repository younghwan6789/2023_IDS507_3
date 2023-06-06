################################################################################
# 1. 패키지 인스톨 - 1회 실행
################################################################################
install.packages("readr")
install.packages("dplyr")
install.packages("lubridate") # ymd() 사용을 위해
install.packages("xml2") # 공휴일 정보 xml 파일 입력을 위해
install.packages("ggplot2") # 그래프 그릴 때
install.packages("GGally") # 산점도 행렬
install.packages("randomForest")
install.packages("caTools") # train/test set 분류
install.packages("caret") # confusion matrix
install.packages("pROC")
install.packages("scales") # 정규화

################################################################################
# 2. 파일 로딩
################################################################################
source('libraries.R')     # 라이브러리
source('utils.R')         # 유틸리티
source('data.R')          # data file 관련 함수
source('eda.R')           # EDA 관련 함수
source('validation.R')    # 검증 관련 함수
source('visualization.R') # 시각화 관련 함수

################################################################################
# 3. 통합 데이터 파일 로딩
################################################################################
df = get_full_merged_info()

str(df)
summary(df)
sum(is.na(df))

### 정규화
columns <- c("avg_temperature", "low_temperature", "high_temperature", "rainy", "windy", "no2_ppm", "o3_ppm", "co_ppm", "so2_ppm", "part_matter", "ultra_part_matter")
df <- as.data.frame(lapply(df[columns], rescale))

################################################################################
# 4. 데이터 분할
################################################################################
sample_split = sample.split(Y = df, SplitRatio = 0.7)

train_set <- subset(x = df, sample_split == TRUE)
test_set <- subset(x = df, sample_split == FALSE)

train_x <- subset(train_set, select = -c(rent))
train_y <- subset(train_set, select = c(rent))

test_x <- subset(test_set, select = -c(rent))
test_y <- subset(test_set, select = c(rent))

################################################################################
# 5. Random Forest
################################################################################

#### 5.1. 그냥 테스트
rf_model <- randomForest(train_x, train_y$rent, type = "regression")
print(rf_model)

predicted_y <- predict(rf_model, test_x)

mse <- mean((test_y$rent - predicted_y)^2)
print(mse) # 낮을 수록 좋음

calculate_r_squared(predicted_y, test_y$rent) # 1일 수록 좋음

varImpPlot(rf_model, type = 2, col = 1, cex = 1)

# 잔차 계산
residuals <- test_y$rent - predicted_y
print(residuals)
# 잔차 시각화 - MSE 점수 확인
plot(predicted_y, residuals,
     main = "Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals")


#### 5.2. ntree 100 테스트

do_randomForest <- function(x, y) {
  start_time = Sys.time()
  print(paste("Start   :", start_time))
  rf <- randomForest(x, y, ntree = 100, type = "regression")
  finish_time = Sys.time()
  print(paste("Finish  :", finish_time))
  print(paste("Elapsed :", round(difftime(finish_time, start_time, units = "secs"), 3), "seconds"))
  
  return(rf)
}

rf_model <- do_randomForest(train_x, train_y$rent)
print(rf_model)

predicted_y <- predict(rf_model, test_x)

mse <- mean((test_y$rent - predicted_y)^2)
print(mse) # 낮을 수록 좋음

calculate_r_squared(predicted_y, test_y$rent) # 1일 수록 좋음

varImpPlot(rf_model, type = 2, col = 1, cex = 1)

# 잔차 계산
residuals <- test_y$rent - predicted_y
print(residuals)
# 잔차 시각화 - MSE 점수 확인
plot(predicted_y, residuals,
     main = "Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals")
####



#### 5.3. ntree 100, 변수를 줄여서 테스트

do_randomForest_with_some_var <- function(train_set) {
  start_time = Sys.time()
  print(paste("Start   :", start_time))

  rf <- randomForest(rent ~ avg_temperature + rainy + windy + no2_ppm + o3_ppm + co_ppm + so2_ppm + part_matter + ultra_part_matter + holiday, 
                             data = train_set, ntree = 100, type = "regression") #importance = TRUE
  
  finish_time = Sys.time()
  print(paste("Finish  :", finish_time))
  print(paste("Elapsed :", round(difftime(finish_time, start_time, units = "secs"), 3), "seconds"))

  return(rf)
}

rf_model <- do_randomForest_with_some_var(train_set)
print(rf_model)

predicted_y <- predict(rf_model, test_x)

mse <- mean((test_y$rent - predicted_y)^2)
print(mse) # 낮을 수록 좋음

calculate_r_squared(predicted_y, test_y$rent) # 1일 수록 좋음

varImpPlot(rf_model, type = 2, col = 1, cex = 1)

# 잔차 계산
residuals <- test_y$rent - predicted_y
print(residuals)
# 잔차 시각화 - MSE 점수 확인
plot(predicted_y, residuals,
     main = "Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals")
####



#### 5.4. ntree 100, 정규화 후, 변수를 줄여서 테스트

########### 정규화
#columns <- c("avg_temperature", "low_temperature", "high_temperature", "rainy", "windy", "no2_ppm", "o3_ppm", "co_ppm", "so2_ppm", "part_matter", "ultra_part_matter")
df_normalized <- df

normalize_column <- function(col_data) {
  return(rescale(col_data))
}

df_normalized$avg_temperature <- normalize_column(df_normalized$avg_temperature)
df_normalized$low_temperature <- normalize_column(df_normalized$low_temperature)
df_normalized$high_temperature <- normalize_column(df_normalized$high_temperature)
df_normalized$rainy <- normalize_column(df_normalized$rainy)
df_normalized$windy <- normalize_column(df_normalized$windy)
df_normalized$no2_ppm <- normalize_column(df_normalized$no2_ppm)
df_normalized$o3_ppm <- normalize_column(df_normalized$o3_ppm)
df_normalized$co_ppm <- normalize_column(df_normalized$co_ppm)
df_normalized$so2_ppm <- normalize_column(df_normalized$so2_ppm)
df_normalized$part_matter <- normalize_column(df_normalized$part_matter)
df_normalized$ultra_part_matter <- normalize_column(df_normalized$ultra_part_matter)


sample_split = sample.split(Y = df_normalized, SplitRatio = 0.7)

train_set <- subset(x = df_normalized, sample_split == TRUE)
test_set <- subset(x = df_normalized, sample_split == FALSE)

train_x <- subset(train_set, select = -c(rent))
train_y <- subset(train_set, select = c(rent))

test_x <- subset(test_set, select = -c(rent))
test_y <- subset(test_set, select = c(rent))


###########


do_randomForest_with_some_normalized_var <- function(train_set) {
  start_time = Sys.time()
  print(paste("Start   :", start_time))
  
  rf <- randomForest(rent ~ avg_temperature + rainy + windy + no2_ppm + o3_ppm + co_ppm + so2_ppm + part_matter + ultra_part_matter + holiday, 
                     data = train_set, ntree = 100, type = "regression") #importance = TRUE
  
  finish_time = Sys.time()
  print(paste("Finish  :", finish_time))
  print(paste("Elapsed :", round(difftime(finish_time, start_time, units = "secs"), 3), "seconds"))
  
  return(rf)
}

rf_model <- do_randomForest_with_some_normalized_var(train_set)
print(rf_model)

predicted_y <- predict(rf_model, test_x)

mse <- mean((test_y$rent - predicted_y)^2)
print(mse) # 낮을 수록 좋음

calculate_r_squared(predicted_y, test_y$rent) # 1일 수록 좋음

varImpPlot(rf_model, type = 2, col = 1, cex = 1)

# 잔차 계산
residuals <- test_y$rent - predicted_y
print(residuals)
# 잔차 시각화 - MSE 점수 확인
plot(predicted_y, residuals,
     main = "Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals")
####
start_time = Sys.time()
finish_time = Sys.time()
print(paste("Elapsed :", finish_time - start_time))


#### 5.5. ntree 100, 정규화 후, 변수를 더 줄여서 테스트

do_randomForest_with_some_normalized_var <- function(train_set) {
  start_time = Sys.time()
  print(paste("Start   :", start_time))
  
  rf <- randomForest(rent ~ avg_temperature + rainy + windy + part_matter + holiday, 
                     data = train_set, ntree = 100, type = "regression") #importance = TRUE
  
  finish_time = Sys.time()
  print(paste("Finish  :", finish_time))
  print(paste("Elapsed :", round(difftime(finish_time, start_time, units = "secs"), 3), "seconds"))
  
  return(rf)
}

rf_model <- do_randomForest_with_some_normalized_var(train_set)
print(rf_model)

predicted_y <- predict(rf_model, test_x)

mse <- mean((test_y$rent - predicted_y)^2)
print(mse) # 낮을 수록 좋음

calculate_r_squared(predicted_y, test_y$rent) # 1일 수록 좋음

varImpPlot(rf_model, type = 2, col = 1, cex = 1)

# 잔차 계산
residuals <- test_y$rent - predicted_y
print(residuals)
# 잔차 시각화 - MSE 점수 확인
plot(predicted_y, residuals,
     main = "Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals")
####
