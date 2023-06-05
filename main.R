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
head(df)

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
### (1) 5월 데이터만 실험한 결과가 아래와 같습니다.
rf_model <- randomForest(train_x, train_y$rent, ntree = 100, type = "regression")
print(rf_model)

# 테스트 데이터 예측
predicted_y <- predict(rf_model, test_x)

mse <- mean((test_y$rent - predicted_y)^2)
print(mse) # 낮을 수록 좋음 > 여기서는 2174.692
calculate_r_squared(predicted_y, test_y$rent) # 1일 수록 좋음 > 여기서는 0.3889918

# 변수의 중요도 파악
varImpPlot(rf_model, type = 2, col = 1, cex = 1)


### (2) 5월 데이터만 실험한 결과가 아래와 같습니다.
rf_model <- randomForest(rent ~ avg_temperature + rainy + windy + no2_ppm + o3_ppm + co_ppm + so2_ppm + mise + chomise + holiday, data = train_set, importance = TRUE)
print(rf_model)

# 테스트 데이터 예측
predicted_y <- predict(rf_model, test_x)

mse <- mean((test_y$rent - predicted_y)^2)
print(mse) # 낮을 수록 좋음 > 여기서는 3139.815
calculate_r_squared(predicted_y, test_y$rent) # 1일 수록 좋음 > 여기서는 0.1112568

# 변수의 중요도 파악
varImpPlot(rf_model, type = 2, col = 1, cex = 1)
