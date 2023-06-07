################################################################################
# -1. vscode 사용 시에만 1회 실행
################################################################################
install.packages("languageserver")
install.packages("jsonlite")

################################################################################
# 0. 패키지 인스톨 - 1회 실행
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
# 1. 파일 로딩
################################################################################
source('libraries.R')     # 라이브러리
source('utils.R')         # 유틸리티
source('data.R')          # data file 관련 함수
source('eda.R')           # EDA 관련 함수
source('validation.R')    # 검증 관련 함수
source('visualization.R') # 시각화 관련 함수

################################################################################
# 결측치 확인용 예시 코드
################################################################################

#str(df)
#dim(df)
#summary(df)
#sum(is.na(df))
#df[!complete.cases(df), ]


################################################################################
# 2. 따릉이 데이터 선택
# '2.1' 이나 '2.2' 둘 중에 하나 선택하신 후, '2'로 넘어가세요.
#
# 따릉이 데이터는 아래 사이트에서 다운로드 후 data/ddarung/ 아래 복사.
#  - https://data.seoul.go.kr/dataList/OA-15246/F/1/datasetView.do#
################################################################################
################################################################################
# 2.1. 따릉이 2018 ~ 2022 전체 데이터 (대여일자, 대여소, 총이용건수)
################################################################################

df_2018 <- get_ddarung_daily_info_2018() # 데이터 수 : 439,061, 결측치 : 197, 결측치 row : 197
df_2019 <- get_ddarung_daily_info_2019() # 데이터 수 : 549,404, 결측치 : 0
df_2020 <- get_ddarung_daily_info_2020() # 데이터 수 : 661,776, 결측치 : 0
df_2021 <- get_ddarung_daily_info_2021() # 데이터 수 : 601,139, 결측치 : 0
df_2022 <- get_ddarung_daily_info_2022("full") # 데이터 수 : 945,346, 결측치 : 0

df_ddarung_full_data <- rbind(df_2018, df_2019, df_2020, df_2021, df_2022)

# 전체 데이터 수 : 3,196,736
# 결측치 수 : 197, 결측치 row: 197
# 결측치 제외 전체 데이터 수 : 3,196,539

################################################################################
# 2.2. 따릉이 대여소 하나의 2018 ~ 2022 데이터 (대여일자, 대여소, 총이용건수)
################################################################################

## 대여소 예시
## 2715 - 강서구, 마곡나루역 2번출구 대여소
## 502 - 광진구, 뚝섬유원지역 1번출구 대여소
## 대여소 번호는 "공공자전거 대여소 정보(22.12월 기준).csv" 파일 참조

station_no <- 502

df_2018 <- get_ddarung_daily_info_2018(station_no) # 데이터 수 : 184, 결측치 : 0
df_2019 <- get_ddarung_daily_info_2019(station_no) # 데이터 수 : 363, 결측치 : 0
df_2020 <- get_ddarung_daily_info_2020(station_no) # 데이터 수 : 366, 결측치 : 0
df_2021 <- get_ddarung_daily_info_2021(station_no) # 데이터 수 : 274, 결측치 : 0
df_2022 <- get_ddarung_daily_info_2022("full") # 데이터 수 : 945,356, 결측치 : 0
df_2022 <- df_2022[df_2022$대여소번호 == station_no, ] # 데이터 수 : 364, 결측치 : 0

df_ddarung_full_data <- rbind(df_2018, df_2019, df_2020, df_2021, df_2022)

# 전체 데이터 수 : 1,551
# 결측치 수 : 0
# 결측치 제외 전체 데이터 수 : 1,551

################################################################################
# 3. 날씨 데이터 준비 (2018 ~ 2022)
#  - 연도별 로딩은 2022년만 가능.
#  - 출처
#    - https://data.kma.go.kr/data/grnd/selectAsosRltmList.do?pgmNo=36
################################################################################
df_weather <- get_weather_info()

# 2022년 데이터만 불러오려면
# df_weather <- get_weather_info(2022)

# 전처리
## 1) 일강수량 결측치 존재 시 0으로 치환
## 2) 구로구 정보(2022-06-16, 2022-09-06)에 대한 결측치 삽입
df_weather <- pre_proc_weather(df_weather)

# 전체 데이터 수 : 51,340
# 결측치 수 : 0
# 결측치 제외 전체 데이터 수 : 51,340

################################################################################
# 4. 대기오염 데이터 준비 (2018 ~ 2022)
#  - 연도별 로딩 시, get_airpolution_info(x) 로 호출. x는 2018, 2019, 2020, 2021, 2022 중 택1.
#  - 출처
#    - http://data.seoul.go.kr/dataList/337/S/2/datasetView.do
################################################################################
df_airpolution <- get_airpolution_info()

# 2022년 데이터만 불러오려면
# df_airpolution <- get_airpolution_info(2022)

# 전처리
## 1) 결측치에 대해 0으로 치환
## 2) 측정일시 데이터 타입이 character로 되어있다면 date 로 변경
df_airpolution <- pre_proc_airpolustion(df_airpolution)

# 전체 데이터 수 : 87,182
# 결측치 수 : 0
# 결측치 제외 전체 데이터 수 : 87,182

################################################################################
# 5. 공휴일 데이터 준비 (2018 ~ 2022)
#  - 연도별 로딩은 2022년만 가능.
#  - 출처
#    - https://www.data.go.kr/data/15012690/openapi.do
################################################################################
df_holiday <- get_holiday_info() # 94개

# 2022년 데이터만 불러오려면
#df_holiday <- get_holiday_info(2022) # 20개

# 전체 데이터 수 : 94
# 결측치 수 : 0
# 결측치 제외 전체 데이터 수 : 94


################################################################################
# 6. 대여소와 날씨 정보 매칭을 위한 대여소 번호 데이터 준비 (2022. 12. 현재 최신 데이터)
#  - 출처
#    - https://data.seoul.go.kr/dataList/OA-13252/F/1/datasetView.do
#  - 매칭 데이터는 수동으로 만든 데이터.
################################################################################
df_station <- get_ddarung_station_info()

# 전체 데이터 수 : 2,719
# 결측치 수 : 0
# 결측치 제외 전체 데이터 수 : 2,719


df_matching_info <- get_matching_info()

# 전체 데이터 수 : 29
# 결측치 수 : 0
# 결측치 제외 전체 데이터 수 : 29


################################################################################
# 7. 데이터 결합
################################################################################

## 7.1. 따릉이 데이터 + 대여소 데이터
df_combined_1 <- merge(df_ddarung_full_data, df_station, by.x = "대여소번호", by.y = "대여소 번호", all.x = TRUE)


## 7.2. + 매칭 데이터 결합
df_combined_2 <- merge(df_combined_1, df_matching_info, by.x ="자치구", by.y = "지점명", all.x = TRUE)

### 전처리: 종로구는 날씨 측정소가 없으므로 용산구(415)로 치환.
df_combined_2[df_combined_2$자치구 == "종로구" & is.na(df_combined_2$지점), "지점"] <- 415 


## 7.3. + 날씨 데이터
df_combined_3 <- merge(df_combined_2, df_weather, by.x = c("대여일자", "지점"), by.y = c("일시", "지점"), all.x = TRUE)

### "502" 의 경우, 여기서 결측치 5 발생
df_combined_3[!complete.cases(df_combined_3), ]

#### "502" 의 경우,  1개의 row에서 결측치 발생 확인
#####       대여일자   지점 자치구 대여소번호 총이용건수 평균기온(°C) 최저기온(°C) 최고기온(°C) 일강수량(mm) 평균 풍속(m/s)
##### 675  2020-05-07  413 광진구    502       742       NA          NA          NA           NA             NA

#### "502" 의 경우, 해당 결측치들을 임의의 값으로 채우는 것 보다 아예 이 line을 없애는 것이 더 나을 것 같다고 판단함.
df_combined_3 <- df_combined_3[-675, ]


## 7.4. + 대기오염 데이터
df_combined_4 <- merge(df_combined_3, df_airpolution, by.x = c("대여일자", "자치구"), by.y = c("측정일시", "측정소명"), all.x = TRUE)


## 7.5. + 공휴일 데이터
df_combined_4$holiday <- "0" # 초기화 0 = 'N'
df_combined_4$holiday[ymd(df_combined_4$대여일자) %in% ymd(df_holiday$datetime)] <- "1" # 1 = 'Y'


################################################################################
# 8. 컬럼 전처리
################################################################################
## 자치구 삭제 (지점과 동일한 의미를 가지므로 분석 데이터량 감소를 위해)
df_combined_4 <- subset(df_combined_4, select = -c(자치구))

## RandomForest에서 특수문자를 이해하지 못하므로 컬럼명 변경
colnames(df_combined_4) <- c("datetime","branch", "branch_no", "rent", "avg_temperature", "low_temperature", "high_temperature", "rainy", "windy", "no2_ppm", "o3_ppm", "co_ppm", "so2_ppm", "part_matter", "ultra_part_matter", "holiday")

#############################
#### Data frame 준비 완료 ####
sum(is.na(df_combined_4))
dim(df_combined_4)
summary(df_combined_4)
str(df_combined_4)
df_combined_4[!complete.cases(df_combined_4), ]

#### 대여소번호 502 문제 없음 ####


#### 230607 ####
## TODO: 결측치 분석 필요
df_combined_4 <- na.omit(df_combined_4)
#############################

################################################################################
# 9. Split data
################################################################################

all_full_ready <- df_combined_4

## temp - 몇 가지 더 삭제 -> [502 case] 이후 1,550개 -> 1,073개 / 47개로 분할
all_full_ready <- subset(all_full_ready, select = -c(datetime, branch, branch_no))

sample_split = sample.split(Y = all_full_ready, SplitRatio = 0.7)

train_set <- subset(x = all_full_ready, sample_split == TRUE)
test_set <- subset(x = all_full_ready, sample_split == FALSE)

train_x <- subset(train_set, select = -c(rent))
train_y <- subset(train_set, select = c(rent))

test_x <- subset(test_set, select = -c(rent))
test_y <- subset(test_set, select = c(rent))

summary(all_full_ready)
dim(all_full_ready)
dim(train_set)
dim(train_x)
dim(train_y)
dim(test_set)
dim(test_x)
dim(test_y)

head(all_full_ready)

################################################################################
# 10. Random Forest
# 10.x 중 하나만 선택하여 수행하시면 됩니다.
################################################################################

## 10.1. 전체 변수 포함
### 10.1.1. ntree = 100
model_rf <- randomForest(rent ~ .,
                       data = train_set, ntree = 100, type = "regression", importance = TRUE)

    ### 10.1.1. and 502 case
      ### Mean of squared residuals: 26355.4
      ### % Var explained: 67.82

### 10.1.2. ntree = 500
  model_rf <- randomForest(rent ~ ., 
                     data = train_set, ntree = 500, type = "regression", importance = TRUE)

    ### 10.1.2. and 502 case
      ### Mean of squared residuals: 25583.59
      ### % Var explained: 68.76

## 10.2. 선택 변수만
### 10.2.1. ntree = 100
  model_rf <- randomForest(rent ~ avg_temperature + rainy + windy + no2_ppm + o3_ppm + co_ppm + so2_ppm + part_matter + ultra_part_matter + holiday, 
                     data = train_set, ntree = 100, type = "regression", importance = TRUE)

    ### 10.2.1. and 502 case
      ### Mean of squared residuals: 28826.67
      ### % Var explained: 64.8

### 10.2.2. ntree = 500
  model_rf <- randomForest(rent ~ avg_temperature + rainy + windy + no2_ppm + o3_ppm + co_ppm + so2_ppm + part_matter + ultra_part_matter + holiday, 
                     data = train_set, ntree = 500, type = "regression", importance = TRUE)

    ### 10.2.2. and 502 case
      ### Mean of squared residuals: 28555.16
      ### % Var explained: 65.14

### 결과 확인
print(model_rf)
summary(model_rf)

################################################################################
# 11. Validation
################################################################################

## 예측
predicted_y <- predict(model_rf, test_x)

## MSE 점수 확인 - 예측값과 실제값 사이의 제곱 오차의 평균을 계산합니다. 작을수록 좋은 모델입니다.
mse <- mean((test_y$rent - predicted_y)^2)
print(mse)

## MAE 점수 확인 - 예측값과 실제값 사이의 절댓값 오차의 평균을 계산합니다. 작을수록 좋은 모델입니다.
mae <- mean(abs(predicted_y - test_y$rent))
print(mae)

## MAPE 점수 확인 - 예측값과 실제값 사이의 절댓값 오차를 실제값으로 나눈 후 평균을 계산합니다. 작을수록 좋은 모델입니다.
mape <- mean(abs((predicted_y - test_y$rent) / test_y$rent)) * 100
print(mape)

## R-Squared 점수 확인 - 1일 수록 좋음
calculate_r_squared(predicted_y, test_y$rent)

## [시각화] 관련이 높은 변수 확인
varImpPlot(model_rf, type = 2, col = 1, cex = 1)

## [시각화] 잔차
residuals <- test_y$rent - predicted_y
print(residuals)

plot(predicted_y, residuals,
     main = "Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals")


################################################################################
# 12. 다중 선형 회귀 및 Validation
# 12.x 중 하나만 선택하여 수행하시면 됩니다.
################################################################################

## 12.1. 전체 변수 포함
### 12.1.1.
  model_lm <- lm(rent ~ ., data = train_set)
    ### 12.1.1. and 502 case
      ### p-value: < 2.2e-16

## 12.1. 선택 변수만
### 12.1.1.
  model_lm <- lm(rent ~ avg_temperature + rainy + windy + no2_ppm + o3_ppm + co_ppm + so2_ppm + part_matter + ultra_part_matter + holiday, data = train_set)
    ### 12.1.1. and 502 case
      ### Mean of squared residuals: 28826.67
      ### % Var explained: 64.8



## 값 확인
summary(model_lm)

## 예측
predicted_y <- predict(model_lm, test_x)

## MSE 점수 확인 - 예측값과 실제값 사이의 제곱 오차의 평균을 계산합니다. 작을수록 좋은 모델입니다.
mse <- mean((test_y$rent - predicted_y)^2)
print(mse)

## MAE 점수 확인 - 예측값과 실제값 사이의 절댓값 오차의 평균을 계산합니다. 작을수록 좋은 모델입니다.
mae <- mean(abs(predicted_y - test_y$rent))
print(mae)

## MAPE 점수 확인 - 예측값과 실제값 사이의 절댓값 오차를 실제값으로 나눈 후 평균을 계산합니다. 작을수록 좋은 모델입니다.
mape <- mean(abs((predicted_y - test_y$rent) / test_y$rent)) * 100
print(mape)

## R-Squared 점수 확인 - 1일 수록 좋음
calculate_r_squared(predicted_y, test_y$rent)

## [시각화] 관련이 높은 변수 확인
varImpPlot(model_lm, type = 2, col = 1, cex = 1)

## [시각화] 잔차
residuals <- test_y$rent - predicted_y
print(residuals)

plot(predicted_y, residuals,
     main = "Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals")