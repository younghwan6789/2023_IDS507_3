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
# 0. 파일 로딩
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
# 1. 따릉이 데이터 선택
# '1.1' 이나 '1.2' 둘 중에 하나 선택하신 후, '2'로 넘어가세요.
#
# 따릉이 데이터는 아래 사이트에서 다운로드 후 data/ddarung/ 아래 복사.
#  - https://data.seoul.go.kr/dataList/OA-15246/F/1/datasetView.do#
################################################################################
################################################################################
# 1.1. 따릉이 2018 ~ 2022 전체 데이터 (대여일자, 대여소, 총이용건수)
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
# 1.2. 따릉이 대여소 하나의 2018 ~ 2022 데이터 (대여일자, 대여소, 총이용건수)
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
# 2. 날씨 데이터 준비 (2018 ~ 2022)
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


################################################################################
# 3. 대기오염 데이터 준비 (2018 ~ 2022)
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


################################################################################
# 4. 공휴일 데이터 준비 (2018 ~ 2022)
#  - 연도별 로딩은 2022년만 가능.
#  - 출처
#    - https://www.data.go.kr/data/15012690/openapi.do
################################################################################
df_holiday <- get_holiday_info() # 94개

# 2022년 데이터만 불러오려면
#df_holiday <- get_holiday_info(2022) # 20개


################################################################################
# 5. 대여소와 날씨 정보 매칭을 위한 대여소 번호 데이터 준비 (2022. 12. 현재 최신 데이터)
#  - 출처
#    - https://data.seoul.go.kr/dataList/OA-13252/F/1/datasetView.do
#  - 매칭 데이터는 수동으로 만든 데이터.
################################################################################
df_station <- get_ddarung_station_info()
df_matching_info <- get_matching_info()


################################################################################
# 6. 데이터 결합
################################################################################

## 6.1. 따릉이 데이터 + 대여소 데이터
df_combined_1 <- merge(df_ddarung_full_data, df_station, by.x = "대여소번호", by.y = "대여소 번호", all.x = TRUE)

## 6.2. + 매칭 데이터 결합
df_combined_2 <- merge(df_combined_1, df_matching_info, by.x ="자치구", by.y = "지점명", all.x = TRUE)

### 전처리: 종로구는 날씨 측정소가 없으므로 용산구(415)로 치환.
df_combined_2[df_combined_2$자치구 == "종로구" & is.na(df_combined_2$지점), "지점"] <- 415 

## 6.3. + 날씨 데이터
df_combined_3 <- merge(df_combined_2, df_weather, by.x = c("대여일자", "지점"), by.y = c("일시", "지점"), all.x = TRUE)

## 6.4. + 대기오염 데이터
df_combined_4 <- merge(df_combined_3, df_airpolution, by.x = c("대여일자", "자치구"), by.y = c("측정일시", "측정소명"), all.x = TRUE)

## 6.5. + 공휴일 데이터
df_combined_4$holiday <- "0" # 초기화 0 = 'N'
df_combined_4$holiday[ymd(df_combined_4$대여일자) %in% ymd(df_holiday$datetime)] <- "1" # 1 = 'Y'


################################################################################
# 7. 컬럼 전처리
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

#### 230607 ####
## TODO: 결측치 분석 필요
df_combined_4 <- na.omit(df_combined_4)
#############################

################################################################################
# 8. Split data
################################################################################

all_full_ready <- df_combined_4

sample_split = sample.split(Y = all_full_ready, SplitRatio = 0.7)

train_set <- subset(x = all_full_ready, sample_split == TRUE)
test_set <- subset(x = all_full_ready, sample_split == FALSE)

train_x <- subset(train_set, select = -c(rent))
train_y <- subset(train_set, select = c(rent))

test_x <- subset(test_set, select = -c(rent))
test_y <- subset(test_set, select = c(rent))

################################################################################
# 8. Random Forest
# 8.x 중 하나만 선택하여 수행하시면 됩니다.
################################################################################

## 8.1. 전체 변수 포함
### 8.1.1. ntree = 100
  model_rf <- randomForest(rent ~ ., 
                     data = train_set, ntree = 100, type = "regression", importance = TRUE)

### 8.1.2. ntree = 500
  model_rf <- randomForest(rent ~ ., 
                     data = train_set, ntree = 500, type = "regression", importance = TRUE)

## 8.2. 선택 변수만
### 8.2.1. ntree = 100
  model_rf <- randomForest(rent ~ avg_temperature + rainy + windy + no2_ppm + o3_ppm + co_ppm + so2_ppm + part_matter + ultra_part_matter + holiday, 
                     data = train_set, ntree = 100, type = "regression", importance = TRUE)

### 8.2.2. ntree = 500
  model_rf <- randomForest(rent ~ avg_temperature + rainy + windy + no2_ppm + o3_ppm + co_ppm + so2_ppm + part_matter + ultra_part_matter + holiday, 
                     data = train_set, ntree = 500, type = "regression", importance = TRUE)


### 결과 확인
print(model_rf)

################################################################################
# 9. Validation
################################################################################

## 예측
predicted_y <- predict(model_rf, test_x)

## MSE 점수 확인 - 낮을 수록 좋음
mse <- mean((test_y$rent - predicted_y)^2)
print(mse)

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

