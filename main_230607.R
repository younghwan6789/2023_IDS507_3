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
install.packages("car") # 다중공선성 확인 - VIF 값


################################################################################
# 1. 파일 로딩
################################################################################
source("libraries.R")     # 라이브러리
source("utils.R")         # 유틸리티
source("data.R")          # data file 관련 함수
source("eda.R")           # EDA 관련 함수
source("validation.R")    # 검증 관련 함수
source("visualization.R") # 시각화 관련 함수


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

df_2018 <- get_ddarung_daily_info_2018() # 데이터: 439,061, 결측치: 197, 결측치row: 197
df_2019 <- get_ddarung_daily_info_2019() # 데이터: 549,404, 결측치: 0
df_2020 <- get_ddarung_daily_info_2020() # 데이터: 661,776, 결측치: 0
df_2021 <- get_ddarung_daily_info_2021() # 데이터: 601,139, 결측치: 0
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
#  - 연도별 로딩 시, get_airpolution_info(x) 로 호출.
#    x는 2018, 2019, 2020, 2021, 2022 중 택1.
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
df_combined_1 <- merge(df_ddarung_full_data, df_station,
                        by.x = "대여소번호", by.y = "대여소 번호", all.x = TRUE)


## 7.2. + 매칭 데이터 결합
df_combined_2 <- merge(df_combined_1, df_matching_info,
                        by.x ="자치구", by.y = "지점명", all.x = TRUE)

### 전처리: 종로구는 날씨 측정소가 없으므로 용산구(415)로 치환.
df_combined_2[df_combined_2$자치구 == "종로구" & is.na(df_combined_2$지점), "지점"] <- 415


## 7.3. + 날씨 데이터
df_combined_3 <- merge(df_combined_2, df_weather,
                        by.x = c("대여일자", "지점"), by.y = c("일시", "지점"),
                        all.x = TRUE)

### "502" 의 경우, 여기서 결측치 5 발생 -> 1개의 row
df_combined_3[!complete.cases(df_combined_3), ]

#### "502" 의 경우,  1개의 row에서 결측치 발생 확인
#####       대여일자   지점 자치구 대여소번호 총이용건수 평균기온(°C) 최저기온(°C) 최고기온(°C) 일강수량(mm) 평균 풍속(m/s)
##### 675  2020-05-07  413 광진구    502       742       NA          NA          NA           NA             NA

#### "502" 의 경우, 해당 결측치들을 임의의 값으로 채우는 것 보다 아예 이 line을 없애는 것이 더 나을 것 같다고 판단함.
df_combined_3 <- df_combined_3[-675, ]


## 7.4. + 대기오염 데이터
df_combined_4 <- merge(df_combined_3, df_airpolution,
                        by.x = c("대여일자", "자치구"), by.y = c("측정일시", "측정소명"),
                        all.x = TRUE)


## 7.5. + 공휴일 데이터
# 초기화 0 = 'N', 1 = 'Y'
df_combined_4$holiday <- "0"
df_combined_4$holiday[ymd(df_combined_4$대여일자) %in% ymd(df_holiday$datetime)] <- "1"


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

## holiday를 숫자형으로 변경
all_full_ready$holiday <- as.numeric(all_full_ready$holiday)
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
# 10. 다중 공선성 확인
################################################################################
# VIF 계산
vif_values <- vif(lm(rent ~ ., data = train_set))

# VIF 출력
print(vif_values)

  # avg_temperature   low_temperature  high_temperature             rainy
  #      397.758330        120.748050        118.457185          1.153421
  #           windy           no2_ppm            o3_ppm            co_ppm
  #        1.891346          4.368845          2.120304          2.524933
  #         so2_ppm       part_matter ultra_part_matter           holiday
  #        1.480916          2.950705          4.367486          1.025162

# VIF (Variance Inflation Factor) 값은 다중공선성을 평가하기 위해 사용되는 지표입니다.
# 일반적으로 VIF 값이 1보다 작거나 1에 가까우면 다중공선성의 문제가 거의 없다고 판단됩니다.
# VIF 값이 1보다 큰 경우, 변수들 간에 상관 관계가 높아 다중공선성의 가능성이 높아집니다.
# 보통 5 또는 10 이상의 VIF 값이 있는 변수들은 다중공선성의 문제가 있을 수 있습니다.

# 해당 결과를 해석하면 다음과 같습니다:

# avg_temperature: 397.758330
# low_temperature: 120.748050
# high_temperature: 118.457185
# rainy: 1.153421
# windy: 1.891346
# no2_ppm: 4.368845
# o3_ppm: 2.120304
# co_ppm: 2.524933
# so2_ppm: 1.480916
# part_matter: 2.950705
# ultra_part_matter: 4.367486
# holiday: 1.025162
# 해석하자면, avg_temperature 변수의 VIF 값은 397.758330으로 매우 높으며, low_temperature과 high_temperature 변수도 다중공선성이 높게 나타납니다. 반면, rainy, windy, so2_ppm, holiday 변수의 VIF 값은 모두 1에 가깝기 때문에 다중공선성의 문제는 거의 없다고 볼 수 있습니다.

# 다중공선성이 높은 변수들은 모델에 포함되어 있을 경우 예측 성능에 부정적인 영향을 미칠 수 있으므로, 이를 고려하여 변수 선택이나 다른 조치를 취할 필요가 있습니다.



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


# Call:
#  randomForest(formula = rent ~ avg_temperature + rainy + windy +      no2_ppm + o3_ppm + co_ppm + so2_ppm + part_matter + ultra_part_matter +      holiday, data = train_set, ntree = 500, type = "regression",      importance = TRUE)
#                Type of random forest: regression
#                      Number of trees: 500
# No. of variables tried at each split: 3

#           Mean of squared residuals: 27420.4
#                     % Var explained: 65.49
# > summary(model_rf)
#                 Length Class  Mode
# call               6   -none- call
# type               1   -none- character
# predicted       1073   -none- numeric
# mse              500   -none- numeric
# rsq              500   -none- numeric
# oob.times       1073   -none- numeric
# importance        20   -none- numeric
# importanceSD      10   -none- numeric
# localImportance    0   -none- NULL
# proximity          0   -none- NULL
# ntree              1   -none- numeric  
# mtry               1   -none- numeric
# forest            11   -none- list
# coefs              0   -none- NULL
# y               1073   -none- numeric
# test               0   -none- NULL
# inbag              0   -none- NULL
# terms              3   terms  call

# 해당 결과를 해석하면 다음과 같습니다:

# 모델의 호출(Call) 정보를 나타내고 있으며, 사용된 데이터셋과 사용된 변수들의 정보를 확인할 수 있습니다.
# 모델의 유형(Type)은 회귀(regression) 모델임을 나타냅니다.
# 500개의 트리(Number of trees)를 사용하여 모델이 생성되었습니다.
# 각 분할 시 시도하는 변수의 수(No. of variables tried at each split)는 3개입니다.
# 평균 제곱 잔차(Mean of squared residuals)는 27420.4로 나타나며, 모델의 예측 성능을 평가하는 지표입니다.
# 값이 작을수록 모델의 예측이 실제 데이터와 일치한다고 볼 수 있습니다.
# 변수들이 종속 변수의 변동을 얼마나 설명하는지를 나타내는 설명력(Var explained)은 65.49%입니다.
# 또한, summary() 함수의 결과는 다양한 정보를 담고 있으며, 해당 결과에서는 추가적인 정보가 제공되지 않고 있습니다.

################################################################################
# 11. Validation
################################################################################

## 예측
predicted_y <- predict(model_rf, test_x)

## MSE, MAE, MAPE 점수 확인
calculate_mse_mae_mape(test_y$rent, predicted_y)

# [1] "MSE :  31624.7298286794"
# [1] "MAE :  115.266445667195"
# [1] "MAPE:  59.6474251909169"

## R-Squared 점수 확인 - 1일 수록 좋음
calculate_r_squared(predicted_y, test_y$rent)

# [1] 0.6313917

## [시각화] 관련이 높은 변수 확인
varImpPlot(model_rf, type = 2, col = 1, cex = 1)

## [시각화] 잔차
view_residual_plot(test_y$rent, predicted_y)


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

# Call:
# lm(formula = rent ~ avg_temperature + rainy + windy + no2_ppm +
#     o3_ppm + co_ppm + so2_ppm + part_matter + ultra_part_matter +
#     holiday, data = train_set)

# Residuals:
#     Min      1Q  Median      3Q     Max
# -529.02 -120.07  -36.49   77.46  974.38

# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)
# (Intercept)        2.306e+02  4.830e+01   4.774 2.06e-06 ***
# avg_temperature    9.250e+00  1.039e+00   8.905  < 2e-16 ***
# rainy             -4.723e+00  4.534e-01 -10.416  < 2e-16 ***
# windy             -6.899e+01  1.689e+01  -4.085 4.74e-05 ***
# no2_ppm            1.522e+03  1.205e+03   1.264  0.20664
# o3_ppm             8.854e+03  6.672e+02  13.271  < 2e-16 ***
# co_ppm            -4.477e+01  4.841e+01  -0.925  0.35524
# so2_ppm           -2.961e+04  5.928e+03  -4.996 6.86e-07 ***
# part_matter        1.172e+00  4.464e-01   2.626  0.00875 **
# ultra_part_matter -3.720e+00  9.528e-01  -3.905  0.00010 ***
# holiday1           1.521e+02  2.816e+01   5.402 8.13e-08 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 208.4 on 1062 degrees of freedom
# Multiple R-squared:  0.459,     Adjusted R-squared:  0.4539
# F-statistic: 90.11 on 10 and 1062 DF,  p-value: < 2.2e-16





## 예측
predicted_y <- predict(model_lm, test_x)

## MSE, MAE, MAPE 점수 확인
calculate_mse_mae_mape(test_y$rent, predicted_y)

# [1] "MSE :  48988.2584683718"
# [1] "MAE :  154.436750983967"
# [1] "MAPE:  91.2198709329412"

## R-Squared 점수 확인 - 1일 수록 좋음
calculate_r_squared(predicted_y, test_y$rent)

# [1] 0.4293177

## [시각화] 잔차
view_residual_plot(test_y$rent, predicted_y)


################################################################################
# 13. GLM 및 Validation
################################################################################

# 정규성을 알아보기 위한 qq 플롯 그리기
qqnorm(all_full_ready$rent)  # 정규성 검정을 위한 QQ 플롯
qqline(all_full_ready$rent)  # 기대되는 정규분포와 비교하는 선 그리기
# 502 데이터의 경우, 이 정도면 정규성을 따른다고 보임.

# 정규성을 따른다면, glm 모델 적용 가능.
## 13.1. 전체 변수 포함
model_glm <- glm(rent ~ ., data = train_set, family = gaussian)

# 모델 요약
summary(model_glm)

# Call:
# glm(formula = rent ~ ., family = gaussian, data = train_set)

# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)
# (Intercept)        5.671e+01  4.719e+01   1.202 0.229649
# avg_temperature    2.279e+00  1.172e+01   0.194 0.845925    
# low_temperature   -2.771e+01  6.388e+00  -4.338 1.57e-05 ***
# high_temperature   3.298e+01  6.127e+00   5.383 9.04e-08 ***
# rainy             -3.226e+00  4.384e-01  -7.359 3.72e-13 ***
# windy             -5.457e+01  1.578e+01  -3.459 0.000564 ***
# no2_ppm           -2.823e+03  1.183e+03  -2.387 0.017142 *
# o3_ppm             6.534e+03  6.476e+02  10.091  < 2e-16 ***
# co_ppm             1.535e+01  4.538e+01   0.338 0.735163
# so2_ppm           -2.462e+04  5.537e+03  -4.445 9.69e-06 ***
# part_matter        4.035e-01  4.203e-01   0.960 0.337286
# ultra_part_matter -2.591e+00  8.945e-01  -2.896 0.003852 ** 
# holiday1           1.421e+02  2.632e+01   5.396 8.40e-08 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# (Dispersion parameter for gaussian family taken to be 37689.57)

#     Null deviance: 85249838  on 1072  degrees of freedom
# Residual deviance: 39950946  on 1060  degrees of freedom
# AIC: 14366

# Number of Fisher Scoring iterations: 2

# 해당 결과는 일반화 선형 모델인 glm의 요약 정보를 나타냅니다.

# 모델의 호출(Call) 정보를 나타내고 있으며, 사용된 데이터셋과 사용된 변수들의 정보를 확인할 수 있습니다.
# 각 변수의 회귀 계수(Coefficients)가 나타나며, 변수들과 회귀 계수의 추정치(Estimate), 표준 오차(Std. Error), t-value, p-value가 제공됩니다.
# 회귀 계수는 해당 변수가 종속 변수에 미치는 영향의 크기와 방향을 나타냅니다.
# 모델의 유의성을 평가하기 위한 p-value를 나타냅니다.
# 유의수준 0.05보다 작은 p-value는 해당 변수가 종속 변수에 유의미한 영향을 미친다는 것을 나타냅니다.
# Signif. codes는 p-value 값에 따라 유의성 수준을 나타내는 기호입니다.
# 모델의 분산 파라미터(Dispersion parameter)는 37689.57로 나타나며, 가우시안 분포를 따른다고 가정한 경우의 분산을 나타냅니다.
# Null deviance는 모델이 종속 변수를 예측하지 않을 때의 오차 제곱합입니다. Residual deviance는 모델의 잔차 제곱합을 나타냅니다.
# 두 값의 차이는 모델의 예측 능력을 나타내는 지표입니다.
# AIC(Akaike Information Criterion)는 모델의 상대적인 품질을 평가하는 지표입니다. 값이 작을수록 모델의 품질이 좋다고 판단할 수 있습니다.
# Fisher Scoring iterations은 모델의 최적화를 위해 수행된 반복 횟수를 나타냅니다.


## 13.2. 선택 변수만
model_glm <- glm(rent ~ avg_temperature + rainy + windy + no2_ppm + o3_ppm + co_ppm + so2_ppm + part_matter + ultra_part_matter + holiday, data = train_set, family = gaussian)

# 모델 요약
summary(model_glm)


# Call:
# glm(formula = rent ~ avg_temperature + rainy + windy + no2_ppm +
#     o3_ppm + co_ppm + so2_ppm + part_matter + ultra_part_matter +
#     holiday, family = gaussian, data = train_set)

# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)
# (Intercept)        2.306e+02  4.830e+01   4.774 2.06e-06 ***
# avg_temperature    9.250e+00  1.039e+00   8.905  < 2e-16 ***
# rainy             -4.723e+00  4.534e-01 -10.416  < 2e-16 ***
# windy             -6.899e+01  1.689e+01  -4.085 4.74e-05 ***
# no2_ppm            1.522e+03  1.205e+03   1.264  0.20664
# o3_ppm             8.854e+03  6.672e+02  13.271  < 2e-16 ***
# co_ppm            -4.477e+01  4.841e+01  -0.925  0.35524
# so2_ppm           -2.961e+04  5.928e+03  -4.996 6.86e-07 ***
# part_matter        1.172e+00  4.464e-01   2.626  0.00875 **
# ultra_part_matter -3.720e+00  9.528e-01  -3.905  0.00010 ***
# holiday1           1.521e+02  2.816e+01   5.402 8.13e-08 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# (Dispersion parameter for gaussian family taken to be 43426.17)

#     Null deviance: 85249838  on 1072  degrees of freedom
# Residual deviance: 46118591  on 1062  degrees of freedom
# AIC: 14516

# Number of Fisher Scoring iterations: 2



# 해당 결과는 일반화 선형 모델인 glm의 요약 정보를 나타냅니다.

# 모델의 호출(Call) 정보를 나타내고 있으며, 사용된 데이터셋과 사용된 변수들의 정보를 확인할 수 있습니다.
# 각 변수의 회귀 계수(Coefficients)가 나타나며, 변수들과 회귀 계수의 추정치(Estimate), 표준 오차(Std. Error), t-value, p-value가 제공됩니다.
# 회귀 계수는 해당 변수가 종속 변수에 미치는 영향의 크기와 방향을 나타냅니다.
# 모델의 유의성을 평가하기 위한 p-value를 나타냅니다.
# 유의수준 0.05보다 작은 p-value는 해당 변수가 종속 변수에 유의미한 영향을 미친다는 것을 나타냅니다.
# Signif. codes는 p-value 값에 따라 유의성 수준을 나타내는 기호입니다.
# 모델의 분산 파라미터(Dispersion parameter)는 43426.17로 나타나며, 가우시안 분포를 따른다고 가정한 경우의 분산을 나타냅니다.
# Null deviance는 모델이 종속 변수를 예측하지 않을 때의 오차 제곱합입니다. Residual deviance는 모델의 잔차 제곱합을 나타냅니다.
# 두 값의 차이는 모델의 예측 능력을 나타내는 지표입니다.
# AIC(Akaike Information Criterion)는 모델의 상대적인 품질을 평가하는 지표입니다. 값이 작을수록 모델의 품질이 좋다고 판단할 수 있습니다.
# Fisher Scoring iterations은 모델의 최적화를 위해 수행된 반복 횟수를 나타냅니다.


## 예측
predicted_y <- predict(model_glm, test_x)

## MSE, MAE, MAPE 점수 확인
calculate_mse_mae_mape(test_y$rent, predicted_y)

# [1] "MSE :  48988.2584683718"
# [1] "MAE :  154.436750983967"
# [1] "MAPE:  91.2198709329412"

## R-Squared 점수 확인 - 1일 수록 좋음
calculate_r_squared(predicted_y, test_y$rent)

# [1] 0.4293177

## [시각화] 잔차
view_residual_plot(test_y$rent, predicted_y)

head(all_full_ready)
summary(all_full_ready)

# 데이터프레임에서 변수들 간의 상관 행렬 계산
cor_matrix <- cor(all_full_ready)

# 상관 행렬 출력
print(cor_matrix)
