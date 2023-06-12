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
install.packages("corrplot") # 상관관계 그래프


################################################################################
# 1. 파일 로딩
################################################################################
source("libraries.R") # 라이브러리
source("utils.R") # 유틸리티
source("data.R") # data file 관련 함수
source("eda.R") # EDA 관련 함수
source("validation.R") # 검증 관련 함수
source("visualization.R") # 시각화 관련 함수


################################################################################
# 결측치 확인용 예시 코드
################################################################################

# str(df)
# dim(df)
# summary(df)
# sum(is.na(df))
# df[!complete.cases(df), ]


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
## 대여소 번호가 결측치이므로, 삭제를 선택
# 결측치 제외 전체 데이터 수 : 3,196,539

####
a <- df_ddarung_full_data[df_ddarung_full_data$대여소번호 == 2715, ]
a <- na.omit(a)
# 841
a <- df_ddarung_full_data[df_ddarung_full_data$대여소번호 == 502, ]
a <- na.omit(a)
dim(a)
# 1730
a <- df_ddarung_full_data[df_ddarung_full_data$대여소번호 == 207, ]
# 1730

a <- df_ddarung_full_data[df_ddarung_full_data$대여소번호 == 4217, ]
# 413
a <- na.omit(a)
dim(a)

a <- df_ddarung_full_data[df_ddarung_full_data$대여소번호 == 1210, ]
# 1730
a <- na.omit(a)
dim(a)

a <- df_ddarung_full_data[df_ddarung_full_data$대여소번호 == 4217, ]
# 1730
a <- na.omit(a)
dim(a)

a <- df_ddarung_full_data[df_ddarung_full_data$대여소번호 == 2102, ]
# 1730
a <- na.omit(a)
dim(a)
####

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
## 1) 일강수량 결측치 존재 시 0으로 대체
## 2) 평균 풍속, 최저 기온, 최고 기온, 평균 기온은 해당 일의 다른 지역 평균값으로 대체
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

# 결측치 10,493

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
# df_holiday <- get_holiday_info(2022) # 20개

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
  by.x = "대여소번호", by.y = "대여소 번호", all.x = TRUE
)


## 7.2. + 매칭 데이터 결합
df_combined_2 <- merge(df_combined_1, df_matching_info,
  by.x = "자치구", by.y = "지점명", all.x = TRUE
)

### 전처리: 종로구는 날씨 측정소가 없으므로 용산구(415)로 치환.
df_combined_2[df_combined_2$자치구 == "종로구" & is.na(df_combined_2$지점), "지점"] <- 415


## 7.3. + 날씨 데이터
df_combined_3 <- merge(df_combined_2, df_weather,
  by.x = c("대여일자", "지점"), by.y = c("일시", "지점"),
  all.x = TRUE
)

### "502" 의 경우, 여기서 결측치 10 발생 -> 2개의 row
df_combined_3[!complete.cases(df_combined_3), ]

#### "502" 의 경우,  2개의 row에서 결측치 발생 확인
#####       대여일자   지점 자치구 대여소번호 총이용건수 평균기온(°C) 최저기온(°C) 최고기온(°C) 일강수량(mm) 평균 풍속(m/s)
##### 136  2020-05-07  413 광진구    502       742       NA          NA          NA           NA             NA
##### 854  2020-05-07  413 광진구    502       742       NA          NA          NA           NA             NA

#### "502" 의 경우, 해당 결측치들을 임의의 값으로 채우는 것 보다 아예 이 line을 없애는 것이 더 나을 것 같다고 판단함.
df_combined_3 <- df_combined_3[-136, ]
df_combined_3[!complete.cases(df_combined_3), ]
df_combined_3 <- df_combined_3[-853, ]
df_combined_3[!complete.cases(df_combined_3), ]

## 7.4. + 대기오염 데이터
df_combined_4 <- merge(df_combined_3, df_airpolution,
  by.x = c("대여일자", "자치구"), by.y = c("측정일시", "측정소명"),
  all.x = TRUE
)


## 7.5. + 공휴일 데이터
# 초기화 0 = 'N', 1 = 'Y'
df_combined_4$holiday <- "0"
df_combined_4$holiday[ymd(df_combined_4$대여일자) %in% ymd(df_holiday$datetime)] <- "1"

sum(is.na(df_combined_4))

################################################################################
# 8. 컬럼 전처리
################################################################################
## 자치구, 지점, 대여소번호 삭제 (데이터량 감소를 위해)
df_combined_4 <- subset(df_combined_4, select = -c(자치구, 지점, 대여소번호))

summary(df_combined_4)

## RandomForest에서 특수문자를 이해하지 못하므로 컬럼명 변경
colnames(df_combined_4) <- c("datetime", "rent", "avg_temperature", "low_temperature", "high_temperature", "rainy", "windy", "no2_ppm", "o3_ppm", "co_ppm", "so2_ppm", "part_matter", "ultra_part_matter", "holiday")

## holiday를 수치형으로 변경
df_combined_4$holiday <- as.numeric(df_combined_4$holiday)


#############################
#### Data frame 준비 완료 ####
sum(is.na(df_combined_4))
dim(df_combined_4) # 1,728 개
summary(df_combined_4)
str(df_combined_4)
df_combined_4[!complete.cases(df_combined_4), ]

################################################################################
# 9. EDA
################################################################################
eda <- df_combined_4

source("eda.R")

# 날짜 별 사용량
draw_rent_per_day_geom_line(eda)

# 날짜 별 기온
drwa_temper_per_day_geom_line(eda)

# 날짜 별 강수량
draw_rainy_per_day_geom_line(eda)

# 날짜 별 풍속
draw_windy_per_day_geom_line(eda)

# 미세먼지, 초미세먼지
draw_part_matters_geom_line(eda)

# correlation matrix - datetime 제외 후 계산
ggpairs(subset(eda, select = -c(datetime)), title = "Correlation Matrix")


# eda 데이터 정제 후 확인
## 로그화 (왜도가 Positive인 변수들) - 음수가 나오지 않도록 +0.01 보정
### 종속 변수 rent
eda$rent <- log(df_combined_4$rent)
### rainy
eda$rainy <- log(df_combined_4$rainy + 1)
### windy
eda$windy <- log(df_combined_4$windy + 0.9)
### no2_ppm 이산화질소
eda$no2_ppm <- log(df_combined_4$no2_ppm + 1)
### o3_ppm 오존
eda$o3_ppm <- log(edf_combined_4da$o3_ppm + 1)
### co_ppm 일산화탄소
eda$co_ppm <- log(df_combined_4$co_ppm + 1)
### part_matter 미세먼지
eda$part_matter <- log(df_combined_4$part_matter + 1)
### ultra_part_matter 초미세먼지
eda$ultra_part_matter <- log(df_combined_4$ultra_part_matter + 1)

summary(eda)

# correlation matrix - 로그화 이후 다시 확인
ggpairs(subset(eda, select = -c(datetime)), title = "Correlation Matrix")

# correlation matrix - 히트맵 생성
correlation <- cor(subset(eda, select = -c(datetime)))
corrplot(correlation, method = "color", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "red")

################################################################################
# 10. Split data
################################################################################
# 전체 데이터 준비
df_ready <- subset(df_combined_4, select = -c(datetime))

# 분할
sample_split <- sample.split(Y = df_ready, SplitRatio = 0.7)

train_set <- subset(x = df_ready, sample_split == TRUE)
test_set <- subset(x = df_ready, sample_split == FALSE)

train_x <- subset(train_set, select = -c(rent))
train_y <- subset(train_set, select = c(rent))

test_x <- subset(test_set, select = -c(rent))
test_y <- subset(test_set, select = c(rent))

# 로그화 하기 전 데이터 백업
origin_test_y <- test_y

## 로그화 (왜도가 Positive인 변수들) - 음수가 나오지 않도록 +0.01 보정
### 종속 변수 rent
train_set$rent <- log(train_set$rent)
test_set$rent <- log(test_set$rent)
train_y$rent <- log(train_y$rent)
test_y$rent <- log(test_y$rent)
### rainy
train_x$rainy <- log(train_x$rainy + 1)
test_x$rainy <- log(test_x$rainy + 1)
### windy
train_x$windy <- log(train_x$windy + 0.9)
test_x$windy <- log(test_x$windy + 0.9)

### no2_ppm 이산화질소
train_x$no2_ppm <- log(train_x$no2_ppm + 1)
test_x$no2_ppm <- log(test_x$no2_ppm + 1)

### o3_ppm 오존
train_x$o3_ppm <- log(train_x$o3_ppm + 1)
test_x$o3_ppm <- log(test_x$o3_ppm + 1)

### co_ppm 일산화탄소
train_x$co_ppm <- log(train_x$co_ppm + 1)
test_x$co_ppm <- log(test_x$co_ppm + 1)

### part_matter 미세먼지
train_x$part_matter <- log(train_x$part_matter + 1)
test_x$part_matter <- log(test_x$part_matter + 1)

### ultra_part_matter 초미세먼지
train_x$ultra_part_matter <- log(train_x$ultra_part_matter + 1)
test_x$ultra_part_matter <- log(test_x$ultra_part_matter + 1)

#high_temperature + rainy + windy + no2_ppm + o3_ppm + co_ppm + so2_ppm + part_matter + ultra_part_matter + holiday

################################################################################
# 10. 다중 공선성 확인
################################################################################
# VIF 계산
vif_values <- vif(lm(rent ~ ., data = train_set))

# VIF 출력
print(vif_values)

# VIF 시각화
draw_vif(data.frame(Variable = names(vif_values), VIF = vif_values))

# avg_temperature   low_temperature  high_temperature             rainy
#      451.721490        132.062308        138.371064          1.157015
#           windy           no2_ppm            o3_ppm            co_ppm
#        1.928044          4.600797          1.937228          2.301119
#         so2_ppm       part_matter ultra_part_matter           holiday
#        1.468099          3.181534          4.507002          1.017024

# VIF (Variance Inflation Factor) 값은 다중공선성을 평가하기 위해 사용되는 지표입니다.
# 일반적으로 VIF 값이 1보다 작거나 1에 가까우면 다중공선성의 문제가 거의 없다고 판단됩니다.
# VIF 값이 1보다 큰 경우, 변수들 간에 상관 관계가 높아 다중공선성의 가능성이 높아집니다.
# 보통 5 또는 10 이상의 VIF 값이 있는 변수들은 다중공선성의 문제가 있을 수 있습니다.

# avg_temperature 제거한 후 VIF 계산
vif_values <- vif(lm(rent ~ ., data = subset(train_set, select = -c(avg_temperature))))

# VIF 출력
print(vif_values)

# low_temperature  high_temperature             rainy             windy
#       19.765853         19.281852          1.151929          1.927305
#         no2_ppm            o3_ppm            co_ppm           so2_ppm
#        4.553829          1.936310          2.300424          1.462943
#     part_matter ultra_part_matter           holiday
#        3.172519          4.490386          1.012553

################################################################################
# 11. Random Forest
# 11.x 중 하나만 선택하여 수행하시면 됩니다.
################################################################################

## 11.1. 전체 변수 포함
### 11.1.1. ntree = 100
model_rf <- randomForest(rent ~ .,
  data = train_set, ntree = 100, type = "regression", importance = TRUE
)

### 11.1.2. ntree = 500
model_rf <- randomForest(rent ~ .,
  data = train_set, ntree = 500, type = "regression", importance = TRUE
)

## 11.2. 선택 변수만 - 기온 중 상관관계가 가장 높았던 high를 살려둠.
### 11.2.1. ntree = 100
model_rf <- randomForest(rent ~ high_temperature + rainy + windy + no2_ppm + o3_ppm + co_ppm + so2_ppm + part_matter + ultra_part_matter + holiday,
  data = train_set, ntree = 100, type = "regression", importance = TRUE
)

### 11.2.2. ntree = 500
model_rf <- randomForest(rent ~ high_temperature + rainy + windy + no2_ppm + o3_ppm + co_ppm + so2_ppm + part_matter + ultra_part_matter + holiday,
  data = train_set, ntree = 500, type = "regression", importance = TRUE
)


### test용
model_rf <- randomForest(rent ~ high_temperature + rainy + windy + no2_ppm + o3_ppm + co_ppm + so2_ppm + part_matter + ultra_part_matter + holiday,
  data = train_set, ntree = 500, type = "regression", importance = TRUE
)

### 결과 확인
print(model_rf)
summary(model_rf)

################################################################################
# 12. Validation
################################################################################

## 예측
predicted_y <- predict(model_rf, test_x)
summary(train_set$rent)
summary(predicted_y)
summary(test_y$rent)
summary(origin_test_y$rent)
## MSE, MAE, MAPE 점수 확인
calculate_mse_mae_mape(test_y$rent, predicted_y)
## R-Squared 점수 확인 - 1일 수록 좋음
calculate_r_squared(predicted_y, test_y$rent)
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

## 12.1. 선택 변수만
### 12.1.1.
model_lm <- lm(rent ~ high_temperature + rainy + windy + no2_ppm + o3_ppm + co_ppm + so2_ppm + part_matter + ultra_part_matter + holiday, data = train_set)

## 값 확인
summary(model_lm)

## 예측
predicted_y <- predict(model_lm, test_x)

## MSE, MAE, MAPE 점수 확인
calculate_mse_mae_mape(test_y$rent, predicted_y)

## R-Squared 점수 확인 - 1일 수록 좋음
calculate_r_squared(predicted_y, test_y$rent)

## [시각화] 잔차
view_residual_plot(test_y$rent, predicted_y)


################################################################################
# 13. GLM 및 Validation
################################################################################

# 정규성을 알아보기 위한 qq 플롯 그리기
qqnorm(all_full_ready$rent) # 정규성 검정을 위한 QQ 플롯
qqline(all_full_ready$rent) # 기대되는 정규분포와 비교하는 선 그리기

# 정규성을 따른다면, glm 모델 적용 가능.
## 13.1. 전체 변수 포함
model_glm <- glm(rent ~ ., data = train_set, family = gaussian)

# 모델 요약
summary(model_glm)

## 13.2. 선택 변수만
model_glm <- glm(rent ~ high_temperature + rainy + windy + no2_ppm + o3_ppm + co_ppm + so2_ppm + part_matter + ultra_part_matter + holiday, data = train_set, family = gaussian)

# 모델 요약
summary(model_glm)

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
