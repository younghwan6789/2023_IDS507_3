################################################################################
# 2. 파일 로딩
################################################################################
source('libraries.R')     # 라이브러리
source('utils.R')         # 유틸리티
source('data.R')          # data file 관련 함수
source('eda.R')           # EDA 관련 함수
source('validation.R')    # 검증 관련 함수
source('visualization.R') # 시각화 관련 함수



df_2018 <- get_ddarung_daily_info_2018(2715)
df_2019 <- get_ddarung_daily_info_2019(2715)
df_2020 <- get_ddarung_daily_info_2020(2715)
df_2021 <- get_ddarung_daily_info_2021(2715)
df_2022 <- get_ddarung_daily_info_2022("full")

dim(df_2018)
dim(df_2019)
dim(df_2020)
dim(df_2021)
dim(df_2022)

df_2022_2715 <- df_2022[df_2022$대여소번호==2715,]

dim(df_2022_2715)

df_full_2715 <- rbind(df_2018, df_2019, df_2020, df_2021, df_2022_2715)

summary(df_full_2715)
dim(df_full_2715)
str(df_full_2715)
sum(is.na(df_full_2715))

ggplot(df_full_2715, aes(x = 대여일자, y = 총이용건수)) +
  geom_line() +
  xlab("대여일자") +
  ylab("총이용건수") +
  ggtitle("대여일자별 총이용건수 변화") +
  theme_minimal()


## 각각 로딩
df_station <- get_ddarung_station_info()
# df_weather <- get_weather_info() <- 이건 2022년꺼만...
df_weather <- read_csv("data/weather/OBS_AWS_DD_20230531164605_2018-2022.csv", locale=locale('ko', encoding='euc-kr'))

# 지점명 삭제, 지점 ID를 대여소 번호와 매칭 예정.
df_weather <- subset(df_weather, select = -c(지점명))

#df_airpolution <- get_airpolution_info()
ap1 <- read_csv("data/airpolution/일별평균대기오염도_2018.csv", locale=locale('ko', encoding='euc-kr'))
ap2 <- read_csv("data/airpolution/일별평균대기오염도_2019.csv", locale=locale('ko', encoding='euc-kr'))
ap3 <- read_csv("data/airpolution/일별평균대기오염도_2020.csv", locale=locale('ko', encoding='euc-kr'))
ap4 <- read_csv("data/airpolution/일별평균대기오염도_2021.csv", locale=locale('ko', encoding='euc-kr'))
ap5 <- read_csv("data/airpolution/일별평균대기오염도_2022.csv", locale=locale('ko', encoding='euc-kr'))
df_airpolution <- rbind(ap1, ap2, ap3, ap4, ap5)

df_matching_info <- get_matching_info()
df_holiday <- get_holiday_info()

### 전처리
df_weather <- pre_proc_weather(df_weather)
df_airpolution <- pre_proc_airpolustion(df_airpolution)



### 따릉이 데이터와 대여소 데이터 결합
df_merge1 <- merge(df_full_2715, df_station, by.x = "대여소번호", by.y = "대여소 번호", all.x = TRUE)

### 따릉이 데이터와 날씨 데이터를 연결하기 위한 custom matching data 결합  
df_merge2 <- merge(df_merge1, df_matching_info, by.x ="자치구", by.y = "지점명", all.x = TRUE)
df_merge2[df_merge2$자치구 == "종로구" & is.na(df_merge2$지점), "지점"] <- 415 # 종로구는 날씨 측정소가 없음. 용산구로 치환.

### 따릉이 데이터와 날씨 데이터 결합
df_merge3 <- merge(df_merge2, df_weather, by.x = c("대여일자", "지점"), by.y = c("일시", "지점"), all.x = TRUE)
sum(is.na(df_merge3))

### 따릉이 데이터와 대기오염 데이터 결합
df_merge4 <- merge(df_merge3, df_airpolution, by.x = c("대여일자", "자치구"), by.y = c("측정일시", "측정소명"), all.x = TRUE)

### 따릉이 데이터에 공휴일 정보 추가
df_merge4$holiday <- "0" # 초기화 0 = 'N'
df_merge4$holiday[ymd(df_merge4$대여일자) %in% ymd(df_holiday$datetime)] <- "1" # 1 = 'Y'


### 자치구 삭제 (지점과 동일한 의미를 가지므로 분석 데이터량 감소를 위해)
df_merge4 <- subset(df_merge4, select = -c(자치구))


### RandomForest에서 특수문자를 이해하지 못하므로 컬럼명 변경
colnames(df_merge4) <- c("datetime","branch", "branch_no", "rent", "avg_temperature", "low_temperature", "high_temperature", "rainy", "windy", "no2_ppm", "o3_ppm", "co_ppm", "so2_ppm", "part_matter", "ultra_part_matter", "holiday")

all_full_ready <- df_merge4
sum(is.na(all_full_ready))





####

sample_split = sample.split(Y = all_full_ready, SplitRatio = 0.7)

train_set <- subset(x = all_full_ready, sample_split == TRUE)
test_set <- subset(x = all_full_ready, sample_split == FALSE)

train_x <- subset(train_set, select = -c(rent))
train_y <- subset(train_set, select = c(rent))

test_x <- subset(test_set, select = -c(rent))
test_y <- subset(test_set, select = c(rent))


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

source('validation.R')
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












df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_file$대여소번호 <- as.numeric(df_file$대여소번호) # 숫자 앞에 '00' 제거

df_2715 = df_file[df_file$대여소번호==2715,]

#temp <- full_merged_2715
full_merged_2715 <- rbind(full_merged_2715, df_2715)

summary(df_2715)
sum(is.na(df_2715))
summary(full_merged_2715)
sum(is.na(full_merged_2715))
dim(full_merged_2715)
###################################### 2021 1 finish
###################################### 2021 2 start
###################################### 2021 2 finish
###################################### 2021 3 start
###################################### 2021 3 finish

summary(df_file)











sum(is.na(df_2715))
df_2715[!complete.cases(df_2715), ]
print(df_2715, n=154)

full_merged_2715 <- na.omit(df_2715)


df_file <- subset(df_file, select = -c(`'대여소'`, `'대여구분코드'`, `'SEX_CD'`, `'연령대코드'`, `'운동량'`, `'탄소량'`, `'이동거리(M)'`, `'이동시간(분)'`))

df_file <- df_file %>%
  group_by(`'대여일자'`, `'대여소번호'`) %>%
  summarize(`'총이용건수'` = sum(`'이용건수'`))

df_2715 = df_file[df_file$`'대여소번호'`=="'2715'",]

sum(is.na(df_2715))
df_2715[!complete.cases(df_2715), ]
print(df_2715, n=154)

full_merged_2715 <- na.omit(df_2715)






sum(is.na(full_merged_2715))
str(full_merged_2715)
summary(full_merged_2715)
summary(temp)
sum(is.na(full_merged_2715))
summary(full_merged_2715)



sum(is.na(df_2715))
df_2715[!complete.cases(df_2715), ]
print(df_2715, n=154)
df_2715 <- na.omit(df_2715)
