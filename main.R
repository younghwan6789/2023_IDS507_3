# 1. 패키지 인스톨
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

# 2. 패키지 로딩
library(readr)
library(dplyr)
library(lubridate)
library(xml2)
library(ggplot2)
library(GGally)
library(randomForest)
library(caTools)
library(caret)
library(pROC)

# 3. 함수 선언
## 3.1. 따릉이 대여소 정보(2022. 12. 기준)를 얻어오기 위한 함수 로딩
load_dda_station_info <- function() {
  dda_station_info <- read_csv("data/ddarung/공공자전거 대여소 정보(22.12월 기준)_mod.csv", locale=locale('ko', encoding='euc-kr'))
  # 대여소 번호와 자치구 정보만 사용
  return(dda_station_info[, c("대여소 번호", "자치구")])
}

## 3.2. 따릉이 일별 사용률 정보를 얻어오기 위한 함수 로딩
load_dda_daily_info <- function() {
  # 오래 걸리니까 간단한 테스트 시, 위 두 줄을 주석으로 막고 아래 두 줄을 열고 테스트 해보세요.
  ## 2023. 06. 04. 데이터를 모두 사용할 경우, 
  ## 2023. 06. 04. 15:00 - 5월, 9월 데이터만 사용하는 것으로.
  #file_names_upper <- c("22.01", "22.02", "22.03", "22.04", "22.05", "22.06")
  #file_names_lower <- c("22.07", "22.08", "2209", "2210", "2211", "2212")
  file_names_upper <- c("22.05")
  file_names_lower <- c("2209")
  
  dda_daily_list <- list()
  
  for (file_name in file_names_upper) {
    file_path <- paste0("data/ddarung/서울특별시 공공자전거 이용정보(일별)_", file_name, ".csv")
    dda_daily <- read_csv(file_path, locale=locale('ko', encoding='euc-kr'))
    dda_daily <- subset(dda_daily, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))
    dda_daily <- dda_daily %>%
      group_by(대여일자, 대여소번호) %>%
      summarize(총이용건수 = sum(이용건수))
    
    dda_daily_list[[file_name]] <- dda_daily
  }
  
  for (file_name in file_names_lower) {
    file_path <- paste0("data/ddarung/서울특별시 공공자전거 이용정보(일별)_", file_name, ".csv")
    dda_daily <- read_csv(file_path, locale=locale('ko', encoding='euc-kr'))
    dda_daily <- subset(dda_daily, select = -c(대여소, 대여구분코드, 성별, 연령대, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))
    dda_daily <- dda_daily %>%
      group_by(대여일자, 대여소번호) %>%
      summarize(총이용건수 = sum(이용건수))
    
    dda_daily_list[[file_name]] <- dda_daily
  }
  
  return(dda_daily_list)
}

## 3.3. 날씨정보 결측치 처리 시 사용할 평균 전환 함수
replace_with_mean <- function(df, column) {
  df[column][is.na(df[column])] <- sapply(df[is.na(df[column]), "일시"], function(x) mean(df[df$일시 == x, column][[1]], na.rm = TRUE))
  df
}

## 3.4. 날씨 정보를 얻어오기 위한 함수 로딩
load_weather_info <- function() {
  weather_info <- read_csv("data/weather/OBS_AWS_DD_20230531190259_2022.csv", locale=locale('ko', encoding='euc-kr'))
  # 지점명 삭제, 지점 ID를 대여소 번호와 매칭 예정.  
  return(subset(weather_info, select = -c(지점명)))
}

# 4. 따릉이 대여소 정보 로딩 (2022년 12월 기준)
dda_station_info <- load_dda_station_info()

## 4.1. EDA
summary(dda_station_info) # 대여소 번호: number , 자치구 character 타입 확인 --> 둘 모두 명목형
str(dda_station_info) # 2719 rows 확인
length(unique(dda_station_info$자치구)) # 자치구 데이터의 고유값 확인 : 25개의 자치구(정보: 서울시는 25개 자치구와 426개의 행정동으로 이루어짐.)
sum(is.na(dda_station_info))

######
## 그래프로 확인

length(unique(dda_station_info$`대여소 번호`)) # 2,719 개
length(unique(dda_station_info$자치구)) # 25개

# 자치구별 대여소 개수 계산
count <- table(dda_station_info$자치구)

# 막대 그래프 그리기
ggplot(data = dda_station_info, aes(x = 자치구)) +
  geom_bar(fill = "#21a37d") +
  labs(x = "자치구", y = "대여소 개수") +
  ggtitle("자치구별 대여소 번호 개수") +
  theme_minimal()

######

# 5. 따릉이 일별 사용률 얻어오기 (오래 걸립니다. - 약 90초 - PC 사양마다 차이가 있습니다. 빨간 경고 메시지 나오면 다 된겁니다.)
## 5, 9월 데이터만 -> 약 10초
dda_daily_list <- load_dda_daily_info()

# 6. 따릉이 일별 데이터를 모두 결합 # 총 945,356 건
## 5, 9월 데이터만 -> 158,721 건
dda_daily_full <- do.call(rbind, dda_daily_list)


##########
# 데이터 확인용 구간이라서 skip 하세요~

summary(dda_daily_list[1]$`22.01`$총이용건수)

head(dda_daily_list[1]$총이용건수)
summary(dda_daily_list[2])
summary(dda_daily_list[3])

boxplot(dda_daily_list[1]$`22.01`$총이용건수,
        dda_daily_list[2]$`22.02`$총이용건수,
        dda_daily_list[3]$`22.03`$총이용건수,
        dda_daily_list[4]$`22.04`$총이용건수,
        dda_daily_list[5]$`22.05`$총이용건수,
        dda_daily_list[6]$`22.06`$총이용건수,
        dda_daily_list[7]$`22.07`$총이용건수,
        dda_daily_list[8]$`22.08`$총이용건수,
        dda_daily_list[9]$`2209`$총이용건수,
        dda_daily_list[10]$`2210`$총이용건수,
        dda_daily_list[11]$`2211`$총이용건수,
        dda_daily_list[12]$`2212`$총이용건수,
        names = c("1월", "2월", "3월", "4월", "5월", "6월", "7월", "8월", "9월", "10월", "11월", "12월"),
        main = "BoxPlot",
        ylab = "총이용건수",
        col = "lightblue",
        outline = FALSE)


##########



## 6.1. EDA
summary(dda_daily_full)
str(dda_daily_full)
sum(is.na(dda_daily_full))
### 대여일자, 대여소번호는 명목척도이므로 이용건수에 대해서만 boxplot을 그려봄.
boxplot(dda_daily_full$총이용건수, main = "BoxPlot", ylab = "총이용건수")


# 7. 날씨 정보 로딩
weather_2022 <- load_weather_info()
#weather_2022 <- read_csv("data/weather/OBS_AWS_DD_20230531190259_2022.csv", locale=locale('ko', encoding='euc-kr'))
# 지점명 삭제, 지점 ID를 대여소 번호와 매칭 예정.
# weather_2022 <- subset(weather_2022, select = -c(지점명))

## 7.1. EDA
str(weather_2022)
summary(weather_2022)
sum(is.na(weather_2022)) # 결측치 251.

boxplot(weather_2022$`평균 풍속(m/s)`, main = "BoxPlot", ylab = "평균 풍속(m/s)")

####################### 결측치 그래프 예시
# 결측치 비율 계산
missing_ratio <- colMeans(is.na(weather_2022))

# 데이터 프레임 생성
missing_data <- data.frame(variable = names(missing_ratio), missing_ratio = missing_ratio)

# 바 그래프로 시각화
ggplot(missing_data, aes(x = variable, y = missing_ratio, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(x = "변수", y = "결측치 비율", title = "결측치 비율") +
  scale_fill_manual(values = c("평균 풍속(m/s)" = "purple", "일강수량(mm)" = "skyblue", "최고기온(°C)" = "red", "평균기온(°C)" = "yellow", "최저기온(°C)" = "blue")) +
  theme_minimal()
####################### 결측치 그래프 예시 끝

## 7.2. Pre-processing
## 강수량은 0 치환 가능, 평균, 최저, 최고 의 경우 어떻게 치환?
weather_2022_mod <- weather_2022
weather_2022_mod$`일강수량(mm)`[is.na(weather_2022_mod$`일강수량(mm)`)] <- 0
sum(is.na(weather_2022_mod)) # 결측치 245.

## 최저 기온, 최고 기온, 평균 기온, 풍속 모두 해당 일의 모든 측정소의 평균을 넣어봅시다.
### "3.3. 날씨정보 결측치 처리 시 사용할 평균 전환 함수" 참조
### warning은 무시해주세요.

# 풍속 결측치 처리
weather_2022_mod <- replace_with_mean(weather_2022_mod, "평균 풍속(m/s)")

# 최저 기온 결측치 처리
weather_2022_mod <- replace_with_mean(weather_2022_mod, "최저기온(°C)")

# 최고 기온 결측치 처리
weather_2022_mod <- replace_with_mean(weather_2022_mod, "최고기온(°C)")

# 평균 기온 결측치 처리
weather_2022_mod <- replace_with_mean(weather_2022_mod, "평균기온(°C)")

## 7.3. EDA again

sum(is.na(weather_2022_mod)) # 결측치 0.

boxplot(weather_2022_mod$`평균 풍속(m/s)`, main = "BoxPlot", ylab = "평균 풍속(m/s)")

ggplot(weather_2022_mod, aes(x = 일시)) +
  geom_bar(aes(y = `평균 풍속(m/s)`, fill = "평균풍속"), stat = "identity") +
  labs(x = "일시", y = "평균 풍속(m/s)", fill = "변수") +
  scale_fill_manual(values = c("평균풍속" = "blue")) +
  theme_minimal()

ggplot(weather_2022_mod, aes(x = 일시)) +
  geom_bar(aes(y = `일강수량(mm)`, fill = "일강수량"), stat = "identity") +
  labs(x = "일시", y = "일강수량(mm)", fill = "변수") +
  scale_fill_manual(values = c("일강수량" = "blue")) +
  theme_minimal()

ggplot(weather_2022_mod, aes(x = `평균 풍속(m/s)`)) +
  geom_histogram(fill = "blue", color = "white", bins = 30) +
  labs(x = "평균 풍속(m/s)", y = "빈도") +
  theme_minimal()

ggplot(weather_2022_mod, aes(x = `평균기온(°C)`)) +
  geom_histogram(fill = "blue", color = "white", bins = 30) +
  labs(x = "평균기온(°C)", y = "빈도") +
  theme_minimal()

ggplot(weather_2022_mod, aes(x = `일강수량(mm)`)) +
  geom_histogram(fill = "blue", color = "white", bins = 30) +
  labs(x = "일강수량(mm)", y = "빈도") +
  theme_minimal()

ggplot(weather_2022_mod, aes(x = 일시)) +
  geom_line(aes(y = `평균기온(°C)`, color = "평균기온")) +
  geom_line(aes(y = `최저기온(°C)`, color = "최저기온")) +
  geom_line(aes(y = `최고기온(°C)`, color = "최고기온")) +
  labs(x = "일시", y = "온도(°C)", color = "변수") +
  scale_color_manual(values = c("평균기온" = "yellow", "최저기온" = "red", "최고기온" = "blue")) +
  theme_minimal()

# 8. 매칭을 위한 날씨 카테고리 로딩
weather_category <- read_csv("data/weather/category_weather.csv", locale=locale('ko', encoding='euc-kr'))

# 9. 대기오염도 로딩
airpolution_2022 <- read_csv("data/airpolution/일별평균대기오염도_2022.csv", locale=locale('ko', encoding='euc-kr'))

## 9.1. EDA
sum(is.na(airpolution_2022)) # 결측치 565.
print(airpolution_2022[!complete.cases(airpolution_2022),], n=156) # 156 개 라인이 결측치 보유
# 측정이 안될만큼 깨끗한 날씨였다면 0으로 처리.
# 그러나, 그런 의미는 아닌 것 같음.
# 해당 일의 다른 자료들의 평균치로 기입.

####################### 결측치 그래프 예시
# 결측치 비율 계산
missing_ratio <- colMeans(is.na(airpolution_2022))

# 데이터 프레임 생성
missing_data <- data.frame(variable = names(missing_ratio), missing_ratio = missing_ratio)

# 바 그래프로 시각화
ggplot(missing_data, aes(x = variable, y = missing_ratio)) +
  geom_bar(stat = "identity") +
  labs(x = "변수", y = "결측치 비율", title = "결측치 비율") +
  theme_minimal()
####################### 결측치 그래프 예시 끝

## TODO: 아래 내용 삭제하고 제대로 결측치 채울 것!!!!!
#### 우선, 다른거부터 확인하기 위해 0으로 채우고 넘어가...
airpolution_2022[is.na(airpolution_2022)] <- 0

boxplot(airpolution_2022$`이산화질소농도(ppm)`,
        airpolution_2022$`오존농도(ppm)`,
        names = c("이산화질소", "오존"),
        main = "BoxPlot",
        ylab = "ppm",
        col = "lightblue",
        outline = FALSE)

boxplot(airpolution_2022$`일산화탄소농도(ppm)`, main = "BoxPlot", ylab = "일산화탄소농도(ppm)", col="lightblue", outline=FALSE)

boxplot(airpolution_2022$`아황산가스농도(ppm)`, main = "BoxPlot", ylab = "아황산가스농도(ppm)", col="lightblue", outline=FALSE)

boxplot(airpolution_2022$`미세먼지농도(㎍/㎥)`,
        airpolution_2022$`초미세먼지농도(㎍/㎥)`,
        names = c("미세먼지", "초미세먼지"),
        main = "BoxPlot",
        ylab = "농도(㎍/㎥)",
        col = "lightblue",
        outline = FALSE)


# 10. 일일 데이터에 있는 대여소번호와 마스터 데이터의 대여소_ID 를 매칭시킨 후 merge
## merge 전 id 타입 일치를 위한 변환
dda_daily_full$대여소번호 <- as.numeric(dda_daily_full$대여소번호) # 숫자 앞에 '00' 제거
dda_station_info$`대여소 번호` <- as.numeric(dda_station_info$`대여소 번호`)

sum(is.na(dda_daily_full))
sum(is.na(dda_station_info))

dda_merge1 <- merge(dda_daily_full, dda_station_info, by.x = "대여소번호", by.y = "대여소 번호", all.x = TRUE)

sum(is.na(dda_merge1)) # 결측치 8,929. -> 5, 9월 데이터는 1,440.

dda_merge1_na_rows = dda_merge1[!complete.cases(dda_merge1), ]
## 총 59개의 자치구 정보 누락된 대여소 번호 발견
unique(dda_merge1_na_rows$대여소번호)
length(unique(dda_merge1_na_rows$대여소번호))
## 누락된 대여소 번호
###[1]     3     5    10    11   116   210   274   439   507   572   604   625   658   847   901  1014  1131  1146  1211
###[20]  1369  1412  1522  1852  2063  2220  2610  2638  2722  3124  3406  3687  3795  3928  3929  4046  4048  4075  4132
###[39]  4245  4330  4534  4702  4821  4839  4853  4861  4894  5757  9979  9980  9985  9992  9993  9997  9998  9999 88888
###[58] 99998 99999

# 2022년 12월 기준으로 사라진 대여소라고 판단. 모두 삭제
dda_merge1_mod <- na.omit(dda_merge1)
sum(is.na(dda_merge1_mod)) # 0개 확인
str(dda_merge1_mod) # 남아있는 라인 확인, 936,427개


head(dda_merge1_mod)
head(weather_category)

# 11. 자치구 '구' 정보를 통해 날씨 정보를 가져오기 전, 날씨 측정 지점을 가져옴
dda_merge2 <- merge(dda_merge1_mod, weather_category, by.x = "자치구", by.y = "지점명", all.x = TRUE)
head(dda_merge2)
sum(is.na(dda_merge2)) # 전체 데이터에서 36,979 개 발생, 5, 9월 데이터 -> 6,216 개

dda_merge2_na_rows = dda_merge2[!complete.cases(dda_merge2), ]
head(dda_merge2_na_rows)
summary(dda_merge2_na_rows)

# 종로구는 지점이 없다. 용산구 위에 있으므로 용산구의 기상과 동일하게 취급.(limitation) #category_weather.csv 참조
dda_merge2_mod <- dda_merge2
dda_merge2_mod[dda_merge2_mod$자치구 == "종로구" & is.na(dda_merge2_mod$지점), "지점"] <- 415

sum(is.na(dda_merge2_mod)) # 결측치 사라짐. 모두 종로구였음.


# 12. 날짜와 지점 정보를 이용하여 날씨 정보들을 가져옴
dda_merge3 <- merge(dda_merge2_mod, weather_2022_mod, by.x = c("대여일자", "지점"), by.y = c("일시", "지점"), all.x = TRUE)
sum(is.na(dda_merge3)) # 결측치 1,035. 5, 9월 데이터는 530.

dda_merge3_na_rows = dda_merge3[!complete.cases(dda_merge3), ]
head(dda_merge3_na_rows)
tail(dda_merge3_na_rows)
# 결측치 주인공은 구로구
## 구로구 날씨 정보에 아래 날짜 row가 비어있음
unique(dda_merge3_na_rows$대여일자)
### 2022-06-16, 2022-09-06
#### 두 날짜에 대해 데이터를 생성하여 채워준다. 앞 뒤 날짜의 평균값을 넣는다.
weather_2022_mod_guro <- weather_2022_mod

weather_guro_0616 <- data.frame(
  지점 = 423,
  일시 = ymd("2022-06-16"),
  `평균기온(°C)` = (weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-06-15" & weather_2022_mod$지점 == 423), "평균기온(°C)"] + weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-06-17" & weather_2022_mod$지점 == 423), "평균기온(°C)"]) / 2,
  `최저기온(°C)` = (weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-06-15" & weather_2022_mod$지점 == 423), "최저기온(°C)"] + weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-06-17" & weather_2022_mod$지점 == 423), "최저기온(°C)"]) / 2,
  `최고기온(°C)` = (weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-06-15" & weather_2022_mod$지점 == 423), "최고기온(°C)"] + weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-06-17" & weather_2022_mod$지점 == 423), "최고기온(°C)"]) / 2,
  `일강수량(mm)` = (weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-06-15" & weather_2022_mod$지점 == 423), "일강수량(mm)"] + weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-06-17" & weather_2022_mod$지점 == 423), "일강수량(mm)"]) / 2,
  `평균 풍속(m/s)`= (weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-06-15" & weather_2022_mod$지점 == 423), "평균 풍속(m/s)"] + weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-06-17" & weather_2022_mod$지점 == 423), "평균 풍속(m/s)"]) / 2
)
weather_guro_0906 <- data.frame(
  지점 = 423,
  일시 = ymd("2022-09-06"),
  `평균기온(°C)` = (weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-09-05" & weather_2022_mod$지점 == 423), "평균기온(°C)"] + weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-09-07" & weather_2022_mod$지점 == 423), "평균기온(°C)"]) / 2,
  `최저기온(°C)` = (weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-09-05" & weather_2022_mod$지점 == 423), "최저기온(°C)"] + weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-09-07" & weather_2022_mod$지점 == 423), "최저기온(°C)"]) / 2,
  `최고기온(°C)` = (weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-09-05" & weather_2022_mod$지점 == 423), "최고기온(°C)"] + weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-09-07" & weather_2022_mod$지점 == 423), "최고기온(°C)"]) / 2,
  `일강수량(mm)` = (weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-09-05" & weather_2022_mod$지점 == 423), "일강수량(mm)"] + weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-09-07" & weather_2022_mod$지점 == 423), "일강수량(mm)"]) / 2,
  `평균 풍속(m/s)` = (weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-09-05" & weather_2022_mod$지점 == 423), "평균 풍속(m/s)"] + weather_2022_mod_guro[(weather_2022_mod_guro$일시 == "2022-09-07" & weather_2022_mod$지점 == 423), "평균 풍속(m/s)"]) / 2
)

colnames(weather_guro_0616) <- c("지점", "일시", "평균기온(°C)", "최저기온(°C)", "최고기온(°C)", "일강수량(mm)", "평균 풍속(m/s)")
colnames(weather_guro_0906) <- c("지점", "일시", "평균기온(°C)", "최저기온(°C)", "최고기온(°C)", "일강수량(mm)", "평균 풍속(m/s)")

weather_2022_mod_guro <- rbind(weather_2022_mod_guro, weather_guro_0616)
weather_2022_mod_guro <- rbind(weather_2022_mod_guro, weather_guro_0906)

# 구로구 보정 후 다시 결합 시도.
dda_merge3 <- merge(dda_merge2_mod, weather_2022_mod_guro, by.x = c("대여일자", "지점"), by.y = c("일시", "지점"), all.x = TRUE)
sum(is.na(dda_merge3)) # 결측치 0.


# 13. 대여일자와 자치구를 이용하여 미세먼지 정보를 가져옴
## 날짜 형식 통일
airpolution_2022$측정일시 <- ymd(airpolution_2022$측정일시)
dda_merge4 <- merge(dda_merge3, airpolution_2022, by.x = c("대여일자", "자치구"), by.y = c("측정일시", "측정소명"), all.x = TRUE)

head(dda_merge4)
sum(is.na(dda_merge4)) # 결측치 0

# 14. 공휴일 정보 읽어오기
data_holiday <- read_xml("data/holiday/korea_holidays_2022.xml")
datetime <- xml2::xml_find_all(data_holiday, "//item/locdate") %>% xml2::xml_text()
isHoliday <- xml2::xml_find_all(data_holiday, "//item/isHoliday") %>% xml2::xml_text()
df_holiday <- data.frame(datetime, isHoliday)
df_holiday$datetime <- as.Date(df_holiday$datetime, format="%Y%m%d")

sum(is.na(df_holiday)) # 결측치 0


# 15. 공휴일 정보 merge
dda_merge5 <- dda_merge4 # 데이터 백업
dda_merge5$holiday <- "0" # 초기화 0 = 'N'
dda_merge5$holiday[dda_merge4$대여일자 %in% df_holiday$datetime] <- "1" # 1 = 'Y'

head(dda_merge5)

sum(is.na(dda_merge5)) # 결측치 0


# 16. 데이터 분할
## 16.1. randomForest에서 컬럼의 특수문자를 이해하지 못하므로, 컬럼명 변경.
df_for_rf = dda_merge5 # 복사

colnames(df_for_rf) <- c("대여일자", "자치구", "지점", "대여소번호", "총이용건수", "평균기온", "최저기온", "최고기온", "일강수량", "평균풍속", "이산화질소농도", "오존농도", "일산화탄소농도", "아황산가스농도", "미세먼지농도", "초미세먼지농도", "공휴일여부")

df_for_rf <- subset(df_for_rf, select = -c(자치구, 지점)) # 데이터 줄이기

sample_split <- sample.split(Y = df_for_rf, SplitRatio = 0.5)
train_set <- subset(x = df_for_rf, sample_split == TRUE)
test_set <- subset(x = df_for_rf, sample_split == FALSE)

dim(train_set)
dim(test_set)


head(train_set)

#### 17. 18.
rf_model <- randomForest(총이용건수 ~ 평균기온 + 일강수량 + 평균풍속 + 이산화질소농도 + 오존농도 + 일산화탄소농도 + 아황산가스농도 + 미세먼지농도 + 초미세먼지농도 + 공휴일여부, data = train_set, importance = TRUE)
predictions <- predict(rf_model, newdata = test_set)

rf_model

mse <- mean((test_set$총이용건수 - predictions)^2)
print(mse)

levels(predictions)
levels(as.factor(predictions))
levels(test_set$총이용건수)
levels(as.factor(test_set$총이용건수))

# 레벨 맞추기
factor_predictions <- factor(predictions, levels = levels(as.factor(test_set$총이용건수)))
factor_test <- factor(test_set$총이용건수, levels = levels(as.factor(test_set$총이용건수)))
confusion_matrix <- confusionMatrix(factor_predictions_2, factor_test)

print(confusion_matrix)

# 변수의 중요도 파악
varImpPlot(rf_model, type = 2, col = 1, cex = 1)

# Accuracy 계산
accuracy <- confusion_matrix$overall["Accuracy"]
print(paste("Accuracy:", accuracy))

# Precision 계산
precision <- confusion_matrix$byClass["Pos Pred Value"]
print(paste("Precision:", precision))

# Recall 계산
recall <- confusion_matrix$byClass["Sensitivity"]
print(paste("Recall:", recall))

# F1 Score 계산
f1_score <- confusion_matrix$byClass["F1"]
print(paste("F1 Score:", f1_score))

# AUC 계산
roc <- roc(test_set$총이용건수, as.numeric(predictions))
auc <- auc(roc)
print(paste("AUC:", auc))

####
###########
# 이건 3분만에 돌아감 (ntree = 100)

str(train_set)
head(train_set)

rf_model_2 <- randomForest(총이용건수 ~ 평균기온 + 일강수량 + 평균풍속 + 이산화질소농도 + 오존농도 + 일산화탄소농도 + 아황산가스농도 + 미세먼지농도 + 초미세먼지농도 + 공휴일여부, data = train_set, importance = TRUE,
                           ntree = 100)


predictions_2 <- predict(rf_model_2, newdata = test_set)

levels(as.factor(predictions_2))
levels(as.factor(test_set$총이용건수))

# 레벨 맞추기
factor_predictions <- factor(predictions_2, levels = levels(as.factor(test_set$총이용건수)))
factor_test <- factor(test_set$총이용건수, levels = levels(factor_predictions))

levels(factor_predictions)
levels(factor_test)

sum(is.na(predictions_2))
sum(is.na(factor_test))

confusion_matrix <- confusionMatrix(data = factor_predictions, reference = factor_test)

confusion_matrix

# 변수의 중요도 파악
varImpPlot(rf_model_2, type = 2, col = 1, cex = 1)

# Accuracy 계산
accuracy <- confusion_matrix$overall["Accuracy"]
print(paste("Accuracy:", accuracy))

# Precision 계산
precision <- confusion_matrix$byClass["Pos Pred Value"]
print(paste("Precision:", precision))

# Recall 계산
recall <- confusion_matrix$byClass["Sensitivity"]
print(paste("Recall:", recall))

# F1 Score 계산
f1_score <- confusion_matrix$byClass["F1"]
print(paste("F1 Score:", f1_score))

# AUC 계산
roc <- roc(test_set$총이용건수, as.numeric(predictions))
auc <- auc(roc)
print(paste("AUC:", auc))

###########

# 17. Random forest 모델 생성 (5, 9월 데이터 -> 30분정도)
rf_model <- randomForest(총이용건수 ~ ., data = train_set)

# 18. 모델 예측
predictions <- predict(rf_model, newdata = test_set)


# 19. 모델 평가
mse <- mean((test_set$총이용건수 - predictions)^2)
print(mse)

rmse <- sqrt(mse)
print(rmse)



# 잔차 계산
residuals <- test_set$총이용건수 - predictions
print(residuals)
# 잔차 시각화 - MSE 점수 확인
plot(predictions, residuals,
     main = "Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals")






#####
# 산점도 행렬 (얘네는 오래 걸립니다.)
## 날씨
ggpairs(weather_2022_mod[, c("평균기온(°C)", "최저기온(°C)", "최고기온(°C)", "일강수량(mm)", "평균 풍속(m/s)")])

## 대기오염
ggpairs(airpolution_2022[, c("이산화질소농도(ppm)", "오존농도(ppm)", "일산화탄소농도(ppm)", "아황산가스농도(ppm)", "미세먼지농도(㎍/㎥)", "초미세먼지농도(㎍/㎥)")])

## 총이용건수 포함
ggpairs(dda_merge5[, c("총이용건수", "평균기온(°C)", "최저기온(°C)", "최고기온(°C)", "일강수량(mm)", "평균 풍속(m/s)", "이산화질소농도(ppm)", "오존농도(ppm)", "일산화탄소농도(ppm)", "아황산가스농도(ppm)", "미세먼지농도(㎍/㎥)", "초미세먼지농도(㎍/㎥)", "holiday")])


