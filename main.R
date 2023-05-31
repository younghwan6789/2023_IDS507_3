# 1. 패키지 인스톨
install.packages("readr")
install.packages("dplyr")
install.packages("lubridate") # ymd() 사용을 위해
install.packages("xml2") # 공휴일 정보 xml 파일 입력을 위해

# 2. 패키지 로딩
library(readr)
library(dplyr)
library(lubridate)
library(xml2)

# 3. 따릉이 마스터 정보를 얻어오기 위한 함수 로딩
load_dda_master_info <- function() {
  dda_master <- read_csv("data/ddarung/서울시 따릉이대여소 마스터 정보_mod.csv", locale=locale('ko', encoding='euc-kr'))
  return(dda_master)  
}

# 4. 따릉이 일별 사용률 정보를 얻어오기 위한 함수 로딩
load_dda_daily_info <- function() {
  # 오래 걸리니까 간단한 테스트 시, 위 두 줄을 주석으로 막고 아래 두 줄을 열고 테스트 해보세요.
  file_names_upper <- c("22.01", "22.02", "22.03", "22.04", "22.05", "22.06")
  file_names_lower <- c("22.07", "22.08", "2209", "2210", "2211", "2212")
  #file_names_upper <- c("22.01")
  #file_names_lower <- c("22.07")
  
  dda_daily_list <- list()
  
  # 오래 걸립니다.
  for (file_name in file_names_upper) {
    file_path <- paste0("data/ddarung/서울특별시 공공자전거 이용정보(일별)_", file_name, ".csv")
    dda_daily <- read_csv(file_path, locale=locale('ko', encoding='euc-kr'))
    dda_daily <- subset(dda_daily, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))
    dda_daily <- dda_daily %>%
      group_by(대여일자, 대여소번호) %>%
      summarize(총이용건수 = sum(이용건수))
    
    dda_daily_list[[file_name]] <- dda_daily
  }
  
  # 오래 걸립니다.
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

# 5. 날씨 정보 로딩
weather_2022 <- read_csv("data/weather/OBS_AWS_DD_20230531190259_2022.csv", locale=locale('ko', encoding='euc-kr'))
# 지점명 삭제, 지점 ID를 대여소 번호와 매칭 예정.
weather_2022 <- subset(weather_2022, select = -c(지점명))

# 6. 매칭을 위한 날씨 카테고리 로딩
weather_category <- read_csv("data/weather/category_weather.csv", locale=locale('ko', encoding='euc-kr'))

# 7. 대기오염도 로딩
airpolution_2022 <- read_csv("data/airpolution/일별평균대기오염도_2022.csv", locale=locale('ko', encoding='euc-kr'))

# 8. 따릉이 마스터 정보 얻어오기
dda_master <- load_dda_master_info()

# 9. 따릉이 일별 사용률 얻어오기 (오래 걸립니다. - 약 90초 - PC 사양마다 차이가 있습니다. 빨간 경고 메시지 나오면 다 된겁니다.)
dda_daily_list <- load_dda_daily_info()

# 10. 따릉이 일별 데이터를 모두 결합 # 총 943,356 건
dda_daily_full <- do.call(rbind, dda_daily_list)

# 11. 일일 데이터에 있는 대여소번호와 마스터 데이터의 대여소_ID 를 매칭시킨 후 merge
## merge 전 id 타입 일치를 위한 변환
dda_daily_full$대여소번호 <- as.numeric(dda_daily_full$대여소번호)
dda_master$대여소_ID <- as.numeric(dda_master$대여소_ID)
dda_merge1 <- merge(dda_daily_full, dda_master, by.x = "대여소번호", by.y = "대여소_ID", all.x = TRUE)

#################
# !!! 여기에서 제공되는 대여소번호와 대여소_ID 가 매칭되지 않아 결측치 43,393 건 발생. 해결 필요.
#################

# 12. 주소1 '구' 정보를 통해 날씨 정보를 가져오기 전, 날씨 측정 지점을 가져옴
dda_merge2 <- merge(dda_merge1, weather_category, by.x = "주소1", by.y = "지점명", all.x = TRUE)

# 13. 날짜와 지점 정보를 이용하여 날씨 정보들을 가져옴
dda_merge3 <- merge(dda_merge2, weather_2022, by.x = c("대여일자", "지점"), by.y = c("일시", "지점"), all.x = TRUE)

# 14. 대여일자와 주소1을 이용하여 미세먼지 정보를 가져옴
## 날짜 형식 통일
airpolution_2022$측정일시 <- ymd(airpolution_2022$측정일시)
dda_merge4 <- merge(dda_merge3, airpolution_2022, by.x = c("대여일자", "주소1"), by.y = c("측정일시", "측정소명"), all.x = TRUE)

# 15. 공휴일 정보 읽어오기
data_holiday <- read_xml("data/holiday/korea_holidays_2018_2022.xml")
datetime <- xml2::xml_find_all(data_holiday, "//item/locdate") %>% xml2::xml_text()
isHoliday <- xml2::xml_find_all(data_holiday, "//item/isHoliday") %>% xml2::xml_text()
df_holiday <- data.frame(datetime, isHoliday)
df_holiday$datetime <- as.Date(df_holiday$datetime, format="%Y%m%d")

sum(is.na(df_holiday)) # 결측치 0

# 16. 공휴일 정보 merge
dda_merge5 <- dda_merge4 # 데이터 백업
dda_merge5$holiday <- "N" # 초기화
dda_merge5$holiday[dda_merge4$대여일자 %in% df_holiday$datetime] <- "Y"

head(dda_merge5)