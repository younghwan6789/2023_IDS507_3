source('preprocessing.R')

################################################################################
# 따릉이 대여소 정보(2022. 12. 기준)
################################################################################
get_ddarung_station_info <- function() {
  df <- read_csv("data/ddarung/공공자전거 대여소 정보(22.12월 기준)_mod.csv", locale=locale('ko', encoding='euc-kr'))
  
  df$`대여소 번호` <- as.numeric(df$`대여소 번호`)
  
  # 대여소 번호와 자치구 정보만 사용
  return(df[, c("대여소 번호", "자치구")])
}

################################################################################
# 따릉이 일별 사용줄 정보
#
# 1~12월 full load 시 90초
# 5, 9월 load 시 10초
################################################################################
get_ddarung_daily_info <- function() {
  ### full load
  #file_names_upper <- c("22.01", "22.02", "22.03", "22.04", "22.05", "22.06")
  #file_names_lower <- c("22.07", "22.08", "2209", "2210", "2211", "2212")

  ### 5월 & 9월 case
  file_names_upper <- c("22.05")
  file_names_lower <- c("2209")
  
  df_list <- list()
  
  for (file_name in file_names_upper) {
    file_path <- paste0("data/ddarung/서울특별시 공공자전거 이용정보(일별)_", file_name, ".csv")
    df <- read_csv(file_path, locale=locale('ko', encoding='euc-kr'))
    df <- subset(df, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))
    df <- df %>%
      group_by(대여일자, 대여소번호) %>%
      summarize(총이용건수 = sum(이용건수))
    
    df_list[[file_name]] <- df
  }
  
  for (file_name in file_names_lower) {
    file_path <- paste0("data/ddarung/서울특별시 공공자전거 이용정보(일별)_", file_name, ".csv")
    df <- read_csv(file_path, locale=locale('ko', encoding='euc-kr'))
    df <- subset(df, select = -c(대여소, 대여구분코드, 성별, 연령대, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))
    df <- df %>%
      group_by(대여일자, 대여소번호) %>%
      summarize(총이용건수 = sum(이용건수))
    
    df_list[[file_name]] <- df
  }
  
  
  # 일별 데이터를 모두 결합
  # 1~12월 총 : 945,356 건
  # 5, 9월    : 158,721 건
  df_binded <- do.call(rbind, df_list)
  
  df_binded$대여소번호 <- as.numeric(df_binded$대여소번호) # 숫자 앞에 '00' 제거
  
  return(df_binded)
}

################################################################################
# 날씨 정보
################################################################################
get_weather_info <- function() {
  df <- read_csv("data/weather/OBS_AWS_DD_20230531190259_2022.csv", locale=locale('ko', encoding='euc-kr'))
  
  # 지점명 삭제, 지점 ID를 대여소 번호와 매칭 예정.
  return(subset(df, select = -c(지점명)))
}

################################################################################
# 날씨 - 자치구 - 대여소 매칭을 위한 custom file
################################################################################
get_matching_info <- function() {
  return(read_csv("data/weather/category_weather.csv", locale=locale('ko', encoding='euc-kr')))
}

################################################################################
# 대기오염도
################################################################################
get_airpolution_info <- function() {
  return(read_csv("data/airpolution/일별평균대기오염도_2022.csv", locale=locale('ko', encoding='euc-kr')))  
}

################################################################################
# 공휴일 정보
################################################################################
get_holiday_info <- function() {
  data_holiday <- read_xml("data/holiday/korea_holidays_2022.xml")
  
  datetime <- xml2::xml_find_all(data_holiday, "//item/locdate") %>% xml2::xml_text()
  isHoliday <- xml2::xml_find_all(data_holiday, "//item/isHoliday") %>% xml2::xml_text()
  
  df_holiday <- data.frame(datetime, isHoliday)
  df_holiday$datetime <- as.Date(df_holiday$datetime, format="%Y%m%d")
  
  return(df_holiday)
}

################################################################################
# 통합 데이터
################################################################################
get_full_merged_info <- function() {
### 각각 로딩
  df_station <- get_ddarung_station_info()
  df_daily <- get_ddarung_daily_info()
  df_weather <- get_weather_info()
  df_airpolution <- get_airpolution_info()
  df_matching_info <- get_matching_info()
  df_holiday <- get_holiday_info()
  
### 전처리
  df_weather <- pre_proc_weather(df_weather)
  df_airpolution <- pre_proc_airpolustion(df_airpolution)
  
### 따릉이 데이터와 대여소 데이터 결합
  df_merge1 <- merge(df_daily, df_station, by.x = "대여소번호", by.y = "대여소 번호", all.x = TRUE)
  df_merge1 <- na.omit(df_merge1) # 2022년 12월 기준 사라진 대여소 삭제.

### 따릉이 데이터와 날씨 데이터를 연결하기 위한 custom matching data 결합  
  df_merge2 <- merge(df_merge1, df_matching_info, by.x ="자치구", by.y = "지점명", all.x = TRUE)
  df_merge2[df_merge2$자치구 == "종로구" & is.na(df_merge2$지점), "지점"] <- 415 # 종로구는 날씨 측정소가 없음. 용산구로 치환.
  
### 따릉이 데이터와 날씨 데이터 결합
  df_merge3 <- merge(df_merge2, df_weather, by.x = c("대여일자", "지점"), by.y = c("일시", "지점"), all.x = TRUE)
  
### 따릉이 데이터와 대기오염 데이터 결합
  df_merge4 <- merge(df_merge3, df_airpolution, by.x = c("대여일자", "자치구"), by.y = c("측정일시", "측정소명"), all.x = TRUE)

### 따릉이 데이터에 공휴일 정보 추가
  df_merge4$holiday <- "0" # 초기화 0 = 'N'
  df_merge4$holiday[ymd(df_merge4$대여일자) %in% ymd(df_holiday$datetime)] <- "1" # 1 = 'Y'

  
### 자치구 삭제 (지점과 동일한 의미를 가지므로 분석 데이터량 감소를 위해)
  df_merge4 <- subset(df_merge4, select = -c(자치구))
  
### RandomForest에서 특수문자를 이해하지 못하므로 컬럼명 변경
  #colnames(df_merge4) <- c("대여일자","지점", "대여소번호", "총이용건수", "평균기온", "최저기온", "최고기온", "일강수량", "평균풍속", "이산화질소농도", "오존농도", "일산화탄소농도", "아황산가스농도", "미세먼지농도", "초미세먼지농도", "공휴일여부")
  colnames(df_merge4) <- c("datetime","branch", "branch_no", "rent", "avg_temperature", "low_temperature", "high_temperature", "rainy", "windy", "no2_ppm", "o3_ppm", "co_ppm", "so2_ppm", "mise", "chomise", "holiday")
  
  return(df_merge4)
}