################################################################################
# 2. 파일 로딩
################################################################################
source('libraries.R')     # 라이브러리
source('utils.R')         # 유틸리티
source('data.R')          # data file 관련 함수
source('eda.R')           # EDA 관련 함수
source('validation.R')    # 검증 관련 함수
source('visualization.R') # 시각화 관련 함수



###################################### 2018 1 start
df_file <- read_csv("data/ddarung/2018년_자전거이용통계_일_1.csv", locale=locale('ko', encoding='euc-kr'))

df_file <- subset(df_file, select = -c(`'대여소'`, `'대여구분코드'`, `'SEX_CD'`, `'연령대코드'`, `'운동량'`, `'탄소량'`, `'이동거리(M)'`, `'이동시간(분)'`))

df_file <- df_file %>%
  group_by(`'대여일자'`, `'대여소번호'`) %>%
  summarize(`'총이용건수'` = sum(`'이용건수'`))

df_502 = df_file[df_file$`'대여소번호'`=="'502'",]

#sum(is.na(df_502))
#df_502[!complete.cases(df_502), ]
#print(df_502, n=154)

full_merged_502 <- na.omit(df_502)
###################################### 2018 1 finish
###################################### 2018 2 start
df_file <- read_csv("data/ddarung/2018년_자전거이용통계_일_2.csv", locale=locale('ko', encoding='euc-kr'))

df_file <- subset(df_file, select = -c(`'대여소'`, `'대여구분코드'`, `'SEX_CD'`, `'연령대코드'`, `'운동량'`, `'탄소량'`, `'이동거리(M)'`, `'이동시간(분)'`))
df_file <- df_file %>%
  group_by(`'대여일자'`, `'대여소번호'`) %>%
  summarize(`'총이용건수'` = sum(`'이용건수'`))

#sum(is.na(df_file))
#head(df_file)

df_502 = df_file[df_file$`'대여소번호'`=="'502'",]

full_merged_502 <- rbind(full_merged_502, df_502)

###################################### 2018 2 finish
###################################### 2018 3 start
df_file <- read_csv("data/ddarung/서울특별시 공공자전거 이용 정보(2018.7.1~12.31)3.csv", 
                    locale = locale('ko', encoding = 'utf-8'), 
                    col_names = c('대여일자', '대여소번호', '대여소', '대여구분코드', 'SEX_CD', '연령대코드', '이용건수', '운동량', '탄소량', '이동거리(M)', '이동시간(분)'))

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, SEX_CD, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이동시간(분)`))

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

#sum(is.na(df_file))
#head(df_file)

df_file$대여소번호 <- as.numeric(df_file$대여소번호) # 숫자 앞에 '00' 제거

df_502 = df_file[df_file$대여소번호==502,]

#summary(full_merged_502)
#summary(df_502)

colnames(full_merged_502) <- c("대여일자", "대여소번호", "총이용건수")

temp <- full_merged_502
#full_merged_502 <- temp
full_merged_502$대여일자 <- ymd(full_merged_502$대여일자)
full_merged_502$대여소번호 <- as.numeric(gsub("'", "", full_merged_502$대여소번호))

#summary(full_merged_502)
#head(full_merged_502)

full_merged_502 <- rbind(full_merged_502, df_502)
###################################### 2018 3 finish
full_2018_502 <- full_merged_502
###################################### 2019 1 start
df_file <- read_csv("data/ddarung/서울특별시 공공자전거 이용 정보(2019.1.1~5.31)4.csv", 
                    locale = locale('ko', encoding = 'utf-8'), 
                    col_names = c('대여일자', '대여소번호', '대여소', '대여구분코드', 'SEX_CD', '연령대코드', '이용건수', '운동량', '탄소량', '이동거리(M)', '이동시간(분)'))

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, SEX_CD, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이동시간(분)`))

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

#sum(is.na(df_file))
#head(df_file)

df_file$대여소번호 <- as.numeric(df_file$대여소번호) # 숫자 앞에 '00' 제거

df_502 = df_file[df_file$대여소번호==502,]

#summary(df_502)

full_merged_502 <- df_502
###################################### 2019 1 finish
###################################### 2019 2 start
df_file <- read_csv("data/ddarung/서울특별시 공공자전거 이용정보_2019.6.csv", locale=locale('ko', encoding='euc-kr'))

#summary(df_file)

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_502 = df_file[df_file$대여소번호==502,]

#temp <- full_merged_502
full_merged_502 <- rbind(full_merged_502, df_502)
###################################### 2019 2 finish
###################################### 2019 3 start
df_file <- read_csv("data/ddarung/서울특별시 공공자전거 이용정보_2019.7.csv", locale=locale('ko', encoding='euc-kr'))

#summary(df_file)

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_502 = df_file[df_file$대여소번호==502,]
#sum(is.na(df_502))
#temp <- full_merged_502
full_merged_502 <- rbind(full_merged_502, df_502)
#sum(is.na(full_merged_502))
###################################### 2019 3 finish
###################################### 2019 4 start
df_file <- read_csv("data/ddarung/서울특별시 공공자전거 이용정보_2019.8.csv", locale=locale('ko', encoding='euc-kr'))

#summary(df_file)

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_502 = df_file[df_file$대여소번호==502,]
#sum(is.na(df_502))
#temp <- full_merged_502
full_merged_502 <- rbind(full_merged_502, df_502)
#sum(is.na(full_merged_502))
###################################### 2019 4 finish
###################################### 2019 5 start
df_file <- read_csv("data/ddarung/서울특별시 공공자전거 이용정보_2019.9.csv", locale=locale('ko', encoding='euc-kr'))

#summary(df_file)

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_502 = df_file[df_file$대여소번호==502,]
#sum(is.na(df_502))
#temp <- full_merged_502
full_merged_502 <- rbind(full_merged_502, df_502)
#sum(is.na(full_merged_502))
###################################### 2019 5 finish
###################################### 2019 6 start
df_file <- read_csv("data/ddarung/서울특별시 공공자전거 이용정보_2019.10.csv", locale=locale('ko', encoding='euc-kr'))

#summary(df_file)

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_502 = df_file[df_file$대여소번호==502,]
#sum(is.na(df_502))
#temp <- full_merged_502
full_merged_502 <- rbind(full_merged_502, df_502)
#sum(is.na(full_merged_502))
###################################### 2019 6 finish
###################################### 2019 7 start
df_file <- read_csv("data/ddarung/서울특별시 공공자전거 이용정보_2019.11.csv", locale=locale('ko', encoding='euc-kr'))

#summary(df_file)

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_502 = df_file[df_file$대여소번호==502,]
#sum(is.na(df_502))
#temp <- full_merged_502
full_merged_502 <- rbind(full_merged_502, df_502)
#sum(is.na(full_merged_502))
###################################### 2019 7 finish
###################################### 2019 8 start
df_file <- read_csv("data/ddarung/공공자전거 이용정보(일별)_2019.12.csv", locale=locale('ko', encoding='euc-kr'))

#summary(df_file)

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_file$대여소번호 <- as.numeric(df_file$대여소번호) # 숫자 앞에 '00' 제거

df_502 = df_file[df_file$대여소번호==502,]
#sum(is.na(df_502))

#temp <- full_merged_502
full_merged_502 <- rbind(full_merged_502, df_502)
#sum(is.na(full_merged_502))

###################################### 2019 8 finish
full_2019_502 <- full_merged_502
###################################### 2020 1 start
df_file <- read_csv("data/ddarung/공공자전거 이용정보(일별)_2020.01~05.csv", locale=locale('ko', encoding='euc-kr'))

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_file$대여소번호 <- as.numeric(df_file$대여소번호) # 숫자 앞에 '00' 제거

df_502 = df_file[df_file$대여소번호==502,]

#temp <- full_merged_502
full_merged_502 <- df_502
###################################### 2020 1 finish
###################################### 2020 2 start
df_file <- read_csv("data/ddarung/공공자전거 이용정보(일별)_2020.06.csv", locale=locale('ko', encoding='euc-kr'))

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_file$대여소번호 <- as.numeric(df_file$대여소번호) # 숫자 앞에 '00' 제거

df_502 = df_file[df_file$대여소번호==502,]

full_merged_502 <- rbind(full_merged_502, df_502)

summary(df_502)
sum(is.na(df_502))
summary(full_merged_502)
sum(is.na(full_merged_502))
dim(full_merged_502)
###################################### 2020 2 finish
###################################### 2020 3 start
df_file <- read_csv("data/ddarung/공공자전거 이용정보(일별)_2020.07~12.csv", locale=locale('ko', encoding='euc-kr'))

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_file$대여소번호 <- as.numeric(df_file$대여소번호) # 숫자 앞에 '00' 제거

df_502 = df_file[df_file$대여소번호==502,]

#temp <- full_merged_502
full_merged_502 <- rbind(full_merged_502, df_502)

summary(df_502)
sum(is.na(df_502))
summary(full_merged_502)
sum(is.na(full_merged_502))
dim(full_merged_502)

###################################### 2020 3 finish
full_2020_502 <- full_merged_502
###################################### 2021 1 start
df_file <- read_csv("data/ddarung/공공자전거 이용정보(일별)_2021.01.csv", locale=locale('ko', encoding='euc-kr'))

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_file$대여소번호 <- as.numeric(df_file$대여소번호) # 숫자 앞에 '00' 제거

df_502 = df_file[df_file$대여소번호==502,]

#temp <- full_merged_502
full_merged_502 <- df_502

summary(df_502)
sum(is.na(df_502))
summary(full_merged_502)
sum(is.na(full_merged_502))
dim(full_merged_502)

summary(df_file)
###################################### 2021 1 finish
###################################### 2021 2 start
df_file <- read_csv("data/ddarung/공공자전거 이용정보(일별)_2021.02.csv", locale=locale('ko', encoding='euc-kr'))

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_file$대여소번호 <- as.numeric(df_file$대여소번호) # 숫자 앞에 '00' 제거

df_502 = df_file[df_file$대여소번호==502,]

temp <- full_merged_502
full_merged_502 <- rbind(full_merged_502, df_502)

summary(df_502)
sum(is.na(df_502))
dim(df_502)

summary(full_merged_502)
sum(is.na(full_merged_502))
dim(full_merged_502)

###################################### 2021 2 finish
###################################### 2021 3 start
df_file <- read_csv("data/ddarung/공공자전거 이용정보(일별)_2021.03.csv", locale=locale('ko', encoding='euc-kr'))

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_file$대여소번호 <- as.numeric(df_file$대여소번호) # 숫자 앞에 '00' 제거

df_502 = df_file[df_file$대여소번호==502,]

temp <- full_merged_502
full_merged_502 <- rbind(full_merged_502, df_502)

summary(df_502)
sum(is.na(df_502))
dim(df_502)

summary(full_merged_502)
sum(is.na(full_merged_502))
dim(full_merged_502)
###################################### 2021 3 finish
###################################### 2021 4 start
df_file <- read_csv("data/ddarung/공공자전거 이용정보(일별)_2021.04.csv", locale=locale('ko', encoding='euc-kr'))

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_file$대여소번호 <- as.numeric(df_file$대여소번호) # 숫자 앞에 '00' 제거

df_502 = df_file[df_file$대여소번호==502,]

temp <- full_merged_502
full_merged_502 <- rbind(full_merged_502, df_502)

summary(df_502)
sum(is.na(df_502))
dim(df_502)

summary(full_merged_502)
sum(is.na(full_merged_502))
dim(full_merged_502)

###################################### 2021 4 finish
###################################### 2021 5 start
df_file <- read_csv("data/ddarung/공공자전거 이용정보(일별)_2021.05.csv", locale=locale('ko', encoding='euc-kr'))

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_file$대여소번호 <- as.numeric(df_file$대여소번호) # 숫자 앞에 '00' 제거

df_502 = df_file[df_file$대여소번호==502,]

temp <- full_merged_502
full_merged_502 <- rbind(full_merged_502, df_502)

summary(df_502)
sum(is.na(df_502))
dim(df_502)

summary(full_merged_502)
sum(is.na(full_merged_502))
dim(full_merged_502)

###################################### 2021 5 finish

###################################### 2021 6 start
df_file <- read_csv("data/ddarung/공공자전거_이용정보(일별)_2021.06_재산출.csv", locale=locale('ko', encoding='euc-kr'))

# -> 깨져서 안 읽힘



###################################### 2021 6 finish
###################################### 2021 7 start
df_file <- read_csv("data/ddarung/공공자전거 이용정보(일별)_2107.csv", locale=locale('ko', encoding='euc-kr'))

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대코드, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_file$대여소번호 <- as.numeric(df_file$대여소번호) # 숫자 앞에 '00' 제거

df_502 = df_file[df_file$대여소번호==502,]

temp <- full_merged_502
full_merged_502 <- rbind(full_merged_502, df_502)

summary(df_502)
sum(is.na(df_502))
dim(df_502)

summary(full_merged_502)
sum(is.na(full_merged_502))
dim(full_merged_502)

###################################### 2021 7 finish
###################################### 2021 8 start
df_file <- read_csv("data/ddarung/공공자전거 이용정보(일별)_2108.csv", locale=locale('ko', encoding='euc-kr'))

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

summary(df_file)

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_file$대여소번호 <- as.numeric(df_file$대여소번호) # 숫자 앞에 '00' 제거

df_502 = df_file[df_file$대여소번호==502,]

temp <- full_merged_502
full_merged_502 <- rbind(full_merged_502, df_502)

summary(df_502)
sum(is.na(df_502))
dim(df_502)

summary(full_merged_502)
sum(is.na(full_merged_502))
dim(full_merged_502)

###################################### 2021 8 finish
###################################### 2021 9 start
df_file <- read_csv("data/ddarung/공공자전거 이용정보(일별)_2109.csv", locale=locale('ko', encoding='euc-kr'))

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

summary(df_file)

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_file$대여소번호 <- as.numeric(df_file$대여소번호) # 숫자 앞에 '00' 제거

df_502 = df_file[df_file$대여소번호==502,]

temp <- full_merged_502
full_merged_502 <- rbind(full_merged_502, df_502)

summary(df_502)
sum(is.na(df_502))
dim(df_502)

summary(full_merged_502)
sum(is.na(full_merged_502))
dim(full_merged_502)

###################################### 2021 9 finish
###################################### 2021 10 start
df_file <- read_csv("data/ddarung/공공자전거 이용정보(일별)_2110.csv", locale=locale('ko', encoding='euc-kr'))

df_file <- subset(df_file, select = -c(대여소, 대여구분코드, 성별, 연령대, 운동량, 탄소량, `이동거리(M)`, `이용시간(분)`))

summary(df_file)

df_file <- df_file %>%
  group_by(대여일자, 대여소번호) %>%
  summarize(총이용건수 = sum(이용건수))

df_file$대여소번호 <- as.numeric(df_file$대여소번호) # 숫자 앞에 '00' 제거

df_502 = df_file[df_file$대여소번호==502,]

temp <- full_merged_502
full_merged_502 <- rbind(full_merged_502, df_502)

summary(df_502)
sum(is.na(df_502))
dim(df_502)

summary(full_merged_502)
sum(is.na(full_merged_502))
dim(full_merged_502)

###################################### 2021 10 finish
###################################### 2021 11 start
df_file <- read_csv("data/ddarung/공공자전거 이용정보(일별)_2111.csv", locale=locale('ko', encoding='euc-kr'))

## -> 여기도 깨짐

###################################### 2021 11 inish
###################################### 2021 12 start
df_file <- read_csv("data/ddarung/공공자전거 이용정보(일별)_2112.csv", locale=locale('ko', encoding='euc-kr'))

# -> 이것도 깨짐

###################################### 2021 12 finish
full_2021_502 <- full_merged_502
###################################### 2022 1 start
source('data.R')
df_file <- get_ddarung_daily_info_2022("full")


summary(df_file)
head(df_file)

df_502 = df_file[df_file$대여소번호==502,]

###################################### 2022 1 finish
full_2022_502 <- df_502

#######################################

df_full_502 <- rbind(full_2018_502, full_2019_502, full_2020_502, full_2021_502, full_2022_502)

dim(df_full_502)
summary(df_full_502)
str(df_full_502)

#### ~~~~~~~~~~~~~~

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
sum(is.na(df_airpolution))


### 따릉이 데이터와 대여소 데이터 결합
df_merge1 <- merge(df_full_502, df_station, by.x = "대여소번호", by.y = "대여소 번호", all.x = TRUE)

### 따릉이 데이터와 날씨 데이터를 연결하기 위한 custom matching data 결합  
df_merge2 <- merge(df_merge1, df_matching_info, by.x ="자치구", by.y = "지점명", all.x = TRUE)
df_merge2[df_merge2$자치구 == "종로구" & is.na(df_merge2$지점), "지점"] <- 415 # 종로구는 날씨 측정소가 없음. 용산구로 치환.

### 따릉이 데이터와 날씨 데이터 결합
df_merge3 <- merge(df_merge2, df_weather, by.x = c("대여일자", "지점"), by.y = c("일시", "지점"), all.x = TRUE)
sum(is.na(df_merge3))

df_merge3[!complete.cases(df_merge3), ] # 두 줄 결측

# 2018-05-18, 2020-05-07 넣어야 하는데 나중에 하자
# util에 create 그거랑 preprocessing에 구로구 데이터 넣으면 됨
temp <- df_merge3
df_merge3 <- na.omit(df_merge3)

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

sample_split = sample.split(Y = all_full_ready, SplitRatio = 0.8)

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

df_502 = df_file[df_file$대여소번호==502,]

#temp <- full_merged_502
full_merged_502 <- rbind(full_merged_502, df_502)

summary(df_502)
sum(is.na(df_502))
summary(full_merged_502)
sum(is.na(full_merged_502))
dim(full_merged_502)
###################################### 2021 1 finish
###################################### 2021 2 start
###################################### 2021 2 finish
###################################### 2021 3 start
###################################### 2021 3 finish

summary(df_file)











sum(is.na(df_502))
df_502[!complete.cases(df_502), ]
print(df_502, n=154)

full_merged_502 <- na.omit(df_502)


df_file <- subset(df_file, select = -c(`'대여소'`, `'대여구분코드'`, `'SEX_CD'`, `'연령대코드'`, `'운동량'`, `'탄소량'`, `'이동거리(M)'`, `'이동시간(분)'`))

df_file <- df_file %>%
  group_by(`'대여일자'`, `'대여소번호'`) %>%
  summarize(`'총이용건수'` = sum(`'이용건수'`))

df_502 = df_file[df_file$`'대여소번호'`=="'502'",]

sum(is.na(df_502))
df_502[!complete.cases(df_502), ]
print(df_502, n=154)

full_merged_502 <- na.omit(df_502)






sum(is.na(full_merged_502))
str(full_merged_502)
summary(full_merged_502)
summary(temp)
sum(is.na(full_merged_502))
summary(full_merged_502)



sum(is.na(df_502))
df_502[!complete.cases(df_502), ]
print(df_502, n=154)
df_502 <- na.omit(df_502)
