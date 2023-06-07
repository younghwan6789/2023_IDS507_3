### 2022년 데이터 중 가장 사용량이 많은 달, 사용량이 많은 대여소를 알아보기 위한 분석
source('libraries.R')     # 라이브러리
source('utils.R')         # 유틸리티
source('data.R')          # data file 관련 함수
source('preprocessing.R')

df_2022 <- get_ddarung_daily_info_2022("full")

summary(df_2022)


df_2022[order(-df_2022$총이용건수), ]  # 총이용건수를 기준으로 내림차순 정렬

result <- df_2022 %>%
  group_by(대여소번호) %>%
  summarise(총이용건수 = sum(총이용건수))

# 결과 출력
print(result)

result[order(-result$총이용건수)[1:10], ]

ggplot(result[order(-result$총이용건수)[1:10], ], aes(x = as.factor(대여소번호), y = 총이용건수)) +
  geom_bar(stat = "identity") +
  xlab("대여소번호") +
  ylab("총이용건수") +
  ggtitle("대여소별 총이용건수") +
  theme_minimal()


## 상위 3개 대여소 - 2715, 502, 207
df_station <- read_csv("data/ddarung/공공자전거 대여소 정보(22.12월 기준)_mod.csv", locale=locale('ko', encoding='euc-kr'))

df_station[df_station$`대여소 번호` == 2715, ]
# 강서구, 마곡나루역 2번 출구
df_station[df_station$`대여소 번호` == 502, ]
# 광진구, 뚝섬유원지역 1번출구
df_station[df_station$`대여소 번호` == 207, ]
# 영등포구, 여의나루역 1번출구

