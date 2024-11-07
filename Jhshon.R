#Jhshon
library(readr);library(tidyverse)
Crime_Data <- read_csv("Crime_Data_from_2020_to_Present.csv")

Cd <- Crime_Data %>%
  mutate(`DATE OCC` = as.Date(`DATE OCC`, format = "%m/%d/%Y")) %>%
  mutate(year = year(`DATE OCC`),
         month = month(`DATE OCC`),
         day = day(`DATE OCC`)) %>%
  subset(year == "2023" & LAT != 0 & LON != 0) %>%
  select(c(colnames(.)[1], c("year", "month", "day"), colnames(.)[4:28]))

ggplot(data = Cd, aes(y = LAT, x= LON)) + 
  geom_point()

# IDEAS
# 1. 부동산 데이터 끌고와서 집값 vs 범죄율, 지역적 특성이 어떻게 범죄와 연결되는지도 확인해볼 수 있다.
# 2. 일별 통계 또는 주간별 통계와 경제 지표와의 상관성(세부 범죄와도 연관 가능)
# 3. 세부 지역 별 범죄 특징(범죄 세부 / 중범죄 경범죄)
# 4. 바다 인접 지역, 숲 인접 지역의 범죄
# 5. 인종별 범죄(지역적 특징)
# 6. 범죄 시간대와 세부 범죄의 주요 시간대
# 7. 인구 밀도(주간 인구 밀도도 가능) 별 범죄율 비교
# 8. 미국 전체 범죄율과 비교도 가능
# 9. Occur 시간과 Report 시간이 차이가 많이 나는 경우의 범죄 분석