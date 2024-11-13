library(tidyverse)
library(stringr)

# crime <- read.csv("Crime_Data_from_2020_to_Present.csv")
# 
# crime %>% names()
# 
# crime_2023 <- crime
# crime_2023$DATE.OCC <- as.Date(crime$DATE.OCC, format = "%m/%d/%Y")
# 
# crime_2023 <- crime_2023 %>%
#   mutate(year = year(`DATE.OCC`),
#          month = month(`DATE.OCC`),
#          day = day(`DATE.OCC`)) %>% subset(year == "2023")
# 
# write.csv(crime_2023, "crime_2023.csv", row.names = FALSE)

crime_2023 <- read.csv("crime_2023.csv")

ggplot(data = crime_2023 %>% subset(LAT != 0 & LON != 0), aes(y = LAT, x= LON, color = Part.1.2)) + 
  geom_point(size = 0.3, alpha = 0.5)
# checking seriousness of the crime based on binary classification given
# part 1 is more serious than part 2
# Part.1.2가 아니라 crime code를 기준으로도 판단할 수 있음 
# Since, crime code의 숫자가 낮을 수록 심각한 범죄를 의미하므로  


crime_2023_seriousness <- crime_2023 %>% group_by(AREA.NAME) %>% 
  summarise(sum_seriousness = sum(Part.1.2, na.rm=TRUE),
                                                 count = n()) %>% 
  mutate(seriousness = sum_seriousness / count )

View(crime_2023_seriousness) # there is more serious crime as seriousness is small

# 상대적 우범지역 상위 5개
crime_2023_seriousness %>% arrange(seriousness)

ggplot(data = crime_2023 %>% filter(LAT != 0 & LON != 0 &
                                      AREA.NAME %in% c("Pacific", "Wilshire", "West LA", "Central", "Newton")),
       aes(y = LAT, x= LON, color = Part.1.2)) + 
  geom_point(size = 0.3, alpha = 0.5) 


# 상대적 안전지역 상위 5개
crime_2023_seriousness %>% arrange(desc(seriousness))

ggplot(data = crime_2023 %>% filter(LAT != 0 & LON != 0 &
                                      AREA.NAME %in% c("Foothill", "Mission", "Southeast", "Harbor", "Hollenbeck")),
       aes(y = LAT, x= LON, color = Part.1.2)) + 
  geom_point(size = 0.3, alpha = 0.5) 

# 종합적 시각
crime_2023_color <- crime_2023 %>%
  filter(LAT != 0, LON != 0) %>%
  mutate(color_group = case_when(
    AREA.NAME %in% c("Foothill", "Mission", "Southeast", "Harbor", "Hollenbeck") ~ "blue",
    AREA.NAME %in% c("Pacific", "Wilshire", "West LA", "Central", "Newton") ~ "red",
    TRUE ~ "other"  # Optional: assign a color for areas not specified
  ))

# 빨강은 우범 지역 파랑은 안전 지역
ggplot(data = crime_2023_color, aes(x = LON, y = LAT, color = color_group)) +
  geom_point(size = 0.3, alpha = 0.5) +
  scale_color_manual(values = c(
    "blue" = "blue",
    "red" = "red",
    "other" = "grey"  
  )) +
  labs(color = "Area Group")

# 가장 위험하다고 판단한 지역의 발생 범죄 구성 보기
View(crime_2023 %>% filter(AREA.NAME == "Pacific") %>% group_by(Crm.Cd.Desc) %>%
  summarise(count = n()))
