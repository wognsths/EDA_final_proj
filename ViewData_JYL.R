library(tidyverse); library(stringr);library(sf);

crime <- read.csv("Crime_Data_from_2020_to_Present.csv")
crime %>% group_by(Status.Desc) %>% count()

crime %>% colnames()
crime %>% select(Status.Desc) %>% head()
crime %>% filter(Part.1.2 == 1) %>% group_by(Status.Desc) %>% count()
crime %>% filter(Part.1.2 == 2) %>% group_by(Status.Desc) %>% count()
crime %>% group_by(Part.1.2) %>% count()

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

# 월별 범죄 발생률의 차이 (생각보다 없음! ^^)
crime_2023 %>% group_by(month) %>% summarise(count = n()) %>% mutate(percent = (count / nrow(crime_2023)))
 
# 각 범죄별 발생 빈도수
crime_2023 %>% group_by(Crm.Cd.Desc) %>% summarise(count = n()) %>% 
  mutate(percent = count / nrow(crime_2023)) %>% arrange(desc(count))

# shapefile <- st_read("Median_Income_and_AMI_(census_tract).shp")
# 
# ggplot(data = shapefile) +
#   geom_sf() +
#   labs(title = "Map of Shapefile Data") +
#   theme_minimal()
# 
# shapeshape <- st_centroid(shapefile)
# shapeshape
# 
# shapeshape <- shapeshape %>%
#   mutate(longitude = st_coordinates(.)[,1],
#          latitude = st_coordinates(.)[,2])
# ggplot() +
#   geom_sf(data = shapeshape, fill = NA, color = "grey") + # Plot the map boundaries
#   geom_point(data = shapeshape, aes(x = longitude, y = latitude), color = "blue") +
#   labs(title = "Points on Map") +
#   theme_minimal()

add_draw
crime_2023_color[c("LON","LAT")]

lapd_areas <- st_read("LAPD_Div/LAPD_Divisions.shp")
lapd_areas %>% unique
add_draw["AREA.NAME"] %>% unique
boundary <- st_as_sf(lapd_areas)
boundary <- st_transform(boundary, crs = 4326)

add_draw <- st_as_sf(crime_2023_color,coords = c("LON","LAT"),crs = st_crs(boundary))
add_draw
# Coordinate Reference Systems (CRS)

boundary <- st_transform(boundary, crs = 4326)

ggplot() +
  geom_sf(data = boundary, fill = NA, color = "black") +
  geom_sf(data = add_draw, aes(color =color_group), size = 0.1) +
  labs(title = "Boundary with Latitude and Longitude")



ggplot(data = crime_2023_color, aes(x = LON, y = LAT, color = color_group)) +
  geom_point(size = 0.3, alpha = 0.5) +
  scale_color_manual(values = c(
    "blue" = "blue",
    "red" = "red",
    "other" = "grey"  
  )) +
  labs(color = "Area Group")

crime_2023 %>% select(Status.Desc) %>% unique()
crime_2023 %>% filter(Status.Desc == "UNK") %>% count()
crime_2023 %>% group_by(Status.Desc) %>% count()

crime_2023 %>% select(Premis.Desc) %>% unique() %>% nrow()
crime_2023 %>% select(Crm.Cd.Desc) %>% unique() %>% nrow()
crime_2023 %>% select(Crm.Cd) %>% unique() %>% nrow()

crime_2023 %>% select(Crm.Cd) %>% unique() 

# only includes the serious part1 crime in this part
# we put others as Part2Crime
# 그럼 part 1 crime에 focus 를 해서 접근하는 것은 어떠한지!!
crime_2023_CD <- crime_2023 %>% mutate(
  Crm.Cd.Group = case_when(
    Crm.Cd %in% c(110,113) ~ "HOMICIDE",
    Crm.Cd %in% c(121,122,815,820,821) ~ "RAPE",
    Crm.Cd %in% c(210,220) ~ "ROBBERY",
    Crm.Cd %in% c(230,231,235,236,250,251,761,926) ~ "AGG.ASSAULTS",
    Crm.Cd %in% c(435,436,437,622,624,625,626,627,647,763,928,930) ~ "SIM.ASSAULTS",
    Crm.Cd %in% c(310,320) ~ "BURGLARY",
    Crm.Cd %in% c(510,520,433) ~ "VEICHLE.THEFT",
    Crm.Cd %in% c(330,331,410,420,421) ~ "BURG.THEFT.FROMVEICHLE",
    Crm.Cd %in% c(350,351,352,353,450,451,452,453) ~ "PERSONAL.THEFT",
    Crm.Cd %in% c(341,343,345,440,441,442,443,444,445,470,471,472,473,474,475,480,485,487,491) ~ "OTHER.THEFT",
    TRUE ~ "Part2Crime"
    
  )
) 

crime_2023_CD %>% group_by(Crm.Cd.Group) %>% count() 
crime_2023_CD %>% filter(is.na(Crm.Cd.Group)) %>% select(Crm.Cd) %>% head(10)
crime_2023_CD %>% filter(Crm.Cd == 662) %>% select(Crm.Cd.Desc) %>% head(10)
crime_2023_CD %>% filter(is.na(Crm.Cd.Group)) %>% select(Crm.Cd) %>% unique() %>% nrow()

crime_2023_CD %>% filter(is.na(Crm.Cd.Group)) %>% select(Crm.Cd.Desc) %>% unique() %>% head(10)

crime_2023_CD %>% colnames()
crime_2023_CD %>% filter(Crm.Cd == 662) %>% select(Part.1.2) %>% count()
crime_2023_CD %>% filter(Crm.Cd.Group == "Part2Crime") %>% group_by(Part.1.2) %>% count()
crime_2023_CD %>% group_by(Part.1.2) %>% count()
crime_2023_CD %>% filter(Crm.Cd.Group != "Part2Crime") %>% group_by(Part.1.2) %>% count()
crime_2023_CD %>% filter(Crm.Cd.Group != "Part2Crime" & Part.1.2 == 2) %>% group_by(Crm.Cd.Group) %>% count()
# simple assault 만 뺴주면 위에 내용들은 모두 part 1 crime에 포함이 된다.

crime_2023_CD %>% filter(Crm.Cd.Group == "Part2Crime" & Part.1.2 == 1) %>% select(Crm.Cd.Desc) %>% unique()
crime_2023_CD %>% filter(Crm.Cd.Group == "Part2Crime" & Part.1.2 == 1) %>% select(Crm.Cd) %>% unique()

# Arson <- 방화;Veichle stolen 이거 두개만 추가해주면 순수 part 1 crime 모두 정리하기 가능!
# 648 <- 방화 ; 522 <- 차량 도둑

crime_2023_CD %>% filter(Crm.Cd == 648)


