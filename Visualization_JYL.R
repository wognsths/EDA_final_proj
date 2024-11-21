library(tidyverse); library(stringr);library(sf);

crime_2023 <- read.csv("crime_2023.csv")

# 심각 범죄에 따른 구분
crime_2023_seriousness <- crime_2023 %>% group_by(AREA.NAME) %>% 
  summarise(sum_seriousness = sum(Part.1.2, na.rm=TRUE),
            count = n()) %>% 
  mutate(seriousness = sum_seriousness / count )

# 우범지역 상위 5개, 하위 5개 from crime_2023_seriousness
crime_2023_color <- crime_2023 %>%
  filter(LAT != 0, LON != 0) %>%
  mutate(color_group = case_when(
    AREA.NAME %in% c("Foothill", "Mission", "Southeast", "Harbor", "Hollenbeck") ~ "SAFE",
    AREA.NAME %in% c("Pacific", "Wilshire", "West LA", "Central", "Newton") ~ "DANGEROUS",
    TRUE ~ "OTHERS"
  ))


# lapd district boundary를 그린 이후 이에 따른 범죄에 대해 그려보기
lapd_areas <- st_read("LAPD_Div/LAPD_Divisions.shp")

boundary <- st_as_sf(lapd_areas)
boundary <- st_transform(boundary, crs = 4326) # Coordinate Reference Systems (CRS)

add_draw <- st_as_sf(crime_2023_color,coords = c("LON","LAT"),crs = st_crs(boundary))
add_draw

boundary


# drawing the boundary of the lapd district
ggplot(boundary) +
  geom_sf(aes(fill = APREC)) +  # Use the APREC column for the fill aesthetic
  theme_minimal() + 
  labs(fill = "APREC")  # Label for the legend


# selecting certain area in the lapd district
boundary$highlight <- ifelse(boundary$APREC %in% c("MISSION", "HOLLYWOOD","NORTH HOLLYWOOD", "OLYMPIC"), "Highlighted", "Others")

ggplot(boundary) +
  geom_sf(aes(fill = highlight)) +
  scale_fill_manual(values = c("Highlighted" = "green", "Others" = "grey")) +
  theme_minimal()


# specifically pointing out safe and dangerous area! 
ggplot() +
  geom_sf(data = boundary, fill = NA, color = "black") +
  geom_sf(data = add_draw, aes(color =color_group), size = 0.1) + 
  scale_color_manual(
    values = c("SAFE" = "blue", "DANGEROUS" = "red", "OTHERS" = "gray")) +
  labs(title = "Boundary with Latitude and Longitude")



