library(tidyverse); library(stringr);library(sf);

crime_2023 <- read.csv("crime_2023.csv")
crime_2023$AREA.NAME <- toupper(crime_2023$AREA.NAME)


crime_2023_CD <- crime_2023 %>% mutate(
  Crm.Cd.Group = case_when(
    Crm.Cd %in% c(110,113) ~ "HOMICIDE",
    Crm.Cd %in% c(121,122,815,820,821) ~ "RAPE",
    Crm.Cd %in% c(210,220) ~ "ROBBERY",
    Crm.Cd %in% c(230,231,235,236,250,251,761,926) ~ "AGG.ASSAULTS",
    Crm.Cd %in% c(310,320) ~ "BURGLARY",
    Crm.Cd %in% c(510,520,433,522) ~ "VEICHLE.THEFT",
    Crm.Cd %in% c(330,331,410,420,421) ~ "BURG.THEFT.FROMVEICHLE",
    Crm.Cd %in% c(350,351,352,353,450,451,452,453) ~ "PERSONAL.THEFT",
    Crm.Cd %in% c(341,343,345,440,441,442,443,444,445,470,471,472,473,474,475,480,485,487,491,648) ~ "OTHER.THEFT",
    TRUE ~ "Part2Crime"
    
  )
) 

crime_2023_CD %>% colnames()

cd_pie <- crime_2023_CD %>% group_by(Crm.Cd.Group) %>% count()
cd_pie
pie(cd_pie$n, labels = cd_pie$Crm.Cd.Group, main = "Pie Chart Example",col = rainbow(nrow(cd_pie)))


# looks at the overall crime
pie_colors <- c("red", "pink", "green", "yellow", "purple", "orange", 
                   "blue", "cyan", "brown", "darkgreen")

ggplot(cd_pie, aes(x = "", y = n, fill = Crm.Cd.Group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "What is the usual crime") +
  scale_fill_manual(values = pie_colors) +
  theme_void()






# select the crime and look in which area it has the most proportion

lapd_areas <- st_read("LAPD_Div/LAPD_Divisions.shp")
add_draw <- st_as_sf(crime_2023_color,coords = c("LON","LAT"),crs = st_crs(boundary))
boundary <- st_as_sf(lapd_areas)
boundary <- st_transform(boundary, crs = 4326)
boundary <- st_transform(boundary, crs = 4326)


crime_2023_CD %>% colnames

crime_2023_CD %>% head()

grouped_data <- crime_2023_CD %>%
  group_by(AREA.NAME, Crm.Cd.Group) %>% 
  count() %>% ungroup()

grouped_data

proportional_data <- grouped_data %>%
  group_by(AREA.NAME) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

proportional_data


proportional_data %>% select(Crm.Cd.Group) %>% unique()

highlight <- (proportional_data %>% filter(Crm.Cd.Group == "PERSONAL.THEFT") %>% 
  arrange(desc(proportion)) %>% head(5))$AREA.NAME

crime_2023_CD %>% filter(Crm.Cd.Group == "PERSONAL.THEFT") %>% select(Crm.Cd.Desc) %>% unique()
# Personal theft 및에 들어가 있는 범죄의 종류를 나중에 같이 띄워주면 이해에 더 쉬울 것 같음.

boundary$highlight <- ifelse(boundary$APREC %in% highlight, "PERSONAL.THEFT", "Others")

ggplot(boundary) +
  geom_sf(aes(fill = highlight)) +
  scale_fill_manual(values = c("PERSONAL.THEFT" = "green", "Others" = "grey")) +
  theme_minimal()








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



# pie chart로 visualize 시킨 후 범죄당 proportion 상위 5개 지역으로 뽑아보자...
