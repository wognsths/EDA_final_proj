## This file is for data analysis & data loaders
library(tidyr);library(ggplot2);library(sf);library(lubridate);library(dplyr);library(readr)
library(data.table);library(RColorBrewer);library(ggmosaic);library(gridExtra);library(plotly);library(kableExtra)

# 기존 Crime_Data는 아래와 같습니다.
# Crime_Data <- read_csv("Crime_Data_from_2020_to_Present.csv")

Crime_Data <- read_csv("Crime_Data_2023.csv")
Police_station <- read_csv("Sheriff_and_Police_Stations.csv")
boundary <- st_transform(boundary, crs = 4326)
acs_stat <- read.csv("acs_stat.csv") # 추가 import


CD <- Crime_Data %>%
  mutate(
    `DATE OCC` = as.Date(`DATE OCC`, format = "%m/%d/%Y"),
    `Date Rptd` = as.Date(`Date Rptd`, format = "%m/%d/%Y"),
    `OCC_year` = year(`DATE OCC`),
    `Dur Rptd` = as.numeric(`Date Rptd` - `DATE OCC`),
    Severity = factor(`Part 1-2`, levels = c(1, 2), labels = c("Severe", "Less Severe")),
    weapon_usage = ifelse(is.na(`Weapon Used Cd`), "No", "Yes"),
    crime_status = ifelse(`Status Desc` %like% "Juv", "Juvenile",
                          ifelse(`Status Desc` %like% "Adult", "Adult", NA)),
    vict_sex = ifelse(((`Vict Sex` == "-") | (`Vict Sex` == "X") | (`Vict Sex` == "")), NA, `Vict Sex`),
    Crm.Cd.Group = case_when(
      `Crm Cd` %in% c(110,113) ~ "HOMICIDE",
      `Crm Cd` %in% c(121,122,815,820,821) ~ "RAPE",
      `Crm Cd` %in% c(210,220) ~ "ROBBERY",
      `Crm Cd` %in% c(230,231,235,236,250,251,761,926) ~ "AGG.ASSAULTS",
      `Crm Cd` %in% c(310,320) ~ "BURGLARY",
      `Crm Cd` %in% c(510,520,433,522) ~ "VEICHLE.THEFT",
      `Crm Cd` %in% c(330,331,410,420,421) ~ "BURG.THEFT.FROMVEICHLE",
      `Crm Cd` %in% c(350,351,352,353,450,451,452,453) ~ "PERSONAL.THEFT",
      `Crm Cd` %in% c(341,343,345,440,441,442,443,444,445,470,471,472,473,474,475,480,485,487,491,648) ~ "OTHER.THEFT",
      TRUE ~ "PART2 Crime"
    ),
    hour = floor(as.numeric(`TIME OCC`) / 100),
    period = case_when(
      hour >= 6 & hour < 12 ~ "Morning",
      hour >= 12 & hour < 18 ~ "Afternoon",
      hour >= 18 & hour < 24 ~ "Night",
      (hour >= 0 & hour < 6) | hour == 24 ~ "Late Night"
    ),
    `AREA NAME` = toupper(`AREA NAME`),
    `AREA NAME` = case_when(
      `AREA NAME` == "N HOLLYWOOD" ~ "NORTH HOLLYWOOD",
      `AREA NAME` == "WEST LA" ~ "WEST LOS ANGELES",
      TRUE ~ `AREA NAME`,
    ),
    vict_age = case_when(
      `Vict Age` == -1 ~ NA,
      (`Vict Age` == 0) & (is.na(vict_sex)) ~ NA,
      `Vict Age` < 15 ~ "Youth (0~14)",
      `Vict Age` > 64 ~ "Elderly (65~)",
      TRUE ~ "Adult (15~64)"
    ),
    vict_descent = case_when(
      `Vict Descent` %in% c("A", "C", "D", "F", "J", "K", "L", "V", "Z") ~ "Asian",
      `Vict Descent` == "B" ~ "Black",
      `Vict Descent` == "H" ~ "Hispanic",
      `Vict Descent` == "W" ~ "White",
      `Vict Descent` %in% c("G", "P", "S", "U") ~ "Pacific Islander",
      `Vict Descent` == "I" ~ "Native American",
      `Vict Descent` == "O" ~ "Other",
      TRUE ~ "Descent is not Specified"
    ),
    cat_dur_rptd = case_when(
      `Dur Rptd` == 0 ~ "Same-day report",
      `Dur Rptd` > 0 & `Dur Rptd` <= 100 ~ "Reported within 100 days",
      `Dur Rptd` > 100 ~ "Reported after 100 days",
      TRUE ~ NA
    )
  ) %>% filter(`OCC_year` == 2023, LAT != 0, LON != 0, `Dur Rptd` >= 0) %>%
  select(., -c('DR_NO', `Date Rptd`, `DATE OCC`, `TIME OCC`,
               'AREA', `Rpt Dist No`, `Part 1-2`, `Crm Cd`, `Crm Cd Desc`,
               'Mocodes', 'Status', `Status Desc`, `Crm Cd 1`, `Crm Cd 2`,
               `Crm Cd 3`, `Crm Cd 4`, 'LOCATION', `Cross Street`, 'OCC_year',
               `Premis Cd`, `Premis Desc`, `Weapon Desc`, `Weapon Used Cd`))

############### 새로 추가 코드 
areas <- list(
  TOPANGA = c("91304", "91307", "91367", "91364", "91303", "91306"),
  DEVONSHIRE = c("91311", "91324", "91330", "91325", "91343", "91344", "91326"),
  MISSION = c("91342", "91345", "91402", "91340"),
  FOOTHILL = c("91331", "91352", "91042", "91040"),
  WEST_VALLEY = c("91335", "91356", "91316", "91436", "91406"),
  VAN_NUYS = c("91405", "91411", "91403", "91423", "91401"),
  NORTH_HOLLYWOOD = c("91605", "91606", "91607", "91604", "91602", "91601", "91522"),
  WEST_LOS_ANGELES = c("90272", "90049", "90077", "90210", "90095", "90024", "90025", "90064", "90035", "90034", "90067"),
  PACIFIC = c("90232", "90066", "90291", "90094", "90293", "90045"),
  HOLLYWOOD = c("90069", "90046", "90068", "90028", "90038"),
  NORTHEAST = c("90027", "90039", "90065", "90041", "90042"),
  WILSHIRE = c("90048", "90036", "90019"),
  OLYMPIC = c("90004", "90020", "90010", "90005", "90006"),
  RAMPART = c("90029", "90057", "90017", "90026"),
  CENTRAL = c("90071", "90012", "90013", "90014", "90015"),
  HOLLENBECK = c("90031", "90032", "90033", "90063", "90023", "91030"),
  SOUTHWEST = c("90016", "90018", "90007", "90089", "90008", "90062"),
  "77TH_STREET" = c("90043", "90047", "90037", "90044"),
  NEWTON = c("90021", "90011", "90001", "90058"),
  SOUTHEAST = c("90003", "90002", "90059", "90061", "90247", "90248"),
  HARBOR = c("90501", "90710", "90744", "90731", "90732")
)

names(areas) <- gsub("_", " ", names(areas))

zipcodes_data <- data.frame(
  zipcode = unlist(areas)
)

zipcodes_data
acs_stat %>% select(c(ZCTA5A, AQNFE001)) %>% slice(2:n()) %>% head()

zipcodes_data <- zipcodes_data %>%
  rename(ZCTA5A = zipcode) %>%  # Rename `zipcode` to match with `ZCTA5A`
  left_join(acs_stat, by = "ZCTA5A") # Match and join with `acs_stat`

zipcodes_data <- zipcodes_data %>% 
  select(c("ZCTA5A","AQNFE001", "AQNGE002", "AQNGE003", "AQNGE005", "AQP6E001", 
           "AQR8E004", "AQR8E005", "AQR8E007", "AQPKE002", 
           "AQPKE003", "AQPKE004", "AQPKE005", "AQPKE006", "AQPKE007", 
           "AQPKE008", "AQPKE009", "AQPKE010", "AQPKE011", "AQPKE012", 
           "AQPKE013", "AQPKE014", "AQPKE015", "AQPKE016", "AQPKE017", 
           "AQPKE019", "AQPKE020", "AQPKE021", "AQPKE022", "AQPKE023", 
           "AQPKE024", "AQPKE025", "AQPZE002", "AQPZE003", "AQPZE004", 
           "AQPZE005", "AQPZE006", "AQPZE007", "AQPZE008"))

zipcodes_data <- zipcodes_data %>%
  rename(
    zipcode = ZCTA5A,
    total_population = AQNFE001,
    white_population = AQNGE002,
    black_population = AQNGE003,
    asian_population = AQNGE005,
    median_household_income = AQP6E001,
    employed = AQR8E004,
    unemployed = AQR8E005,
    not_in_labor_force = AQR8E007,
    under_elementary_1 = AQPKE002, under_elementary_2 = AQPKE003,
    under_elementary_3 = AQPKE004, under_elementary_4 = AQPKE005,
    under_elementary_5 = AQPKE006, under_elementary_6 = AQPKE007,
    under_elementary_7 = AQPKE008, under_elementary_8 = AQPKE009,
    under_elementary_9 = AQPKE010,
    middle_school_1 = AQPKE011, middle_school_2 = AQPKE012,
    middle_school_3 = AQPKE013,
    high_school_1 = AQPKE014, high_school_2 = AQPKE015,
    high_school_3 = AQPKE016, high_school_4 = AQPKE017,
    over_college_1 = AQPKE019, over_college_2 = AQPKE020,
    over_college_3 = AQPKE021, over_college_4 = AQPKE022,
    over_college_5 = AQPKE023, over_college_6 = AQPKE024,
    over_college_7 = AQPKE025,
    poverty_under_50 = AQPZE002, poverty_50_to_99 = AQPZE003,
    poverty_100_to_124 = AQPZE004, poverty_125_to_149 = AQPZE005,
    poverty_150_to_184 = AQPZE006, poverty_185_to_199 = AQPZE007,
    poverty_200_and_over = AQPZE008
  )

zipcodes_data <- zipcodes_data %>%
  filter(!zipcode %in% c(91522, 91330, 90095, 90071, 90089))

zipcodes_data[] <- apply(zipcodes_data, 2, function(x) as.numeric(as.character(x)))

# 정보들 합쳐주기
zipcodes_data <- zipcodes_data %>%
  mutate(
    across(starts_with("under_elementary"), as.numeric),  # Ensure numeric columns
    across(starts_with("middle_school"), as.numeric),
    across(starts_with("high_school"), as.numeric),
    across(starts_with("over_college"), as.numeric)
  ) %>%
  mutate(
    total_underelementary = rowSums(select(., starts_with("under_elementary")), na.rm = TRUE),
    total_middleschool = rowSums(select(., starts_with("middle_school")), na.rm = TRUE),
    total_highschool = rowSums(select(., starts_with("high_school")), na.rm = TRUE),
    total_overcollege = rowSums(select(., starts_with("over_college")), na.rm = TRUE)
  ) %>%
  select(
    -starts_with("under_elementary"),
    -starts_with("middle_school"),
    -starts_with("high_school"),
    -starts_with("over_college")
  )

area_mapping <- bind_rows(
  lapply(names(areas), function(area) {
    data.frame(AREA.NAME = area, zipcode = (areas[[area]]))
  })
)

area_mapping$zipcode <- area_mapping$zipcode %>% as.numeric()

area_data_intermediate <- zipcodes_data %>%
  inner_join(area_mapping, by = "zipcode") %>%  # Join to map zipcodes to areas
  group_by(AREA.NAME) %>%
  summarise(
    Total_population = sum(total_population, na.rm = TRUE),
    Total_employed = sum(employed, na.rm = TRUE),
    Total_unemployed = sum(unemployed, na.rm = TRUE),
    Total_not_in_labor_force = sum(not_in_labor_force, na.rm = TRUE),
    Total_underelementary = sum(total_underelementary, na.rm = TRUE),
    Total_overcollege = sum(total_overcollege, na.rm = TRUE),
    poverty_under_50 = sum(poverty_under_50, na.rm = TRUE),              
    poverty_50_to_99 = sum(poverty_50_to_99, na.rm = TRUE),
    poverty_100_to_124 = sum(poverty_100_to_124, na.rm = TRUE),
    poverty_125_to_149 = sum(poverty_125_to_149, na.rm = TRUE),
    poverty_150_to_184 = sum(poverty_150_to_184, na.rm = TRUE),
    poverty_185_to_199 = sum(poverty_185_to_199, na.rm = TRUE),
    poverty_200_and_over = sum(poverty_200_and_over, na.rm = TRUE),
    weighted_income_sum = sum(median_household_income * total_population, na.rm = TRUE)
  )

zipcodes_data <- area_data_intermediate %>%
  mutate(median_household_income = as.integer(weighted_income_sum / Total_population)) %>%
  select(-starts_with("weighted_"))  # Remove intermediate weighted sum columns

zipcodes_final <- zipcodes_data %>% rename("AREA NAME" = AREA.NAME)

# 형이 말한 daily_crime_by_area
daily_crime_by_area <- CD %>% 
  group_by(`AREA NAME`) %>%
  count(name = "total_crime") %>%
  left_join(zipcodes_final, by = "AREA NAME") %>%
  mutate(dailycrimepercent = total_crime / Total_population / 365 * 100) %>%
  rename(AREA_NAME = `AREA NAME`) %>%
  select(AREA_NAME, dailycrimepercent, total_crime)


############## 새로 추가 코드 완


### DISTANCE FUNCTION ###
Distance_Calculator <- function(Criteria_point, Data, option = 2) {
  Data <- as.matrix(Data)
  Criteria_point <- as.matrix(Criteria_point)
  
  Point_for_real_dist <- Data %>% apply(., 2, range) %>% apply(.,2,mean)
  
  lat_scale <- 111.32  # km per degree latitude
  lon_scale <- 111.32 * cos(Point_for_real_dist[2] * pi / 180)  # km per degree longitude
  
  Data_scaled <- Data
  Data_scaled[, 1] <- Data[, 1] * lat_scale     # Scale latitude
  Data_scaled[, 2] <- Data[, 2] * lon_scale     # Scale longitude
  
  Criteria_scaled <- Criteria_point
  Criteria_scaled[, 1] <- Criteria_point[, 1] * lat_scale
  Criteria_scaled[, 2] <- Criteria_point[, 2] * lon_scale
  
  n <- nrow(Data_scaled)
  m <- nrow(Criteria_scaled)
  
  if (option == 2) {
    # Euclidean distance
    Data_norms <- rowSums(Data_scaled^2)
    Criteria_norms <- rowSums(Criteria_scaled^2)
    Cross_prod <- Data_scaled %*% t(Criteria_scaled)
    
    Distance_sq <- outer(Data_norms, rep(1, m)) + 
      outer(rep(1, n), Criteria_norms) - 2 * Cross_prod
    
    Distance_sq[Distance_sq < 0] <- 0
    
    Distance <- sqrt(Distance_sq)
    return(Distance)
    
  } else if (option == 1) {
    # Manhattan distance
    p <- ncol(Data_scaled)
    Distance <- matrix(0, n, m)
    
    for (k in 1:p) {
      Distance <- Distance + abs(outer(Data_scaled[, k], Criteria_scaled[, k], "-"))
    }
    return(Distance)
    
  } else {
    stop("Invalid option. Choose 1 for Manhattan distance or 2 for Euclidean distance.")
  }
}

### DATALOADER ###
GeoData_Loader <- function() {
  CD %>% filter(LAT != 0, LON != 0, `Dur Rptd` >= 0) -> Geo.CD
  
  PD_data <- Police_station %>% select(., c(latitude, longitude))
  Geo.CD %>% mutate(
    L1_dist = Distance_Calculator(PD_data, Geo.CD %>% select(.,c(LAT, LON)), 1) %>% apply(.,1,min),
    L2_dist = Distance_Calculator(PD_data, Geo.CD %>% select(.,c(LAT, LON)), 2) %>% apply(.,1,min)
  ) -> Geo.CD
  
  return(Geo.CD)
}

MosaicData_Loader <- function() {
  CD %>% 
    select(., c(Severity, weapon_usage, crime_status, 
                vict_sex, `AREA NAME`, `Dur Rptd`,
                vict_descent, vict_age, cat_dur_rptd, period, hour)) -> Mosaic.CD
  return(Mosaic.CD)
}

Geo_CD <- GeoData_Loader()
Mosaic_CD <- MosaicData_Loader()

### 머신러닝 모델 호환을 위해 Recoding ###
mapping <- c(
  "AGG.ASSAULTS" = "ASSAULT",
  "BURG.THEFT.FROMVEICHLE" = "BURGLARY_THEFT_FROM_VEICHLE",
  "BURGLARY" = "BURGLARY",
  "HOMICIDE" = "HOMICIDE",
  "OTHER.THEFT" = "OTHER_THEFT",
  "PART2 Crime" = "LESS_SEVERE_CRIME",
  "PERSONAL.THEFT" = "PERSONAL_THEFT",
  "RAPE" = "RAPE",
  "ROBBERY" = "ROBBERY",
  "VEICHLE.THEFT" = "VEHICLE_THEFT"
)

CD <- CD %>%
  mutate(Crm.Cd.Group = recode(Crm.Cd.Group, !!!mapping))
Geo_CD <- Geo_CD %>%
  mutate(Crm.Cd.Group = recode(Crm.Cd.Group, !!!mapping))


### SAVE FILES ###
saveRDS(Geo_CD, "Geo_CD.rds")
saveRDS(CD, "CD.rds")
saveRDS(Mosaic_CD, "Mosaic_CD.rds")