## This file is for data analysis & data loaders
library(tidyr);library(ggplot2);library(sf);library(lubridate);library(dplyr);library(readr)
library(data.table);library(RColorBrewer);library(ggmosaic);library(gridExtra);library(plotly);library(kableExtra)

Crime_Data <- read_csv("Crime_Data_2023.csv")
zipcodes_final <- read_csv("zipcodes_final.csv") # to be added differently
Police_station <- read_csv("Sheriff_and_Police_Stations.csv")
boundary <- st_read("LAPD_Div/LAPD_Divisions.shp")

boundary.1 <- boundary %>% mutate(id = as.character(APREC))
boundary <- st_cast(boundary.1, "POLYGON")
boundary_df <- do.call(rbind, lapply(1:nrow(boundary), function(i) {
  poly <- boundary[i,]
  coords <- st_coordinates(poly) # X, Y, ..., L1, L2
  # coords에서 X, Y 추출
  data.frame(
    id = poly$id,
    long = coords[,1],
    lat = coords[,2],
    # group 컬럼은 id 기반으로 설정 (한 id 내에 멀티폴리곤 있으면 L2 포함해서 구분 필요)
    group = paste0(poly$id, "_", if("L2" %in% colnames(coords)) coords[,"L2"] else 1),
    stringsAsFactors = FALSE
  )
}))

boundary <- st_transform(boundary, crs = 4326)

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

daily_crime_by_area <- CD %>% 
  group_by(`AREA NAME`) %>%
  count(name = "total_crime") %>%
  left_join(zipcodes_final, by = "AREA NAME") %>%
  mutate(dailycrimepercent = total_crime / Total_population / 365 * 100) %>%
  rename(AREA_NAME = `AREA NAME`) %>%
  select(AREA_NAME, dailycrimepercent, total_crime)

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

CD <- CD %>%
  mutate(Crm.Cd.Group = recode(Crm.Cd.Group, !!!mapping))
Geo_CD <- Geo_CD %>%
  mutate(Crm.Cd.Group = recode(Crm.Cd.Group, !!!mapping))

saveRDS(Geo_CD, "Geo_CD.rds")
saveRDS(CD, "CD.rds")
saveRDS(Mosaic_CD, "Mosaic_CD.rds")

### Other Functions / Data Tables for Data Analysis ###

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