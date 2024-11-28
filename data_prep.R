## This file is for data analysis & data loaders
library(tidyverse);library(ggplot2);library(sf);library(lubridate);library(data.table)

Crime_Data <- read_csv("Crime_Data_from_2020_to_Present.csv")
Police_station <- read_csv("~/EDA_final_proj/Sheriff_and_Police_Stations.csv")
boundary <- st_read("LAPD_Div/LAPD_Divisions.shp")
boundary <- st_transform(boundary, crs = 4326)

CD <- Crime_Data %>%
  mutate(
    `DATE OCC` = as.Date(`DATE OCC`, format = "%m/%d/%Y"),
    `Date Rptd` = as.Date(`Date Rptd`, format = "%m/%d/%Y"),
    OCC_year = year(`DATE OCC`),
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
      `Crm Cd` %in% c(435,436,437,622,624,625,626,627,647,763,928,930) ~ "SIM.ASSAULTS",
      `Crm Cd` %in% c(310,320) ~ "BURGLARY",
      `Crm Cd` %in% c(510,520,433) ~ "VEICHLE.THEFT",
      `Crm Cd` %in% c(330,331,410,420,421) ~ "BURG.THEFT.FROMVEICHLE",
      `Crm Cd` %in% c(350,351,352,353,450,451,452,453) ~ "PERSONAL.THEFT",
      `Crm Cd` %in% c(341,343,345,440,441,442,443,444,445,470,471,472,473,474,475,480,485,487,491) ~ "OTHER.THEFT",
      TRUE ~ "Part2Crime"
    ),
    hour = floor(as.numeric(`TIME OCC`) / 100)
  ) %>% filter(OCC_year == 2023) %>%
  select(., -c('DR_NO', `Date Rptd`, `DATE OCC`, 
               'AREA', `Rpt Dist No`, `Part 1-2`, `Crm Cd`, `Crm Cd Desc`,
               'Mocodes', 'Status', `Status Desc`, `Crm Cd 1`, `Crm Cd 2`,
               `Crm Cd 3`, `Crm Cd 4`, 'LOCATION', `Cross Street`, 'OCC_year',
               `Premis Cd`, `Premis Desc`, `Weapon Desc`, `Weapon Used Cd`))

### DATALOADER ###
GeoData_Loader <- function() {
  CD %>% 
    filter(LAT != 0, LON != 0, `Dur Rptd` > 0) -> Geo.CD
  
  PD_data <- Police_station %>% select(., c(latitude, longitude))
  Geo.CD %>% mutate(
    L1_dist = Distance_Calculator(PD_data, Geo.CD %>% select(.,c(LAT, LON)), 1) %>% apply(.,1,min),
    L2_dist = Distance_Calculator(PD_data, Geo.CD %>% select(.,c(LAT, LON)), 2) %>% apply(.,1,min)
  ) -> Geo.CD
  return(Geo.CD)
}

MosaicData_Loader <- function() {
  CD %>% 
    select(., c(Severity, weapon_usage, crime_status, vict_sex, `AREA NAME`)) -> Mosaic.CD
  return(Mosaic.CD)
}


### Other Functions / Data Tables for Data Analysis ###

Distance_Calculator <- function(Criteria_point, Data, option = 2) {
  Data <- as.matrix(Data)
  Criteria_point <- as.matrix(Criteria_point)
  
  # Scaling factors
  
  Point_for_real_dist <- Data %>% apply(., 2, range) %>% apply(.,2,mean)
  
  lat_scale <- 111.32  # km per degree latitude
  lon_scale <- 111.32 * cos(Point_for_real_dist[2] * pi / 180)  # km per degree longitude
  
  # Apply scaling to Data and Criteria_point
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
    
    Distance_sq[Distance_sq < 0] <- 0  # Correct for numerical errors
    
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