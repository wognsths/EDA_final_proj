library(shiny)
library(tidyverse)
library(ggplot2)
library(sf)

Crime_Data <- read_csv("Crime_Data_from_2020_to_Present.csv")
Police_station <- read_csv("~/EDA_final_proj/Sheriff_and_Police_Stations.csv")

Cd <- Crime_Data %>%
  mutate(
    `DATE OCC` = as.Date(`DATE OCC`, format = "%m/%d/%Y"),
    `Date Rptd` = as.Date(`Date Rptd`, format = "%m/%d/%Y"),
    OCC_year = year(`DATE OCC`),
    `Dur Rptd` = as.numeric(`Date Rptd` - `DATE OCC`)
  ) %>%
  filter(
    OCC_year == 2023,
    LAT != 0,
    LON != 0,
    `Dur Rptd` > 0
  )

Police_station %>% colnames()

PD_data <- Police_station %>%
  select(., c(name, latitude, longitude)) # y: latitude, x: longitude

Data <- Cd %>% select(., c(LAT, LON)) %>% as.matrix()
Criteria_point <- PD_data %>% select(., c(latitude, longitude)) %>% as.matrix()

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

Dist_matrix <- Distance_Calculator(Criteria_point, Data, 2)

Dist_matrix %>% apply(., 1, min) %>% as.data.frame() %>%
  ggplot(., aes(x = .)) + geom_density()

Dist_matrix <- Distance_Calculator(Criteria_point, Data, 1)

Dist_matrix %>% apply(., 1, min) %>% as.data.frame() %>%
  ggplot(., aes(x = .)) + geom_density()



