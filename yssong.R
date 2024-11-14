library(data.table);library(tidyverse);library(lubridate)
#data <- fread("C:/Users/808da/OneDrive/바탕 화면/EDA_final_proj/Crime_Data_from_2020_to_Present.csv")
#data[, DATE_OCC := as.Date(`DATE OCC`, format = "%m/%d/%Y")]
#data_2023 <- data[year(DATE_OCC) == 2023]
#fwrite(data_2023, file = "C:/Users/808da/OneDrive/바탕 화면/EDA_final_proj/Crime_Data_2023.csv", bom = TRUE)
data <- fread("Crime_Data_2023.csv")
data <- data[LAT != 0 & LON != 0]

# 1.location (LAT, LON)
library(ggplot2);library(ggmap);library(osmdata);library(sf)
#bounding_box <- getbb("Los Angeles")
#la_map <- opq(bbox = bounding_box) %>%
#  add_osm_feature(key = "highway") %>%
#  osmdata_sf()
#ggplot() +
#  geom_sf(data = la_map$osm_lines, color = "gray", size = 0.3) +
#  geom_point(data = data, aes(x = LON, y = LAT), color = "red", size = 2, alpha = 0.7) +
#  labs(title = "Crime Locations in Los Angeles", x = "Longitude", y = "Latitude") +
#  theme_minimal()

##overall
data[, .N, by = .(LAT, LON)] |> summary()
data[, .N, by = .(LAT, LON)][order(N)] |> tail(n = 10)

ggplot(data, aes(x = LON, y = LAT)) +
  geom_point(color = "blue", size = 0.2, alpha = 0.5) +
  labs(title = "Crime Locations", x = "Longitude", y = "Latitude") +
  theme_minimal()

##victim gender
ggplot(data, aes(x = LON, y = LAT)) +
  geom_point(color = "blue", size = 0.2, alpha = 0.5) +
  labs(title = "Crime Locations", x = "Longitude", y = "Latitude") +
  theme_minimal()+
  facet_wrap(~`Vict Sex`)

ggplot(data, aes(x = LON, y = LAT, color = `Vict Sex`))+
  geom_point(size = 0.2, alpha = 0.5)+
  labs(title = "Crime Locations", x = "Longitude", y = "Latitude") +
  theme_minimal()

#ggplot(data, aes(x = LON, y = LAT, color = `Vict Sex`))+
#  geom_density2d()+
#  labs(title = "Crime Locations", x = "Longitude", y = "Latitude", color = "Victim Sex")

##victim race
ggplot(data, aes(x = LON, y = LAT)) +
  geom_point(color = "blue", size = 0.2, alpha = 0.5) +
  labs(title = "Crime Locations", x = "Longitude", y = "Latitude") +
  theme_minimal()+
  facet_wrap(~`Vict Descent`)

##victim age
data[, .N, keyby = `Vict Age`] ## age -1, 0 ?? 
#data[, age_group := cut(as.numeric(Vict_Age), breaks = seq(0, 100, by = 10), right = FALSE, labels = paste(seq(0, 90, by = 10), seq(9, 99, by = 10), sep = "-"))]

#2. time 
## during which time of the day crimes frequently occur (+ area)
time_data <- data[, `:=` (hour = floor(`TIME OCC`/100),
                          DATE_Rptd = as.Date(`Date Rptd`, format = "%m/%d/%Y"))] %>%
  .[, .(DR_NO, DATE_Rptd, DATE_OCC, `TIME OCC`, hour, AREA, `Crm Cd`, `Date Rptd`, `DATE OCC`, `Part 1-2`)]
hour_area <- time_data[, .N, by = .(hour, AREA)]

ggplot(hour_area, aes(x = hour, y = N, fill = AREA)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Crime Area by Hour of Day", x = "Hour", y = "Number of Crimes") +
  theme_minimal()

ggplot(hour_area, aes(x = hour, y = AREA, fill = N)) +
  geom_tile() +
  labs(title = "Crime Area by Hour of Day", x = "Hour", y = "Area", fill = "Number of Crimes") +
  theme_minimal() +
  scale_fill_viridis_c()

## is time related to the seriousness of the crime?
## Part 1-2 variable (1: serious, 2: less serious) 
hour_seriousness <- time_data[, .N, by = .(hour, `Part 1-2`)]
ggplot(hour_seriousness, aes(x = hour, y = N, fill = as.factor(`Part 1-2`))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Time of Day vs Crime Seriousness",
       x = "Hour of Day",
       y = "Count",
       fill = "Crime Seriousness")

## delay of reported crime time 
#time_data[, `:=` (`Date Rptd` = as.POSIXct(`Date Rptd`, format = "%m/%d/%Y %I:%M:%S %p"),
#                  `DATE OCC` = as.POSIXct(`DATE OCC`, format = "%m/%d/%Y %I:%M:%S %p"))]
#time_data[, delay_hours := as.numeric(difftime(`Date Rptd`, `DATE OCC`, units = "hours"))]

#3. status (IC - investigation continued, A~ - adult, J~ - juvenile, CC - unknown?)
status_data <- data[(`Status Desc` %like% "Adult") | (`Status Desc` %like% "Juv")] %>%
  .[, `:=` (juv_crime = ifelse(`Status Desc` %like% "Juv", "Juvenile", "Adult"),
            hour = floor(`TIME OCC`/100))]
juv_time_data <- status_data[, .N, by = .(hour, juv_crime)]

#histogram
ggplot(status_data, aes(x = hour, fill = juv_crime)) +
  geom_histogram(aes(y = ..density..), position = "identity", binwidth = 1, alpha = 0.5) +
  labs(title = "Crime Density by Hour (Adult vs Juvenile)", x = "Hour", y = "Density") +
  facet_wrap(~ juv_crime)+
  theme_minimal() 

#density plot
ggplot(status_data, aes(x = hour, color = juv_crime, fill = juv_crime)) +
  geom_density(alpha = 0.5) +
  labs(title = "Crime Density by Hour (Adult vs Juvenile)", x = "Hour", y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = c("Juvenile" = "lightblue", "Adult" = "pink")) +
  scale_color_manual(values = c("Juvenile" = "blue", "Adult" = "red"))

#4. weapon usage 
weapon_data <- data[, weapon_usage := ifelse(is.na(`Weapon Used Cd`), 0, 1)]

########################
new_data <- data[, `:=` (weapon_usage = ifelse(is.na(`Weapon Used Cd`), 0, 1),
                         juv_crime = ifelse(`Status Desc` %like% "Juv", "Juvenile", 
                                            ifelse(`Status Desc` %like% "Adult", "Adult", "Unknown")),
                         hour = floor(`TIME OCC`/100))]%>%
  .[, juv_crime := factor(juv_crime, levels = c("Juvenile", "Adult", "Unknown"))]
#new_data[, `:=` (`Date Rptd` = as.POSIXct(`Date Rptd`, format = "%m/%d/%Y %I:%M:%S %p"),
#                 `DATE OCC` = as.POSIXct(`DATE OCC`, format = "%m/%d/%Y %I:%M:%S %p"))]
#add `TIME OCC` to `DATE OCC`? we do not know the exact reported time... -> days of delay instead of hours? 
#new_data[, delay_hours := as.numeric(difftime(`Date Rptd`, `DATE OCC`, units = "hours"))]
#model <- lm(delay_hours ~ `Crm Cd Desc` + hour + `Vict Sex` + `Weapon Used Cd` + juv_crime, data = new_data)
#summary(model)

new_data[, `:=` (`Date Rptd` = mdy_hms(`Date Rptd`), 
                 `DATE OCC` = mdy_hms(`DATE OCC`))]
new_data[, delay_days := as.numeric(difftime(`Date Rptd`, `DATE OCC`, units = "days"))]
delay_days_model <- lm(delay_days ~ `Crm Cd Desc` + hour + `Vict Sex` + `Weapon Used Cd` + juv_crime, data = new_data)
summary(delay_days_model)