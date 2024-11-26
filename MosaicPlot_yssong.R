library(tidyverse);library(data.table);library(ggplot2);library(ggmosaic)
data <- fread("Crime_Data_2023.csv")
data[, `:=` (Severity = factor(`Part 1-2`, levels = c(1,2), labels = c("Severe", "Less Severe")),
             weapon_usage = ifelse(is.na(`Weapon Used Cd`), "No", "Yes"),
             crime_status = ifelse(`Status Desc` %like% "Juv", "Juvenile", 
                                ifelse(`Status Desc` %like% "Adult", "Adult", NA)),
             vict_sex = ifelse(((`Vict Sex` == "-")|(`Vict Sex` == "X")|(`Vict Sex` == "")), NA, `Vict Sex`))]

vict_sex_data <- data[!is.na(vict_sex)] %>% .[, vict_sex := as.factor(vict_sex)]
weapon_usage_data <- data[, weapon_usage := as.factor(weapon_usage)]
crime_status_data <- data[!is.na(crime_status)] %>% .[, crime_status := as.factor(crime_status)]

ggplot(data = vict_sex_data) +
  geom_mosaic(aes(x = product(Severity, vict_sex), 
                  fill = Severity)) +
  scale_fill_manual(values = c("turquoise3", "orange2")) +
  labs(x = "Victim Sex", 
       y = "Severity", 
       title = "Mosaic Plot of Victim Sex vs Crime Severity") +
  theme_mosaic()

ggplot(data = weapon_usage_data) +
  geom_mosaic(aes(x = product(Severity, weapon_usage), 
                  fill = Severity)) +
  scale_fill_manual(values = c("turquoise3", "orange2")) +
  labs(x = "Weapon Usage", 
       y = "Severity", 
       title = "Mosaic Plot of Weapon Usage vs Crime Severity") +
  theme_mosaic()

ggplot(data = crime_status_data) +
  geom_mosaic(aes(x = product(Severity, crime_status), 
                  fill = Severity)) +
  scale_fill_manual(values = c("turquoise3", "orange2")) +
  labs(x = "Weapon Usage", 
       y = "Crime Status", 
       title = "Mosaic Plot of Crime Status vs Crime Severity") +
  theme_mosaic()
