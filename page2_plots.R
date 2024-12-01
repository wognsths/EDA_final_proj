source("~/EDA_final_proj/data_prep.R")
Geo.CD <- GeoData_Loader()

#Filled Density Plots -> 범죄 심각도가 경찰서 거리/피해자 성별/범죄 시간대에 따라 크게 차이나진 않음ㅜ 
## Severity vs. Distance to Police Station 
ggplot(Geo.CD, aes(x = L1_dist, fill = Severity))+
  geom_density(position = "fill")
ggplot(Geo.CD, aes(x = Severity, y = L1_dist, color = Severity))+
  geom_violin()+coord_flip()

## Severity vs. Vict Age
CD.age <- Geo.CD %>% filter(`Vict Age` != 0)
ggplot(CD.age, aes(x = `Vict Age`, fill = Severity))+
  geom_density(position = "fill")
ggplot(CD.age, aes(x = Severity, y = `Vict Age`, color = Severity))+
  geom_violin()+coord_flip()

## Severity vs. Hour
ggplot(Geo.CD, aes(x = hour, fill = Severity))+
  geom_density(position = "fill")
ggplot(Geo.CD, aes(x = Severity, y = hour, color = Severity))+
  geom_violin()+coord_flip()


#Relationship between L1_dist, Dur Rptd, Severity 
### 분포 비교해봤을 때 범죄 심각도에 큰 차이가 없다,, 
ggplot(Geo.CD, aes(x = L1_dist, y = `Dur Rptd`, color = Severity))+
  geom_density2d()

### Dur Rptd가 0인 값이 많아서 시각화 효과적X
ggplot(Geo.CD, aes(x = L1_dist, y = `Dur Rptd`, color = Severity)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Severity)) +
  labs(title = "Relationship Between Distance and Reporting Time",
       x = "Distance to Police Station (L1_dist)",
       y = "Duration to Report (Dur Rptd)") +
  theme_minimal()

#Scatterplot matrix 
library(GGally)
CD.sub <- Geo.CD %>% select(c(`Dur Rptd`, Severity, L1_dist)) %>% mutate(Severity = as.numeric(Severity))
ggpairs(CD.sub)


#Mosiac Plots (Vict Age, Vict Descent)
mosiac.CD <- Geo.CD %>%
  mutate(
    vict_age = case_when(
      `Vict Age` == -1 ~ NA, 
      (`Vict Age` == 0) & (is.na(vict_sex)) ~ NA, 
      `Vict Age` < 15 ~ "Children and Young Adolescents (0~14)",
      `Vict Age` > 64 ~ "Elderly (65~)", 
      TRUE ~ "Working-Age Population (15~64)"
    ),
    vict_descent = case_when(
      `Vict Descent` %in% c("A", "C", "D", "F", "J", "K", "L", "V", "Z") ~ "Asian",
      `Vict Descent` == "B" ~ "Black",
      `Vict Descent` == "H" ~ "Hispanic",
      `Vict Descent` == "W" ~ "White",
      `Vict Descent` %in% c("G", "P", "S", "U") ~ "Pacific Islander",
      `Vict Descent` == "I" ~ "Native American",
      `Vict Descent` == "O" ~ "Other",
      TRUE ~ NA 
    )
  )

##vict age group 
vict_age_data <- mosiac.CD %>%
  filter(!is.na(vict_age)) %>% 
  mutate(vict_age = as.factor(vict_age))

ggplot(data = vict_age_data) +
  geom_mosaic(aes(x = product(Severity, vict_age), 
                  fill = Severity)) +
  scale_fill_manual(values = c("turquoise3", "orange2")) +
  labs(x = "Victim Age Group", 
       y = "Severity", 
       title = "Mosaic Plot of Victim Age Group vs Crime Severity") +
  theme_mosaic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

##vict descent
vict_descent_data <- mosiac.CD %>%
  filter(!is.na(vict_descent)) %>% 
  mutate(vict_descent = as.factor(vict_descent))

ggplot(data = vict_descent_data) +
  geom_mosaic(aes(x = product(Severity, vict_descent), 
                  fill = Severity)) +
  scale_fill_manual(values = c("turquoise3", "orange2")) +
  labs(x = "Victim Descent", 
       y = "Severity", 
       title = "Mosaic Plot of Victim Descent vs Crime Severity") +
  theme_mosaic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

##Vict Descent vs. Dur Rptd
### outlier 너무 많아서 비교 어려움 
ggplot(data = vict_descent_data, aes(x = vict_descent, y = `Dur Rptd`)) +
  geom_boxplot(fill = "skyblue", color = "black", outlier.color = "red") +
  labs(
    x = "Victim Descent Group",
    y = "Duration Reported (Days)",
    title = "Comparison of Report Duration by Victim Descent"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
ggplot(vict_descent_data, aes(x = vict_descent, y = `Dur Rptd`))+
  geom_violin()+
  coord_flip()

###ecdf
ggplot(vict_descent_data, aes(x = `Dur Rptd`, color = vict_descent)) +
  stat_ecdf(geom = "step") +
  labs(
    title = "Vict Descent별 ECDF Plot",
    x = "Duration Reported",
    y = "ECDF"
  ) +
  theme_minimal()
