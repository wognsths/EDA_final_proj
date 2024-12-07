library(tidyverse);library(MASS);library(arm);library(VGAM);library(ggplot2)

CD <- CD %>% mutate(crime_group = factor(Crm.Cd.Group)) %>% 
  mutate(period = case_when(
    hour >= 6 & hour < 12 ~ "morning",
    hour >= 12 & hour < 18 ~ "afternoon",
    hour >= 18 & hour < 24 ~ "night",
    (hour >= 0 & hour < 6) | hour == 24 ~ "late_night"
  ))
#CD.polr <- polr(crime_group ~ hour, data = CD)
#display(CD.polr)

crime_wide <- CD %>%
  group_by(period, vict_age, vict_sex, Crm.Cd.Group) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = Crm.Cd.Group, 
    values_from = count,      
    values_fill = list(count = 0))
CD.mlogit <- vglm(cbind(AGG.ASSAULTS, BURG.THEFT.FROMVEICHLE, BURGLARY, OTHER.THEFT, 
                        `PART2 Crime`, PERSONAL.THEFT, RAPE, ROBBERY, VEICHLE.THEFT, HOMICIDE)~ period + vict_age + vict_sex, 
                  family = multinomial, data = crime_wide)
CD.mlogit.df <- data.frame(model.frame(CD.mlogit), fitted.values(CD.mlogit))
CD.mlogit.long <- CD.mlogit.df %>% 
  pivot_longer(AGG.ASSAULTS:HOMICIDE, names_to = "crime group", values_to = "probability")

ggplot(CD.mlogit.long, aes(x = `crime group`, y = probability))+
  geom_point()+
  facet_wrap(~period + vict_age + vict_sex, ncol = 10)+
  theme(axis.text.x = element_text(angle = 90))

ggplot(CD.mlogit.long, aes(x = `crime group`, y = probability, col = vict_age))+
  geom_point()+
  facet_wrap(~period + vict_sex, ncol = 4)+
  theme(axis.text.x = element_text(angle = 90))
