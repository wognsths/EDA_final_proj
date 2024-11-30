library(tidyverse); library(stringr);library(sf); library(tidycensus); library(httr); library(jsonlite)

source("data_prep.R")

Geo.CD <- GeoData_Loader()
Mosaic.CD <- MosaicData_Loader()
crime_groups_choices <- Geo.CD$Crm.Cd.Group %>% unique() %>% sort()
AREA_NAME <- Mosaic.CD$`AREA NAME` %>% unique()

CD %>% head()
Geo.CD %>% head()
Mosaic.CD %>% head()
crime_groups_choices
AREA_NAME
zipcodes_final

CD %>% select(Crm.Cd.Group) %>% unique()
cd_pie <- cd_pie <- CD %>% group_by(Crm.Cd.Group) %>% count()

# looks at the overall crime
pie_colors <- c("red", "pink", "green", "yellow", "purple", "orange", 
                "blue","salmon", "cyan", "brown", "darkgreen")

ggplot(cd_pie, aes(x = "", y = n, fill = Crm.Cd.Group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "What is the usual crime") +
  scale_fill_manual(values = pie_colors) +
  theme_void() +
  theme(
    legend.title = element_text(size = 7), # Adjust legend title size
    legend.text = element_text(size = 5),  # Adjust legend text size
    legend.key.size = unit(0.5, "cm")      # Adjust legend key size
  )
