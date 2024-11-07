install.packages("tidycensus")
install.packages("sf")
install.packages("tigris")

library(tidycensus);library(tidyverse)
census_api_key("063a351b77e62241183d588c1af4109e63b2def2", install = TRUE)

year <- 2024
variables <- c(
  total_population = "B01003_001",
  median_age = "B01002_001",
  median_income = "B19013_001"
) # total population

state_data <- get_acs(
  geography = "state",
  variables = variables,
  year = year,
  geometry = TRUE
)

state_data_wide <- state_data %>%
  select(-moe) %>%
  spread(key = variable, value = estimate)

ggplot(state_data_wide, aes(x = total_population, y = median_income)) +
  geom_point() +
  geom_text(aes(label = NAME), hjust = 1.1, vjust = 1.1, size = 3) +
  labs(
    title = paste(year, "년 주별 총인구와 중위 소득 간의 관계"),
    x = "총인구",
    y = "중위 소득 (USD)"
  ) +
  theme_minimal()

