library(dplyr)
library(tidyr)
library(caret)
library(xgboost)
library(randomForest)


multi_logit_model <- readRDS("~/EDA_final_proj/EDA_Shinyapps_Final/multi_logit_model.rds")
xgb_model <- readRDS("~/EDA_final_proj/EDA_Shinyapps_Final/xgb_model.rds")
rf_model <- readRDS("~/EDA_final_proj/EDA_Shinyapps_Final/rf_model.rds")
CD <- readRDS("~/EDA_final_proj/EDA_Shinyapps_Final/CD.rds")

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


# CD 내 Crm.Cd.Group를 재코딩
CD <- CD %>%
  mutate(Crm.Cd.Group = recode(Crm.Cd.Group, !!!mapping))


train_data <- CD %>%
  dplyr::select(Crm.Cd.Group, `AREA NAME`, vict_age, vict_sex, vict_descent, period) %>%
  dplyr::rename(AREA_NAME = `AREA NAME`) %>%
  dplyr::mutate(across(c(Crm.Cd.Group, AREA_NAME, vict_age, vict_sex, vict_descent, period), factor))


area_levels <- levels(train_data$AREA_NAME)
age_levels <- levels(train_data$vict_age)
sex_levels <- levels(train_data$vict_sex)
descent_levels <- levels(train_data$vict_descent)
period_levels <- levels(train_data$period)
crime_levels <- levels(train_data$Crm.Cd.Group)

all_combinations <- expand.grid(
  AREA_NAME = area_levels,
  vict_age = age_levels,
  vict_sex = sex_levels,
  vict_descent = descent_levels,
  period = period_levels,
  stringsAsFactors = FALSE
)

all_combinations$AREA_NAME <- factor(all_combinations$AREA_NAME, levels = area_levels)
all_combinations$vict_age <- factor(all_combinations$vict_age, levels = age_levels)
all_combinations$vict_sex <- factor(all_combinations$vict_sex, levels = sex_levels)
all_combinations$vict_descent <- factor(all_combinations$vict_descent, levels = descent_levels)
all_combinations$period <- factor(all_combinations$period, levels = period_levels)

dmy <- dummyVars("~ AREA_NAME + vict_age + vict_sex + vict_descent + period", data = train_data)
all_x <- data.frame(predict(dmy, newdata = all_combinations))
dall <- xgb.DMatrix(data = as.matrix(all_x))

prob_logit <- predict(multi_logit_model, newdata = all_combinations, type = "prob")
prob_rf <- predict(rf_model, newdata = all_combinations, type = "prob")
prob_xgb <- predict(xgb_model, dall)

prob_xgb_matrix <- matrix(prob_xgb, nrow = nrow(all_combinations), byrow = TRUE)
colnames(prob_xgb_matrix) <- crime_levels

prob_xgb_df <- as.data.frame(prob_xgb_matrix)

final_df <- all_combinations %>%
  bind_cols(
    as.data.frame(prob_logit) %>% rename_with(~paste0("Logit_", .)),
    as.data.frame(prob_rf) %>% rename_with(~paste0("RF_", .)),
    prob_xgb_df %>% rename_with(~paste0("XGB_", .))
  )

colnames(final_df) <- gsub("AGG.ASSAULTS", "ASSAULT", colnames(final_df))
colnames(final_df) <- gsub("BURG.THEFT.FROMVEICHLE", "BURGLARY_THEFT_FROM_VEICHLE", colnames(final_df))
colnames(final_df) <- gsub("OTHER.THEFT", "OTHER_THEFT", colnames(final_df))
colnames(final_df) <- gsub("PART2 Crime", "LESS_SEVERE_CRIME", colnames(final_df))
colnames(final_df) <- gsub("PERSONAL.THEFT", "PERSONAL_THEFT", colnames(final_df))
colnames(final_df) <- gsub("VEICHLE.THEFT", "VEHICLE_THEFT", colnames(final_df))


saveRDS(final_df, "~/EDA_final_proj/EDA_Shinyapps_Final/all_predictions.rds")
