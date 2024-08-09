library(tidyverse)

user_data <- read.csv("user_data.csv")
inspection_data <- read.csv("inspection_data.csv", na.strings = c("NA", "-99"))
compliance_data <- read.csv("compliance_data.csv", na.strings = c("NA", "-99"))

user_valid <- user_data %>%
  filter(QID != 95 & QID != 1 & QID != 2 & QID != 112)

valid_ids <- user_valid %>%
  filter(sessionToken %in% compliance_data[["sessionToken"]]) %>%
  select(sessionToken) %>%
  unlist()

user_valid <- user_valid %>%
  filter(sessionToken %in% valid_ids)

inspection_valid <- inspection_data %>%
  filter(sessionToken %in% valid_ids)

compliance_valid <- compliance_data %>%
  filter(sessionToken %in% valid_ids)

table(user_valid["gruppe"])



dat_items <- inspection_valid %>%
  mutate(
    group = factor(
      gruppe, levels = c(0, 1), 
      labels = c("Control", "Training")
    ),
    id_subject = sessionToken,
    time = factor(
      run, levels = c("pre", "post"),
      labels = c("Pre", "Post")
    ),
    item_effect = paste0(trend, slope),
    effect = factor(
      item_effect, 
      levels = c("00", "10", "01", "11"),
      labels = c("None", "Trend", "Slope", "Trend & Slope"))
  ) %>%
  arrange(id_subject)

dat_items %>%
  filter(question == "effect") %>%
  group_by(group, effect, time) %>%
  summarise(
    mean_true = round(mean(response, na.rm = TRUE), 2)
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = "time", values_from = "mean_true") %>%
  mutate(
    "Difference" = Post - Pre
  ) %>%
  relocate(group, effect)

## Save .csv
write.csv(user_valid, file = "user_data_valid.csv")
write.csv(inspection_valid, file = "inspection_data_valid.csv")
write.csv(compliance_valid, file = "compliance_data_valid.csv")

## Save .RData
saveRDS(user_valid, file = "user_data_valid.RDS")
saveRDS(inspection_valid, file = "inspection_data_valid.RDS")
saveRDS(compliance_valid, file = "compliance_data_valid.RDS")
