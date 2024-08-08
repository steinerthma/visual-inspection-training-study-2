library(tidyverse)
if (!("wmisc" %in% installed.packages())) devtools::install_github("jazznbass/wmisc")
if (!packageDate("wmisc") >= as.Date("2024-06-04")) devtools::install_github("jazznbass/wmisc")
library(wmisc)

dat_items <- readRDS(file.path("inspection_data_valid.RDS"))
dat_subjects <- readRDS(file.path("user_data_valid.RDS"))

dat_items <- dat_items %>%
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
      labels = c("None", "Trend", "Slope", "Trend & Slope")),
    correct = as.numeric(correct)
  ) %>%
  arrange(id_subject,my_time)

dat_subjects <- dat_subjects %>%
  mutate(
    group = factor(
      gruppe,
      levels = c(0, 1),
      labels = c("Control", "Training")
    ),
    #mathe = factor(mathe, levels = c("ja", "nein"), labels = c("yes", "no")),
    id_subject = factor(sessionToken),
    #erfahrung_lvd = case_match(erfahrung_lvd, 0 ~ NA, .default = erfahrung_lvd),
    #erfahrung_lehre = case_match(erfahrung_lehre, 0 ~ NA, .default = erfahrung_lehre),
    #erfahrung_error = case_match(erfahrung_error, 0 ~ NA, .default = erfahrung_error),
    #Q1cc = case_match(Q1cc, -99 ~ NA, .default = Q1cc),
    #Q2cc = case_match(Q2cc, -99 ~ NA, .default = Q2cc),
    #Q3cc = case_match(Q3cc, -99 ~ NA, .default = Q3cc),
    fach = case_match(
      fach,
      "LA (SP/IP)" ~ "Special education",
      c("LA", "GrundschulpÃ¤dagogik") ~ "Primary education",
      .default = fach
    ),
    uni = case_match(
      uni,
      "Universität Potsdam" ~ "University of Potsdam",
      .default = "Other University"
    )
  )
dat_items <- add_label(dat_items, list(
  group = "Group condition",
  effect = "Effect condition",
  time = "Time"
))

dat_subjects <- wmisc::add_label(dat_subjects, list(
  group = "Group condition",
  uni = "University",
  #mathe = "Studying math",
  gesch = "Sex",
  erfahrung_lvd = "Experience with progress monitoring",
  erfahrung_lehre = "Attended class on progress monitoring",
  #Q1cc = "Followed instructions",
  #Q2cc = "Followed instructions at pretest",
  #Q3cc = "Followed instructions at posttest",
  age = "Age",
  fach = "Study subject",
  semester = "Semester"
))

saveRDS(dat_items, file.path("data", "data-items-clean.rds"))
saveRDS(dat_subjects, file.path("data", "data-subjects-clean.rds"))
