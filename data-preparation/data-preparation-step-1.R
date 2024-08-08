library(dplyr)
library(tidyr)
library(tidyverse)

### Get list of data files, select the newest one and load it's data --------------------

## Set data path
data_path <- file.path(".", "Daten")

## Create a tibble with a list of available files
files <- data.frame(file_dir = list.files(path = data_path, pattern = "\\Datensicherung.RData$", full.names=TRUE))

## Create new columns with separate date for each file
files <- files %>%
  mutate(
    file_name = gsub(paste0(data_path, .Platform$file.sep), '', file_dir),
    file_date = as.difftime(gsub("_Datensicherung.RData", "", file_name), format = "%Y%m%d_%H%M%S")
  )

## Find newest file
current_file <- files %>%
  filter(file_date == max(file_date))

## Load data of newest file
load(file = current_file$file_dir)

### Tidy data ------------------
## user_data contains data from initial questionnaires (1 row = 1 participant)
## compliance_data contains data from the compliance questionnaire (1 row = 1 participant)
## inspection_data contains data from visual inspection trials (1 row = 1 trial)

## full_data contains information like rt, type of jspsych plugin, etc. for every page of the online experiment
## information like time spent on each video and replies for the intervention tasks can be found here

## sessionToken is the identifier variable that can be used to combine datasets

## Define cases where performance data should be replaced from full_data
replace_data_list <- c("YkGo0iiMwpRC", "EMcJ0ptkTShY")

## Create new variable with inspection_data for above cases from full_data
replace_data <- full_data %>%
  filter(sessionToken %in% replace_data_list & test_part == "graphreading") %>%
  mutate(correct = if_else(question == "certainty", NA, if_else(response == slope, 1, 0))) %>%
  select(names(inspection_data))

## Delete cases defined above and replace inspection_data rows from full_data
inspection_data <- inspection_data %>%
  filter(!(sessionToken %in% replace_data_list)) %>%
  bind_rows(replace_data)

## Count visual inspection trials per run
inspection_trials <- inspection_data %>%
  count(sessionToken, run, question) %>%
  pivot_wider(names_from = c(run, question), values_from = n, names_prefix = "n_trials_")

## Add compliance questions and n of visual inspection trials
user_data <- user_data %>%
  left_join(compliance_data, by = "sessionToken") %>%
  select(!QID.y & !QID.x) %>%
  left_join(inspection_trials, by = "sessionToken")



## Display tables for n per trial
table(inspection_trials[,c("n_trials_post_certainty", "n_trials_pre_certainty")], useNA = "always")
table(inspection_trials[,c("n_trials_post_effect", "n_trials_pre_effect")], useNA = "always")

## Create a new dataset for valid participants and remove participants with less than 40 trials in either run
user_valid <- user_data %>%
  filter(n_trials_pre_effect  == 40 & n_trials_pre_certainty == 40 & n_trials_post_effect == 40 & n_trials_post_certainty == 40 )


##run tidy_data

### delete all cases in inspection_data, which are not in user_valid
##apply semi_join dplyr function, new variable -> inspection_valid

inspection_valid <- semi_join(inspection_data, user_valid, by = "sessionToken")


###filter different cases in new datasets (z.B. [cpre_zz]); Name including informations of Data:
#question (effect[e]/certainty[c]);
#run [pre] / [post]);
#Slope (0[z] / 1[o]);
#Trend (0[z] / 1[o])


##### CERTAINTY#####
##1/16: certainty, pre, Slope 0; Trend 0;
#create cpre_zz
cpre_zz <- filter(inspection_valid, question == "certainty",
                  slope == 0,
                  trend == 0,
                  run == "pre")

#create mean for cpre_zz and add it to user_valid
cpre_zz <- cpre_zz %>%
  group_by(sessionToken) %>%
  summarise(mean = mean(response))
user_valid <- left_join(user_valid,cpre_zz, by = "sessionToken")

#rename „mean“ to mcpre_zz (meancpre_zz)
user_valid <- rename(user_valid, mcpre_zz = mean)

#delete cpre_zz Dataset
rm(cpre_zz)

##2/16: certainty, pre, Slope 1; Trend 0;
#create cpre_oz
cpre_oz <- filter(inspection_valid, question == "certainty",
                  slope == 1,
                  trend == 0,
                  run == "pre")

#create mean for cpre_oz and add it to user_valid
cpre_oz <- cpre_oz %>%
  group_by(sessionToken) %>%
  summarise(mean = mean(response))
user_valid <- left_join(user_valid,cpre_oz, by = "sessionToken")

#rename „mean“ to mcpre_oz (meancpre_oz)
user_valid <- rename(user_valid, mcpre_oz = mean)

#delete cpre_oz Dataset
rm(cpre_oz)

##3/16: certainty, pre, Slope 0; Trend 1;
#create cpre_zo
cpre_zo <- filter(inspection_valid, question == "certainty",
                  slope == 0,
                  trend == 1,
                  run == "pre")

#create mean for cpre_zo and add it to user_valid
cpre_zo <- cpre_zo %>%
  group_by(sessionToken) %>%
  summarise(mean = mean(response))
user_valid <- left_join(user_valid,cpre_zo, by = "sessionToken")

#rename „mean“ to mcpre_oz (meancpre_zo)
user_valid <- rename(user_valid, mcpre_zo = mean)

#delete cpre_zo Dataset
rm(cpre_zo)

##4/16: certainty, pre, Slope 1; Trend 1;
#create cpre_oo
cpre_oo <- filter(inspection_valid, question == "certainty",
                  slope == 1,
                  trend == 1,
                  run == "pre")

#create mean for cpre_oo and add it to user_valid
cpre_oo <- cpre_oo %>%
  group_by(sessionToken) %>%
  summarise(mean = mean(response))
user_valid <- left_join(user_valid,cpre_oo, by = "sessionToken")

#rename „mean“ to mcpre_oo (meancpre_oo)
user_valid <- rename(user_valid, mcpre_oo = mean)

#delete cpre_oo Dataset
rm(cpre_oo)

##5/16: certainty, post, Slope 0; Trend 0;
#create cpost_zz
cpost_zz <- filter(inspection_valid, question == "certainty",
                   slope == 0,
                   trend == 0,
                   run == "post")

#create mean for cpost_zz and add it to user_valid
cpost_zz <- cpost_zz %>%
  group_by(sessionToken) %>%
  summarise(mean = mean(response))
user_valid <- left_join(user_valid,cpost_zz, by = "sessionToken")

#rename „mean“ to mcpost_zz (meancpost_zz)
user_valid <- rename(user_valid, mcpost_zz = mean)

#delete cpost_zz Dataset
rm(cpost_zz)


##6/16: certainty, post, Slope 1; Trend 0;
#create cpost_oz
cpost_oz <- filter(inspection_valid, question == "certainty",
                   slope == 1,
                   trend == 0,
                   run == "post")


#create mean for cpost_oz and add it to user_valid
cpost_oz <- cpost_oz %>%
  group_by(sessionToken) %>%
  summarise(mean = mean(response))
user_valid <- left_join(user_valid,cpost_oz, by = "sessionToken")

#rename „mean“ to mcpost_oz (meancpost_oz)
user_valid <- rename(user_valid, mcpost_oz = mean)

#delete cpost_oz Dataset
rm(cpost_oz)

##7/16: certainty, post, Slope 0; Trend 1;
#create cpost_zo
cpost_zo <- filter(inspection_valid, question == "certainty",
                   slope == 0,
                   trend == 1,
                   run == "post")

#create mean for cpost_zo and add it to user_valid
cpost_zo <- cpost_zo %>%
  group_by(sessionToken) %>%
  summarise(mean = mean(response))
user_valid <- left_join(user_valid,cpost_zo, by = "sessionToken")

#rename „mean“ to mcpost_oz (meancpost_zo)
user_valid <- rename(user_valid, mcpost_zo = mean)

#delete cpost_zo Dataset
rm(cpost_zo)

##8/16: certainty, post, Slope 1; Trend 1;
#create cpost_oo
cpost_oo <- filter(inspection_valid, question == "certainty",
                   slope == 1,
                   trend == 1,
                   run == "post")

#create mean for cpost_oo and add it to user_valid
cpost_oo <- cpost_oo %>%
  group_by(sessionToken) %>%
  summarise(mean = mean(response))
user_valid <- left_join(user_valid,cpost_oo, by = "sessionToken")

#rename „mean“ to mcpost_oo (meancpost_oo)
user_valid <- rename(user_valid, mcpost_oo = mean)

#delete cpost_oo Dataset
rm(cpost_oo)

##### EFFECT #####
##9/16: effect, pre, Slope 0; Trend 0;
#create epre_zz
epre_zz <- filter(inspection_valid, question == "effect",
                  slope == 0,
                  trend == 0,
                  run == "pre")

#create mean for epre_zz and add it to user_valid
epre_zz <- epre_zz %>%
  group_by(sessionToken) %>%
  summarise(mean = mean(correct))
user_valid <- left_join(user_valid,epre_zz, by = "sessionToken")

#rename „mean“ to mepre_zz (meanepre_zz)
user_valid <- rename(user_valid, mepre_zz = mean)

#delete epre_zz Dataset
rm(epre_zz)

##10/16: effect, pre, Slope 1; Trend 0;
#create epre_oz
epre_oz <- filter(inspection_valid, question == "effect",
                  slope == 1,
                  trend == 0,
                  run == "pre")

#create mean for epre_oz and add it to user_valid
epre_oz <- epre_oz %>%
  group_by(sessionToken) %>%
  summarise(mean = mean(correct))
user_valid <- left_join(user_valid,epre_oz, by = "sessionToken")

#rename „mean“ to mepre_oz (meanepre_oz)
user_valid <- rename(user_valid, mepre_oz = mean)

#delete epre_oz Dataset
rm(epre_oz)

##11/16: effect, pre, Slope 0; Trend 1;
#create epre_zo
epre_zo <- filter(inspection_valid, question == "effect",
                  slope == 0,
                  trend == 1,
                  run == "pre")

#create mean for epre_zo and add it to user_valid
epre_zo <- epre_zo %>%
  group_by(sessionToken) %>%
  summarise(mean = mean(correct))
user_valid <- left_join(user_valid,epre_zo, by = "sessionToken")

#rename „mean“ to mepre_oz (meanepre_zo)
user_valid <- rename(user_valid, mepre_zo = mean)

#delete epre_zo Dataset
rm(epre_zo)

##12/16: effect, pre, Slope 1; Trend 1;
#create epre_oo
epre_oo <- filter(inspection_valid, question == "effect",
                  slope == 1,
                  trend == 1,
                  run == "pre")

#create mean for epre_oo and add it to user_valid
epre_oo <- epre_oo %>%
  group_by(sessionToken) %>%
  summarise(mean = mean(correct))
user_valid <- left_join(user_valid,epre_oo, by = "sessionToken")

#rename „mean“ to ecpre_oo (meanepre_oo)
user_valid <- rename(user_valid, mepre_oo = mean)

#delete epre_oo Dataset
rm(epre_oo)

##13/16: effect, post, Slope 0; Trend 0;
#create epost_zz
epost_zz <- filter(inspection_valid, question == "effect",
                   slope == 0,
                   trend == 0,
                   run == "post")

#create mean for epost_zz and add it to user_valid
epost_zz <- epost_zz %>%
  group_by(sessionToken) %>%
  summarise(mean = mean(correct))
user_valid <- left_join(user_valid,epost_zz, by = "sessionToken")

#rename „mean“ to mepost_zz (meanepost_zz)
user_valid <- rename(user_valid, mepost_zz = mean)

#delete epost_zz Dataset
rm(epost_zz)

##14/16: effect, post, Slope 1; Trend 0;
#create epost_oz
epost_oz <- filter(inspection_valid, question == "effect",
                   slope == 1,
                   trend == 0,
                   run == "post")

#create mean for epost_oz and add it to user_valid
epost_oz <- epost_oz %>%
  group_by(sessionToken) %>%
  summarise(mean = mean(correct))
user_valid <- left_join(user_valid,epost_oz, by = "sessionToken")

#rename „mean“ to mepost_oz (meanepost_oz)
user_valid <- rename(user_valid, mepost_oz = mean)

#delete epost_oz Dataset
rm(epost_oz)

##15/16: effect, post, Slope 0; Trend 1;
#create epost_zo
epost_zo <- filter(inspection_valid, question == "effect",
                   slope == 0,
                   trend == 1,
                   run == "post")

#create mean for epost_zo and add it to user_valid
epost_zo <- epost_zo %>%
  group_by(sessionToken) %>%
  summarise(mean = mean(correct))
user_valid <- left_join(user_valid,epost_zo, by = "sessionToken")

#rename „mean“ to mepost_oz (meanepost_zo)
user_valid <- rename(user_valid, mepost_zo = mean)

#delete epost_zo Dataset
rm(epost_zo)


##16/16: effect, post, Slope 1; Trend 1;
#create epost_oo
epost_oo <- filter(inspection_valid, question == "effect",
                   slope == 1,
                   trend == 1,
                   run == "post")

#create mean for epost_oo and add it to user_valid
epost_oo <- epost_oo %>%
  group_by(sessionToken) %>%
  summarise(mean = mean(correct))
user_valid <- left_join(user_valid,epost_oo, by = "sessionToken")

#rename „mean“ to mepost_oo (meanepost_oo)
user_valid <- rename(user_valid, mepost_oo = mean)

#delete epost_oo Dataset
rm(epost_oo)


##delete n_trials_post_certainty, n_trials_post_effect, n_trials_pre_certainty, n_trials_pre_effect
user_valid$n_trials_post_certainty = NULL
user_valid$n_trials_post_effect = NULL
user_valid$n_trials_pre_certainty = NULL
user_valid$n_trials_pre_effect = NULL

###rename values
##rename values in „uni“
user_valid$uni[user_valid$uni %in% c( "UniversitÃ¤t Potsdam ", "Uni Potsdam", "UniversitÃ¤t Potsdam", "Potsdam", "Potsdam UniversitÃ¤t",  "Potsdam ", "UniversitÃ¤r Potsdam")] <- "Universität Potsdam"
user_valid$uni[user_valid$uni %in% c( "UniversitÃ¤t Wien ", "UniversitÃ¤t Wien")] <- "Universität Wien"
user_valid$uni[user_valid$uni %in% c( "UniversitÃ¤t  Graz", "Karl-Franzens UniversitÃ¤"," Uni Graz", "Uni Graz")] <- "Universität Graz"
user_valid$uni[user_valid$uni %in% c( "UniversitÃ¤t OsnabrÃ¼ck ")] <- "Universität Osnabrück"
user_valid$uni[user_valid$uni %in% c( "")] <- "NA"

##duplicate "fach" twice
#rename one duplicate in "Matematikbezug [mathe]"
user_valid$"mathe" = user_valid$fach

#relocate "mathe" and "SP/IP"
user_valid <- user_valid %>%
  relocate("mathe", .after = "fach")


##rename values in „fach“
user_valid$fach[user_valid$fach %in% c( "Mathe, Sachunterricht", "Mathe, Kunst","Lehramt Deutsch/Sachunterricht", "Bachelor Eduction Mathe/Deutsch", "Deutsch und Sachunterricht ", "Mathematik", "Mathe und Englisch", "Deutsch,Sachunterricht", "Deutsch und Sport", "Deutsch und Sachunterrich", "Lehramt Deutsch/Sachunterricht", "Lehramt Sekundarstufe", "Deutsch/Englisch", "Deutsch, Sachunterricht ", "Deutsch und Sachunterricht", "Deutsch/ Kunst", "Sachunterricht, deutsch ", "Sachunterricht, deutsch ", "Mathematik und Sachunterr", "Deutsch & Sachunterricht ", "Mathe und Deutsch", "Mathematik, Sachunterricht", "Mathematik, Sachunterricht", "Lehramt Englisch & GWK", "Mathematik, Sachunterricht", "Lehramt Englisch & GWK", "Mathematik und Englisch", "Lehramt Englisch und GSPB", "Deutsch/Sachunterricht", "Lehramt", "Mathe", "Deutsch", "Mathematik / Sachunterricht", "Sport", "Betriebswirtschaft", "Englisch/Kunst","Mathematik und Deutsch", "Lehramt Primarstufe", "Lehramt Primarstufe Englisch/ Sachunterricht", "Lehramt fÃ¼r die Primarstufe, Mathe, Sachunterrich", "Grundschullehramt Mathe und Deutsch", "Lehramt Primarstufe Englisch und Kunst", "Lehramt Primarstufe Deutsch, Sachunterricht", "Lehramt Primarstufe Deutsch und Mathe", "Lehramt \tPrimarstufe", "Lehramt Primarstufe ", "Grundschullehramt", "Lehramt fÃ¼r die Primarst", "GrundschulpÃ¤dagogik - De", 	"Lehramt Primarstufe Deuts", "Lehramt Primarstufe mit S", "Grundschullehramt in den ", "Lehramt fÃ¼r die Primarstufe ", "Lehramt Primar", 	"Bachelor Lehramt Primarstufe", "Grundschullehramt (Mathe Sachunterricht)", "Lehramt Primarstufe Deutsch und Mathe", "Englisch, Sachunterricht (Lehramt Primarstufe)", "Lehramt Primarstufe Deutsch, Sachunterricht", "Lehramt Primarstufe Mathe Sachunterricht", "Lehramt Primarstufe Englisch und Kunst", "Mathematik und Sachunterricht in der Primarstufe", "Lehramt Primarstufe Mathe+Deutsch", "Lehramt  Primarstufe Englisch/ Sachunterricht", "PrimarstufenpÃ¤dagogik", "Lehramt fÃ¼r die Primarstufe, Mathe, Sachunterrich", "GrundschulpÃ¤dagogik, Deutsch und Sachunterricht", "Primarstufe", "Primarstufe ")] <- "LA"
user_valid$fach[user_valid$fach %in% c( "FÃ¶derpÃ¤dagogig Sek I mit Sport", "Lehramt Inklusions-/\tSonderpädagogik", "FÃ¶rderpÃ¤dagogik Deutsch", "FÃ¶derpÃ¤dagogig Sek I mit Sport", "InklusionspÃ¤dagogik ", "Inklusionswissenschaft", "Englisch FÃ¶rderpÃ¤dagogi", "InklusionspÃ¤dagogik", "Inklusive PÃ¤dagogik; Ern", "Lehramt Inklusions-/Sonderpädagogik", "FÃ¶rderpÃ¤dagogik", "FÃ¶rderpÃ¤dagogik auf Leh", "InklusionspÃ¤dagogik", "FÃ¶rderpÃ¤dagogik 	Deutsch", "FÃ¶rderpÃ¤dagogik Englisch", "InklusionspÃ¤dagogik Primarstufe","FÃ¶rderpÃ¤dagogik Sportwissenschaft", "FÃ¶derpÃ¤dagogig Sek I mit Sport", "Lehramt fÃ¼r die Primarstufe mit Schwerpunkt Inklu", "Englisch FÃ¶rderpÃ¤dagogik", "FÃ¶rderpÃ¤dagogik Mathematik ", "Patholinguistik", "Patholinguisitik" )] <- "LA (SP/IP)"
user_valid$fach[user_valid$fach %in% c("")] <- "NA"

##rename values in „mathe“
user_valid$mathe[user_valid$mathe %in% c( "Mathe und Englisch", "Mathematik", "Mathematik / Sachunterricht", "Bachelor Eduction Mathe/Deutsch", "Mathe", "Mathematik und Englisch", "Mathematik, Sachunterricht", "Mathematik und Sachunterr", "Mathematik und Deutsch", "Mathe und Deutsch", "Mathe, Kunst", "Mathe, Sachunterricht", "Lehramt fÃ¼r die Primarstufe, Mathe, Sachunterrich", "Grundschullehramt Mathe und Deutsch", "Lehramt Primarstufe Deutsch und Mathe", "Grundschullehramt (Mathe Sachunterricht)", "Lehramt Primarstufe Deutsch und Mathe", "Lehramt Primarstufe Mathe Sachunterricht", "Mathematik und Sachunterricht in der Primarstufe", "Lehramt Primarstufe Mathe+Deutsch", "Lehramt fÃ¼r die Primarstufe, Mathe, Sachunterrich", "FÃ¶rderpÃ¤dagogik Mathematik ")] <- "ja"
user_valid$mathe[user_valid$mathe %in% c( "Lehramt Sekundarstufe", "Patholinguisitik", "GrundschulpÃ¤dagogik, Deutsch und Sachunterricht", "Deutsch und Sachunterricht ", "Deutsch/Englisch", "Lehramt Primarstufe Englisch/ Sachunterricht", "Deutsch, Sachunterricht ", "Deutsch", "Lehramt", "Sport", "Deutsch/ Kunst", "Deutsch/Sachunterricht", "Deutsch,Sachunterricht", "Lehramt Englisch und GSPB", "Sachunterricht, deutsch ", "Deutsch und Sport", "Lehramt Englisch & GWK", "Lehramt Deutsch/Sachunterricht", "Betriebswirtschaft", "Deutsch und Sachunterrich", "Englisch/Kunst", "Deutsch & Sachunterricht ", "Lehramt Primarstufe", "FÃ¶derpÃ¤dagogig Sek I mit Sport", "Lehramt Inklusions-/tSonderpädagogik", "FÃ¶rderpÃ¤dagogik Deutsch", "FÃ¶derpÃ¤dagogig Sek I mit Sport", "InklusionspÃ¤dagogik ", "Inklusionswissenschaft", "Englisch FÃ¶rderpÃ¤dagogi", "InklusionspÃ¤dagogik", "Inklusive PÃ¤dagogik; Ern", "Lehramt Inklusions-/Sonderpädagogik", "FÃ¶rderpÃ¤dagogik", "FÃ¶rderpÃ¤dagogik auf Leh", "InklusionspÃ¤dagogik", "FÃ¶rderpÃ¤dagogik 	Deutsch", "FÃ¶rderpÃ¤dagogik Englisch", "GrundschulpÃ¤dagogik", "InklusionspÃ¤dagogik Primarstufe", "FÃ¶rderpÃ¤dagogik Sportwissenschaft", "FÃ¶derpÃ¤dagogig Sek I mit Sport", "Lehramt fÃ¼r die Primarstufe mit Schwerpunkt Inklu", "Englisch FÃ¶rderpÃ¤dagogik", "Patholinguistik", "Patholinguisitik“, “Lehramt Primarstufe Englisch/ Sachunterricht", "Lehramt Primarstufe Englisch und Kunst", "Lehramt Primarstufe Deutsch, Sachunterricht", "Lehramt \tPrimarstufe", "Lehramt Primarstufe ", "Grundschullehramt", "Lehramt fÃ¼r die Primarst", "GrundschulpÃ¤dagogik - De", "Lehramt Primarstufe Deuts", "Lehramt Primarstufe mit S", "Grundschullehramt in den ", "Lehramt fÃ¼r die Primarstufe ", "Lehramt Primar", "Bachelor Lehramt Primarstufe", "Englisch, Sachunterricht (Lehramt Primarstufe)", "Lehramt Primarstufe Deutsch, Sachunterricht", "Lehramt Primarstufe Englisch und Kunst", "Lehramt  Primarstufe Englisch/ Sachunterricht", "PrimarstufenpÃ¤dagogik", "Deutsch und Sachunterricht", "Primarstufe", "Primarstufe ")] <- "nein"
user_valid$mathe[user_valid$mathe %in% c("")] <- "NA"

## save data
#saveRDS(inspection_valid, file.path("Daten", "data-items-raw.rds"))
#saveRDS(user_valid, file.path("Daten", "data-subjects-raw.rds"))
