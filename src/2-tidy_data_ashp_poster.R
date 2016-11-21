
library(tidyverse)
library(readxl)

met_file <- "data/raw/data_metoprolol_ashp_poster.xlsm"
dilt_file <- "data/raw/data_diltiazem_ashp_poster.xlsm"

# delete columns ES:FZ reading in as NULL
# delete rows 102:150
met_main <- read_excel(met_file, sheet = "Main Data Collection") %>%
    mutate(med = "metoprolol") %>%
    dmap_at("mL/kg of IVF Given in EC", as.character)

# delete row 2:101; then 102:150
dilt_main <- read_excel(dilt_file, sheet = "Main Data Collection") %>%
    mutate(med = "diltiazem")

data_main <- bind_rows(met_main, dilt_main) %>%
    rename(study_id = `Study ID`)

col_nm <- c("study_id", "yes_no", "dose_datetime", "dose", "dose_kg", "appropriate", "last_bolus_datetime")
col_type <- c("numeric", "text", "date", "numeric", "text", "text", "date")
# delete columns H:Z reading in as null
met_doses <- read_excel(met_file, sheet = "Initial Agent Boluses - Met", skip = 1, col_names = col_nm, col_types = col_type, na = "N/A") %>%
    mutate(med = "metoprolol")

# delete columns H:Z reading in as null
# fix date on C185
dilt_doses <- read_excel(dilt_file, sheet = "Initial Agent Boluses- Dilt", skip = 1, col_names = col_nm, col_types = col_type) %>%
    mutate(med = "diltiazem") %>%
    filter(!is.na(dose_datetime))

data_doses <- bind_rows(met_doses, dilt_doses)

col_nm <- c("study_id", "vital_datetime", "sbp", "dbp", "hr", "spo2")
col_type <- c("numeric", "date", rep("numeric", 4))

# delete columns H:Z reading in as null
dilt_vitals <- read_excel(dilt_file, sheet = "Vital Signs - Dilt", skip = 1, col_names = col_nm, col_types = col_type, na = "N/A") %>%
    mutate(med = "diltiazem")

# delete columns H:Z reading in as null
# fixed dates on lines 455, 733, 992, 1063, 1066
met_vitals <- read_excel(met_file, sheet = "Vital Signs - Met", skip = 1, col_names = col_nm, col_types = col_type, na = "none") %>%
    mutate(med = "metoprolol")

data_vitals <- bind_rows(met_vitals, dilt_vitals)

options(scipen = 999)
goal_hr <- data_vitals %>%
    filter(!is.na(hr)) %>%
    group_by(study_id) %>%
    arrange(study_id, vital_datetime) %>%
    mutate(interval = difftime(lead(vital_datetime), vital_datetime, units = "hours"),
           interval = coalesce(interval, 0),
           duration = difftime(vital_datetime, first(vital_datetime), units = "hours"),
           at_goal = hr < 110) %>%
    group_by(study_id, at_goal) %>%
    summarize(interval = sum(interval)) %>%
    mutate(at_goal = if_else(at_goal == TRUE, "goal", "not_goal")) %>%
    spread(at_goal, interval) %>%
    mutate(goal = coalesce(goal, 0),
           percent_goal = as.numeric(goal) / as.numeric(not_goal + goal))

dose_first <- data_doses %>%
    filter(!is.na(dose)) %>%
    group_by(study_id, med) %>%
    arrange(study_id, dose_datetime) %>%
    dmap_at("appropriate", ~ .x == "Yes") %>%
    summarize(dose_datetime_first = first(dose_datetime),
              dose_first = first(dose),
              appropriate = first(appropriate)) %>%
    left_join(data_main[c("study_id", "Weight (kg)")], by = "study_id") %>%
    rename(weight = `Weight (kg)`) %>%
    mutate(dose_wt = dose_first / weight,
           appropriate = if_else(med == "diltiazem" & dose_wt >= 0.225 & dose_wt <= 0.275, TRUE, appropriate)) %>%
    dmap_at("appropriate", ~ coalesce(.x, FALSE))

vitals_doses <- data_vitals %>%
    left_join(dose_first, by = c("study_id", "med")) %>%
    arrange(study_id, vital_datetime) %>%
    mutate(time_from_dose = difftime(vital_datetime, dose_datetime_first, units = "min")) %>%
    filter(vital_datetime >= dose_datetime_first)

vital_first_dose <- vitals_doses %>%
    group_by(study_id) %>%
    summarize(time_next_vital = first(time_from_dose),
              hr = first(hr)) %>%
    mutate(vital_within_30min = time_next_vital <= 30,
           next_vital_goal = hr < 110)

vital_hr_goal <- vitals_doses %>%
    filter(hr < 110) %>%
    group_by(study_id) %>%
    summarize(time_dose_goal = first(time_from_dose)) %>%
    mutate(primary = time_dose_goal <= 30)

data_tidy <- data_main %>%
    left_join(dose_first[c("study_id", "dose_first", "appropriate")], by = "study_id") %>%
    rename(appropriate_dose = appropriate) %>%
    left_join(goal_hr, by = "study_id") %>%
    left_join(vital_first_dose, by = "study_id") %>%
    left_join(vital_hr_goal, by = "study_id") %>%
    select(study_id,
           med,
           dose_first,
           appropriate_dose,
           goal,
           not_goal,
           percent_goal,
           time_dose_goal,
           primary,
           next_vital_goal,
           time_next_vital,
           vital_within_30min,
           everything()) %>%
    dmap_at("primary", ~ coalesce(.x, FALSE))

write_csv(data_tidy, "data/tidy/data_ashp_poster.csv")
write_rds(data_tidy, "data/final/data_tidy.Rds")
