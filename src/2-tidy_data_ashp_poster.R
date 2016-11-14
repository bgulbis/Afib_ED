
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
dilt_doses <- read_excel(dilt_file, sheet = "Initial Agent Boluses- Dilt", skip = 1, col_names = col_nm, col_types = col_type) %>%
    mutate(med = "diltiazem") %>%
    filter(yes_no == "No")

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

data_tidy <- data_main %>%
    left_join(goal_hr, by = "study_id") %>%
    select(study_id, med, goal, not_goal, percent_goal, everything())

write_csv(data_tidy, "data/tidy/data_ashp_poster.csv")
