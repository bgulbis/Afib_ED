# tidy_data

library(tidyverse)
library(readxl)
library(stringr)

cols <- c("study_id", "gender", "age", "weight", "height", "race",
          "ec_admit_datetime", "ec_discharge_datetime", "ec_los",
          "admit_location", "admit_datetime", "ec_admit_los", "new_afib",
          "diagnosis", "complaint", "complaint_other", "mortality",
          "cause_death", "cause_death_other", "pmh", "ef", "substance_abuse",
          "home_anticoag", "home_antiplatelet", "home_betablocker",
          "home_nondhp_ccb", "home_dhp_ccb", "home_other", "compliance",
          "iv_fluid", "fluid_type", "volume", "volume_kg", "fluid_datetime",
          "cardiovert", "cardiovert_adverse", "other", "initial_worked",
          "comments")

raw <- read_excel("data/external/manual_data_demographics.xlsx",
                  sheet = "demographics", skip = 3, col_names = FALSE)

names(raw) <- cols

tidy <- raw %>%
    filter(!is.na(gender)) %>%
    mutate(admitted = !is.na(admit_datetime),
           ec_discharge_datetime = coalesce(ec_discharge_datetime, admit_datetime),
           ec_los = difftime(ec_discharge_datetime, ec_admit_datetime, units = "hours"),
           new_afib = if_else(new_afib == "Yes", TRUE, FALSE),
           initial_worked = if_else(initial_worked == "Yes", TRUE, FALSE),
           pmh_aicd = str_detect(pmh, "AICD"),
           pmh_af = str_detect(pmh, "AF"),
           pmh_afl = str_detect(pmh, "AFL"),
           pmh_asthma = str_detect(pmh, "Asthma"),
           pmh_avb2nd = str_detect(pmh, "2nd AV block"),
           pmh_avb3rd = str_detect(pmh, "3rd AV block"),
           pmh_cva = str_detect(pmh, "CVA"),
           pmh_ckd = str_detect(pmh, "CKD"),
           pmh_copd = str_detect(pmh, "COPD"),
           pmh_chf = str_detect(pmh, "CHF"),
           pmh_cad = str_detect(pmh, "CAD"),
           pmh_dm = str_detect(pmh, "DM"),
           pmh_htn = str_detect(pmh, "HTN"),
           pmh_smoker = str_detect(pmh, "Smoker"),
           pmh_substance = str_detect(pmh, "Substance Abuse (___)"),
           pmh_thyroid = str_detect(pmh, "Thyroid"),
           pmh_valve = str_detect(pmh, "Valvular disease"),
           pmh_vte = str_detect(pmh, "VTE"),
           pmh_unknown = str_detect(pmh, "Unknown"),
           home_asprin = str_detect(home_antiplatelet, "ASA"),
           home_clopid = str_detect(home_antiplatelet, "clopidogrel")) %>%
    select(-home_antiplatelet, -admit_datetime, -ec_admit_los, -pmh)

write_csv(tidy, "data/tidy/tidy_demographics.csv")

cols_vitals <- c("numeric", "date", "numeric", "numeric", "numeric", "numeric")

raw_vitals <- read_excel("data/external/manual_data_vitals.xlsx",
                         sheet = "vitals", col_types = cols_vitals) %>%
    filter(!is.na(vital_datetime))

write_csv(raw_vitals, "data/tidy/tidy_vitals.csv")

raw_tx <- read_excel("data/external/manual_data_treatment.xlsx",
                     sheet = "treatment", skip = 3, col_names = FALSE)

dilt <- raw_tx %>%
    select(X0, X2, X3) %>%
    filter(!is.na(X2)) %>%
    rename(study_id = X0,
           dose_datetime = X2,
           dose = X3)

dilt2 <- raw_tx %>%
    select(X0, X6, X7) %>%
    filter(!is.na(X6)) %>%
    rename(study_id = X0,
           dose_datetime = X6,
           dose = X7)

dilt3 <- raw_tx %>%
    select(X0, X10, X11) %>%
    filter(!is.na(X10)) %>%
    rename(study_id = X0,
           dose_datetime = X10,
           dose = X11)

dilt3 <- raw_tx %>%
    select(X0, X10, X11) %>%
    filter(!is.na(X10)) %>%
    rename(study_id = X0,
           dose_datetime = X10,
           dose = X11)

dilt4 <- raw_tx %>%
    select(X0, X14, X15) %>%
    filter(!is.na(X14)) %>%
    rename(study_id = X0,
           dose_datetime = X14,
           dose = X15)

dilt <- bind_rows(dilt, dilt2, dilt3, dilt4) %>%
    mutate(drug = "diltiazem",
           route = "iv")

metop <- raw_tx %>%
    select(X0, X19, X20, X21) %>%
    filter(!is.na(X19)) %>%
    rename(study_id = X0,
           dose_datetime = X19,
           dose = X20) %>%
    mutate(drug = "metoprolol",
           route = if_else(dose == "Other", "po", "iv"),
           dose = as.numeric(dose),
           dose = coalesce(dose, 20)) %>%
    select(-X21)

met2 <- raw_tx %>%
    select(X0, X23, X24) %>%
    filter(!is.na(X23)) %>%
    rename(study_id = X0,
           dose_datetime = X23,
           dose = X24)

met3 <- raw_tx %>%
    select(X0, X27, X28) %>%
    filter(!is.na(X27)) %>%
    rename(study_id = X0,
           dose_datetime = X27,
           dose = X28)

met4 <- raw_tx %>%
    select(X0, X31, X32) %>%
    filter(!is.na(X31)) %>%
    rename(study_id = X0,
           dose_datetime = X31,
           dose = X32)

metop <- bind_rows(metop, met2, met3, met4) %>%
    mutate(drug = coalesce(drug, "metoprolol"),
           route = coalesce(route, "iv"))

worked <- raw_tx %>%
    select(X0, X35) %>%
    filter(!is.na(X35)) %>%
    rename(study_id = X0,
           dose_datetime = X35) %>%
    mutate(drug = if_else(study_id >= 100, "diltiazem", "metoprolol"),
           route = "po")

drugs <- c("Dilt IVP" = "diltiazem", "Amio IVBP 10 mins" = "amiodarone",
           "Mg Sulfate" = "magnesium", "Dilt PO" = "diltiazem",
           "Dilt" = "diltiazem", "Amio Drip" = "amiodarone")

fail1 <- raw_tx %>%
    select(X0, X37, X38, X39, X40, X41, X42) %>%
    filter(!is.na(X38)) %>%
    rename(study_id = X0,
           drug = X37,
           dose_datetime = X38,
           route = X42,
           infusion_rate = X40,
           infusion_units = X41) %>%
    dmap_at("drug", str_replace_all, pattern = drugs) %>%
    mutate(dose = as.numeric(str_extract(X39, "[0-9]*")),
           dose = if_else(str_detect(X39, "gm"), dose * 1000, dose),
           route = coalesce(route, "iv"),
           route = str_to_lower(route),
           infusion_rate = if_else(infusion_units == "mL/hr",
                                   dose / 125 * infusion_rate,
                                   infusion_rate)) %>%
    select(-X39)

fail2 <- raw_tx %>%
    select(X0, X43:X48) %>%
    filter(!is.na(X43)) %>%
    rename(study_id = X0,
           drug = X44,
           dose_datetime = X43,
           route = X48,
           infusion_rate = X46,
           infusion_units = X47) %>%
    dmap_at("drug", str_replace_all, pattern = drugs) %>%
    mutate(dose = as.numeric(str_extract(X45, "[0-9]*")),
           route = coalesce(route, "po"),
           route = str_to_lower(route),
           infusion_rate = if_else(infusion_units == "mL/hr",
                                   dose / 125 * infusion_rate,
                                   infusion_rate)) %>%
    select(-X45)

fail3 <- raw_tx %>%
    select(X0, X49:X54) %>%
    filter(!is.na(X50)) %>%
    rename(study_id = X0,
           drug = X50,
           dose_datetime = X49,
           route = X54,
           infusion_rate = X52,
           infusion_units = X53) %>%
    dmap_at("drug", str_replace_all, pattern = drugs) %>%
    mutate(dose = as.numeric(str_extract(X51, "[0-9]*")),
           route = coalesce(route, "iv"),
           route = str_to_lower(route),
           infusion_rate = if_else(infusion_units == "mL/hr",
                                   dose / 125 * infusion_rate,
                                   infusion_rate)) %>%
    select(-X51)

fail <- bind_rows(fail1, fail2, fail3) %>%
    mutate(dose = if_else(!is.na(infusion_rate), infusion_rate, dose),
           route = if_else(!is.na(infusion_rate), "cont", route)) %>%
    select(-infusion_rate, -infusion_units)

tidy_treatment <- bind_rows(dilt, metop, worked, fail)

write_csv(tidy_treatment, "data/tidy/tidy_treatment.csv")

goal_hr <- raw_vitals %>%
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

adverse_vitals <- raw_vitals %>%
    group_by(study_id) %>%
    mutate(low_sbp = sbp < 90,
           low_hr = hr < 60) %>%
    summarize(low_sbp = sum(low_sbp) > 0,
              low_hr = sum(low_hr) > 0) %>%
    mutate(low_sbp = coalesce(low_sbp, FALSE),
           low_hr = coalesce(low_hr, FALSE))

meds_summary <- tidy_treatment %>%
    arrange(study_id, dose_datetime) %>%
    group_by(study_id, drug, route) %>%
    summarize(first = first(dose_datetime),
              last = last(dose_datetime)) %>%
    filter((study_id < 100 & drug == "metoprolol") |
               (study_id >= 100 & drug == "diltiazem")) %>%
    mutate(date = if_else(route == "iv", last, first)) %>%
    select(-first, -last) %>%
    spread(route, date) %>%
    mutate(time_po = difftime(po, iv, units = "hours")) %>%
    select(study_id, drug, time_po)

abstract <- full_join(tidy, goal_hr, by = "study_id") %>%
    full_join(adverse_vitals, by = "study_id") %>%
    full_join(meds_summary, by = "study_id")

write_csv(abstract, "data/final/ashp_abstract_data.csv")
# write.csv(abstract, "data/final/ashp_abstract_data.csv", row.names = FALSE)
