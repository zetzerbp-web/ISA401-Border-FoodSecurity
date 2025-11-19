############################################################
# Focus on most recent year (e.g., 2023) and key indicators
############################################################

library(tidyverse)
library(stringr)

# 1. Read the combined dataset you already created --------------------------

master_data <- readr::read_csv("Project Report Data/master_border_data_tidy.csv")

# 2. Keep only the most recent year available -------------------------------

latest_year <- max(master_data$year, na.rm = TRUE)
latest_year

master_latest <- master_data %>%
  filter(year == latest_year)

# 3. Drop obvious junk / structural columns ---------------------------------

master_latest <- master_latest %>%
  # drop "na_*" columns
  select(-starts_with("na_"),
         # drop columns like x2000, x2005, x2019, etc.
         -matches("^x[0-9]{4}"),
         # drop unnamed columns if any
         -matches("^unnamed"))

# Also drop any columns that are entirely NA
master_latest <- master_latest %>%
  select(where(~ !all(is.na(.))))

# 4. Identify ID columns and critical indicator columns ---------------------

# ID columns we care about, if they exist
id_candidates <- c("fips", "state", "county", "county_state", "county_name")
id_present <- intersect(id_candidates, names(master_latest))

# Patterns that indicate "critical" variables
critical_patterns <- c(
  "food_insecure", "food_insecurity", "food_security",
  "poverty",
  "unemploy",           # unemployment, unemployed
  "median_household_income", "med_hh", "income",
  "bachelor", "hs_grad", "high_school", "education",
  "educational_attainment",
  "snap",
  "low_food_security", "very_low_food_security"
)

# For each column name, check if it matches any of the patterns
critical_name_flags <- str_detect(
  names(master_latest),
  regex(paste(critical_patterns, collapse = "|"), ignore_case = TRUE)
)

# Numeric columns only
numeric_cols <- names(master_latest)[sapply(master_latest, is.numeric)]

# Critical numeric columns = numeric AND matching one of the patterns
critical_numeric <- intersect(
  numeric_cols,
  names(master_latest)[critical_name_flags]
)

# 5. Build final, focused dataset -------------------------------------------

master_final <- master_latest %>%
  select(
    all_of(id_present),     # IDs
    all_of(critical_numeric)  # key numeric indicators
  )

# 6. Quick sanity checks ----------------------------------------------------

glimpse(master_final)
summary(select(master_final, where(is.numeric)))

nrow(master_final)
dplyr::n_distinct(master_final$fips)

# 7. Save final, polished dataset ------------------------------------------

readr::write_csv(master_final,
                 "Project Report Data/master_border_data_tidy_latest_year_clean.csv")
saveRDS(master_final,
        "Project Report Data/master_border_data_tidy_latest_year_clean.rds")
