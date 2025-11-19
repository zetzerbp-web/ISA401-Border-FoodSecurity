############################################################
# ISA 401 Border Counties – Data Cleaning & Merge Script
# Assumes files are in: "Project Report Data/"
############################################################

library(tidyverse)
library(janitor)
library(stringr)

############################################################
# 1. Read raw files
############################################################

education_raw <- readr::read_csv("Project Report Data/Education.csv",
                                 col_names = FALSE)
foodsecurity  <- readr::read_csv("Project Report Data/Food security.csv")
County        <- readr::read_csv("Project Report Data/County.csv")
poverty_1     <- readr::read_csv("Project Report Data/PovertyReport1.csv")
poverty_2     <- readr::read_csv("Project Report Data/PovertyReport2.csv")
poverty_3     <- readr::read_csv("Project Report Data/PovertyReport3.csv")
poverty_4     <- readr::read_csv("Project Report Data/PovertyReport4.csv")
unemp         <- readr::read_csv("Project Report Data/Unemployment2023.csv")

############################################################
# 2. Education: fix header row and create fips
############################################################

# Education CSV appears to have a title row, then a header row.
# Here we assume row 2 holds the real column names.
education_clean <- education_raw %>%
  row_to_names(row_number = 2) %>%  # if needed, change 2 to 1
  clean_names()

# Detect a FIPS-like column and standardize to "fips"
fips_col_ed <- names(education_clean)[
  str_detect(names(education_clean), "fips|geoid|county_code|cty_fips")
][1]

if (!is.na(fips_col_ed)) {
  education_clean <- education_clean %>%
    mutate(
      fips = str_pad(as.character(.data[[fips_col_ed]]), 5, pad = "0")
    )
}

############################################################
# 3. Helper: clean names + standardize FIPS for other tables
############################################################

standardize_fips <- function(df) {
  df_clean <- df %>%
    clean_names()
  
  # If "fips" already exists, just standardize it
  if ("fips" %in% names(df_clean)) {
    df_clean <- df_clean %>%
      mutate(fips = str_pad(as.character(fips), 5, pad = "0"))
    return(df_clean)
  }
  
  # Otherwise, look for a likely FIPS column
  fips_candidates <- names(df_clean)[str_detect(
    names(df_clean),
    "fips|geoid|county_code|cty_fips"
  )]
  
  if (length(fips_candidates) > 0) {
    fips_col <- fips_candidates[1]
    df_clean <- df_clean %>%
      mutate(
        fips = str_pad(as.character(.data[[fips_col]]), 5, pad = "0")
      )
  }
  
  df_clean
}

############################################################
# 4. Clean county-level datasets (except Education)
############################################################

# foodsecurity is national/aggregate by category and year; no FIPS here.
foodsecurity_clean <- foodsecurity %>%
  clean_names()

county_clean    <- standardize_fips(County)
poverty_1_clean <- standardize_fips(poverty_1)
poverty_2_clean <- standardize_fips(poverty_2)
poverty_3_clean <- standardize_fips(poverty_3)
poverty_4_clean <- standardize_fips(poverty_4)
unemp_clean     <- standardize_fips(unemp)

############################################################
# 5. Combine poverty files into one table
############################################################

safe_first_non_na <- function(x) {
  x_no_na <- x[!is.na(x)]
  if (length(x_no_na) == 0) NA else x_no_na[1]
}

poverty_all <- bind_rows(
  poverty_1_clean,
  poverty_2_clean,
  poverty_3_clean,
  poverty_4_clean
)

poverty_all_collapsed <- poverty_all %>%
  group_by(fips) %>%
  summarise(across(everything(), safe_first_non_na), .groups = "drop")

############################################################
# 6. Slim each dataset to fips + numeric indicators
############################################################

county_small <- county_clean %>%
  select(fips, everything())

education_small <- education_clean %>%
  select(fips, where(is.numeric))

poverty_small <- poverty_all_collapsed %>%
  select(fips, where(is.numeric))

unemp_small <- unemp_clean %>%
  select(fips, where(is.numeric))

############################################################
# 7. Build master county-level dataset (do NOT join foodsecurity here)
############################################################

master_data <- county_small %>%
  left_join(poverty_small,   by = "fips", suffix = c("", "_pov")) %>%
  left_join(unemp_small,     by = "fips", suffix = c("", "_unemp")) %>%
  left_join(education_small, by = "fips", suffix = c("", "_edu"))

############################################################
# 8. Inspect and save
############################################################

glimpse(master_data)
summary(select(master_data, where(is.numeric)))

readr::write_csv(master_data, "Project Report Data/master_border_data_tidy.csv")
saveRDS(master_data,          "Project Report Data/master_border_data_tidy.rds")
