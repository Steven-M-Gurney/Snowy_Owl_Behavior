###############################################################################
# Snowy Owl Data Prep
# Author: Steven M. Gurney
# Last updated: 15 NOV 2025
#
# Purpose:
#   • Load and clean WCAA snowy owl activity data from multiple sources
#   • Standardize time formatting
#   • Merge datasets and harmonize labels
#
###############################################################################

# 📦 Load Required Packages ----------------------------------------------------
# Load all R packages needed for data manipulation, date and time handling
library(dplyr)
library(lubridate)
library(stringr)


# =============================================================================
# 📁 1. Load and Clean Activity Dataset (Recent WCAA Activity Data)
# -----------------------------------------------------------------------------
# Read the main activity dataset, extract time and date components,
# and prepare it for merging. Also tag the source as "activity" for later tracking.
# =============================================================================

SNOW <- read.csv("SNOW_Activity_14Oct2025.csv", stringsAsFactors = FALSE)

SNOW <- SNOW %>%
  mutate(
    # Convert main datetime field to POSIXct
    activity_date = as.POSIXct(activity_date, format = "%m/%d/%Y %H:%M"),
    
    # Extract hh:mm time from either activity_date_est (if available), fallback to activity_date
    time = if ("activity_date_est" %in% names(.)) {
      format(
        as.POSIXct(activity_date_est, format = "%m/%d/%Y %H:%M"),
        "%H:%M"
      )
    } else {
      format(activity_date, "%H:%M")
    },
    
    # Extract month/day for seasonal analyses
    mmdd = format(activity_date, "%m/%d")
  ) %>%
  # Keep only relevant columns and rename year
  select(activity_year, mmdd, time, zone, management, result) %>%
  rename(year = activity_year) %>%
  # Tag source for tracking
  mutate(source = "activity")


# =============================================================================
# 📁 2. Load and Clean Historical Dataset
# -----------------------------------------------------------------------------
# Read historical dataset, filter for snowy owls, clean the time
# column, parse dates, and tag the source as "historical".
# =============================================================================

HIST <- read.csv("HistoricalActivity_16Oct2025.csv")

HIST <- HIST %>%
  filter(species_code == "SNOW") %>%
  mutate(
    # Clean up time column: remove spaces, standardize AM/PM, handle NAs
    time = str_trim(time),
    time = str_replace_all(time, "\\s+", ""),
    time = str_to_upper(time),
    # Parse time into HH:MM format
    time = suppressWarnings(format(parse_date_time(time,
                                                   orders = c("HMSp", "HMSp", "IMSp", "IMS p")),
                                   "%H:%M")),
    # Convert date column and extract mmdd
    date = as.POSIXct(date, format = "%m/%d/%Y %H:%M:%S"),
    mmdd = format(date, "%m/%d")
  ) %>%
  select(year, mmdd, time, zone, management, disposition) %>%
  rename(result = disposition) %>%
  # Tag source for tracking
  mutate(source = "historical")


# =============================================================================
# 🔗 3. Merge Datasets and Standardize Labels
# -----------------------------------------------------------------------------
# Combine recent and historical datasets, harmonize the 'result'
# and 'management' columns to consistent labels, and correct any special cases.
# =============================================================================

dat <- bind_rows(SNOW, HIST)

# Harmonize result labels
dat <- dat %>%
  mutate(result = case_when(
    result %in% c("Transrelocated (T)", "Relocated", "Foreign Re-trap (FR)", "Taken to Rehabber") ~ "Translocated",
    TRUE ~ result
  ))

# Harmonize management labels
dat <- dat %>%
  mutate(management = case_when(
    str_to_upper(management) %in% c("BC") ~ "Bal-Chatri (BC)",
    str_to_upper(management) %in% c("BN") ~ "Bow Net (BN)",
    str_to_upper(management) == "H"  ~ "Hand (H)",
    str_to_upper(management) == "F"  ~ "Firearms (F)",
    str_to_upper(management) == "N"  ~ "Net (N)",
    str_to_upper(management) == "O"  ~ "Observe (O)",
    str_to_upper(management) == "P"  ~ "Pyrotechnics (P)",
    str_to_upper(management) == "PT" ~ "Pole Trap (PT)",
    str_to_upper(management) %in% c("SG") ~ "Swedish Goshawk (SG)",
    str_to_upper(management) == "T"  ~ "Truck (T)",
    TRUE ~ management
  )) %>%
  # Special case: Translocated birds recorded as "Truck" are actually "Hand"
  mutate(management = if_else(
    result == "Translocated" & management == "Truck (T)",
    "Hand (H)",
    management
  ))

# Save merged, cleaned data for analyses
write.csv(dat, "SNOW_prepped_15Nov2025.csv", row.names = FALSE)