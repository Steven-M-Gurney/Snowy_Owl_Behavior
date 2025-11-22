###############################################################################
# Snowy Owl Data Prep  
# Author: Steven M. Gurney  
# Last updated: 15 NOV 2025
#
# Purpose:
#   • Load, wrangle, and clean snowy owl activity data
#   • Standardize dates, times, labels
#   • Merge WCAA historical and Survey123 datasets
#
###############################################################################

# 📦 Load Required Packages ----------------------------------------------------
# Load packages for data manipulation, date handling, and string cleaning.
library(dplyr)
library(lubridate)
library(stringr)


# =============================================================================
# 📁 1. Load and Clean Activity Dataset (Recent WCAA Activity Data)
# -----------------------------------------------------------------------------
# This section imports the Survey123 dataset, standardizes date/time fields, 
# selects relevant variables, and tags the data as "activity".
# =============================================================================

# Read WCAA snowy owl activity data from Survey123 (exported 14 Oct 2025)
SNOW <- read.csv("SNOW_Activity_14Oct2025.csv", stringsAsFactors = FALSE)

# Reformat time data
SNOW <- SNOW %>%
  mutate(
    # Convert the primary datetime field to POSIXct for consistent time handling
    activity_date = as.POSIXct(activity_date, format = "%m/%d/%Y %H:%M"),
    
    # Determine which timestamp to use: use 'activity_date_est' if present,
    # otherwise fall back to the primary 'activity_date'
    time = if ("activity_date_est" %in% names(.)) {
      format(
        as.POSIXct(activity_date_est, format = "%m/%d/%Y %H:%M"),
        "%H:%M"
      )
    } else {
      format(activity_date, "%H:%M")
    },
    
    # Extract month/day component for later seasonal summaries
    mmdd = format(activity_date, "%m/%d")
  ) %>%
  # Reduce to the relevant columns and rename activity_year to year for consistency
  select(activity_year, mmdd, time, zone, management, result) %>%
  rename(year = activity_year) %>%
  # Add a source flag indicating this dataset came from Survey123
  mutate(source = "activity")


# =============================================================================
# 📁 2. Load and Clean Historical Dataset
# -----------------------------------------------------------------------------
# This section filters the historical database for snowy owls, cleans time and 
# date fields, standardizes column names, and tags the data as "historical".
# =============================================================================

# Read WCAA historical wildlife activity data (all species)
HIST <- read.csv("HistoricalActivity_16Oct2025.csv")

# Filter to only include snowy owl data
HIST <- HIST %>%
  filter(species_code == "SNOW") %>%
  mutate(
    # Trim extra whitespace in time field
    time = str_trim(time),
    # Remove any remaining spaces (e.g., "7 : 30 PM")
    time = str_replace_all(time, "\\s+", ""),
    # Convert to uppercase to standardize AM/PM formatting
    time = str_to_upper(time),
    # Parse cleaned time string into a consistent HH:MM format,
    # using multiple possible order templates to handle inconsistencies
    time = suppressWarnings(format(parse_date_time(time,
                                                   orders = c("HMSp", "HMSp", "IMSp", "IMS p")),
                                   "%H:%M")),
    # Convert date column to POSIXct and extract month/day for summaries
    date = as.POSIXct(date, format = "%m/%d/%Y %H:%M"),
    mmdd = format(date, "%m/%d")
  ) %>%
  # Retain only columns relevant to snowy owl activity summaries
  select(year, mmdd, time, zone, management, disposition) %>%
  # Rename disposition to result to match the activity dataset structure
  rename(result = disposition) %>%
  # Tag dataset origin for downstream tracking
  mutate(source = "historical")



# =============================================================================
# 🔗 3. Merge Datasets and Standardize Labels
# -----------------------------------------------------------------------------
# This section merges the cleaned datasets and standardizes result and 
# management labels to create a unified dataset for analysis.
# =============================================================================

# Combine the two snowy owl datasets from above
dat <- bind_rows(SNOW, HIST)

# Harmonize result labels (simplify all capture outcomes to "translocated")
dat <- dat %>%
  mutate(result = case_when(
    result %in% c("Transrelocated (T)", "Relocated", "Foreign Re-trap (FR)", "Taken to Rehabber") ~ "Translocated",
    TRUE ~ result
  ))

# Harmonize management labels (inconsistent labeling between data sources)
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
  # Replace cases where translocated birds labeled as "Truck (T)" were actually transported by hand
  mutate(management = if_else(
    result == "Translocated" & management == "Truck (T)",
    "Hand (H)",
    management
  ))



# =============================================================================
# 🧭 4. Assign Migration Periods (Sept–Aug Cycles)
# -----------------------------------------------------------------------------
# This section extracts month/day fields and assigns each record to a Sept–Aug 
# migration period labeled "YYYY-YYYY", then filters out periods before 2016–2017.
# =============================================================================

# Extract month and day fields
dat <- dat %>%
  mutate(
    month = as.numeric(str_sub(mmdd, 1, 2)),
    day   = as.numeric(str_sub(mmdd, 4, 5)),
    
    # Assign migration period start year (migration begins in Sept)
    migration_start = case_when(
      month >= 9  ~ year,        # Sept–Dec use the same year as start
      month <= 8  ~ year - 1     # Jan–Aug belong to previous year's migration period
    ),
    
    # Create migration label "YYYY-YYYY"
    migration_label = paste0(migration_start, "-", migration_start + 1)
  ) %>%
  # Remove any data before the first period (Sept 2016–Aug 2017)
  filter(migration_start >= 2016)

# Save the prepped data for analyses
write.csv(dat, "SNOW_prepped_15Nov2025.csv", row.names = FALSE)
