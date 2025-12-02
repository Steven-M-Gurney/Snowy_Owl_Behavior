###############################################################################
# Snowy Owl Banding Summary 
# Author: Steven M. Gurney
# Last updated: 26 NOV 2025
#
# Purpose:
#   • Summarize snowy owl banding records at DTW from 2016–2025
#   • Generate recapture statistics by migration period
#   • Compare recapture rates before and after 2019–2020
#   • Export results for reporting and manuscript development
###############################################################################

# 📦 Load Required Packages ----------------------------------------------------
# Load necessary R package for data wrangling.
library(dplyr)


# =============================================================================
# 📁 1. Load & Prepare Banding Data
# =============================================================================

# Read in WCAA–USDA banding dataset (retrieved 26 Nov 2025)
bands <- read.csv("bands.csv") %>%
  # Keep only snowy owl records
  filter(species_code == "SNOW") %>%
  # Create migration period variables
  mutate(
    # Determine migration period start year based on banding month:
    # • Sept–Dec = same calendar year
    # • Jan–Aug  = previous calendar year
    migration_start = case_when(
      BANDING_MONTH >= 9 ~ BANDING_YEAR,
      BANDING_MONTH <= 8 ~ BANDING_YEAR - 1
    ),
    # Create the "YYYY–YYYY" migration label (e.g., "2016–2017")
    migration_label = paste0(migration_start, "-", migration_start + 1)
  ) %>%
  # Restrict to migration periods where WCAA maintained consistent reporting
  filter(migration_start >= 2016)


# =============================================================================
# 🔁 2. Recapture Summary by Migration Period
# =============================================================================

# Summarize banding activity within each migration period
recap_summary <- bands %>%
  group_by(migration_label) %>%
  summarise(
    # Total captures = total records for that period
    total_captures = n(),
    
    # Unique bands = distinct individuals banded or recaptured
    unique_bands   = n_distinct(band_num),
    
    # Recaptures = total minus unique individuals
    recaptures     = total_captures - unique_bands,
    
    # Recapture rate = proportion of encounters that were repeat individuals
    recapture_rate = recaptures / total_captures,
    
    # Drop group structure
    .groups = "drop"
  ) %>%
  # Arrange by chronological migration period
  arrange(migration_label)

# Save recapture summary by migration period
write.csv(
  recap_summary,
  "SNOW_recapture_summary_by_migration_period.csv",
  row.names = FALSE
)


# =============================================================================
# 📊 3. Overall Recapture Rate (Across All Years)
# =============================================================================

# Sum across all migration periods
overall_total_captures <- sum(recap_summary$total_captures)  # Count of all encounters
overall_total_unique    <- sum(recap_summary$unique_bands)   # Total unique individuals
overall_total_recaps    <- overall_total_captures - overall_total_unique  # Total recaptures

# Compute overall proportion of recaptures
overall_recapture_rate  <- overall_total_recaps / overall_total_captures

# Create one-row summary table
overall_results <- data.frame(
  overall_total_captures = overall_total_captures,
  overall_total_unique   = overall_total_unique,
  overall_total_recaps   = overall_total_recaps,
  overall_recapture_rate = overall_recapture_rate
)

# Save overall recapture summary
write.csv(
  overall_results,
  "SNOW_recapture_overall_summary.csv",
  row.names = FALSE
)


# =============================================================================
# 📆 4. Compare Early vs. Late Migration Periods
# =============================================================================

# Add period grouping variable:
# • "2018–2019 and before" vs. "2019–2020 and after"
recap_summary <- recap_summary %>%
  mutate(period_group =
           ifelse(migration_label >= "2019-2020",
                  "2019–2021_and_after",
                  "2018–2019_and_before"))

# Summarize capture/recapture metrics within each period group
group_compare <- recap_summary %>%
  group_by(period_group) %>%
  summarise(
    total_captures = sum(total_captures),    # Combined captures within group
    total_unique   = sum(unique_bands),      # Combined unique individuals
    total_recaps   = sum(recaptures),        # Combined recaptures
    recapture_rate = total_recaps / total_captures,  # Group-level recapture rate
    .groups = "drop"
  )

# Save early vs. late comparison results
write.csv(
  group_compare,
  "SNOW_recapture_group_comparison.csv",
  row.names = FALSE
)


# =============================================================================
# 🔁 5. Recaptured in Different Periods
# =============================================================================

# When was a bird banded and recaptured in a different migration period?
bands %>%
  distinct(band_num, migration_label) %>%
  group_by(band_num) %>%
  filter(n() > 1) %>%               # more than one period
  arrange(band_num, migration_label)
