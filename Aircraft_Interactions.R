###############################################################################
# Snowy Owl Strikes 
# Author: Steven M. Gurney
# Last updated: 25 NOV 2025
#
# Purpose:
#   • Import FAA strike records for snowy owls
#   • Assign migration periods (Sept–Aug) and associated years (e.g., 2016–2017)
#   • Summarize strike counts and damage by airport and migration period
#   • Produce stacked bar chart comparing DTW vs. all other airports
###############################################################################

# 📦 Load Required Packages ----------------------------------------------------
library(readxl)      # Read FAA Excel database
library(dplyr)       # Data wrangling
library(lubridate)   # Date handling
library(stringr)     # String ops
library(ggplot2)     # Plotting

# =============================================================================
# 🗂️ 1. Load & Filter Snowy Owl Strike Data -----------------------------------
#   • Filter by species
#   • Restrict to ≥ 2016
# =============================================================================

df <- read_excel("FAA_StrikeDatabase_FULL_24OCT2025.xlsx") %>%
  filter(
    SPECIES == "Snowy owl",
    INCIDENT_YEAR >= 2016
  )

write.csv(df, "FAA_SNOW_Strikes_24Oct2025.csv", row.names = FALSE)

# =============================================================================
# 🔄 2. Assign Migration Periods (Sept–Aug Fiscal Year) ------------------------
#   • Convert date fields
#   • Assign migration_start
#   • Create migration-period label
# =============================================================================

df <- read.csv("FAA_SNOW_Strikes_24Oct2025.csv") 

df <- df %>%
  mutate(
    INCIDENT_DATE = as.Date(INCIDENT_DATE),
    month = month(INCIDENT_DATE),
    day   = day(INCIDENT_DATE),
    year  = year(INCIDENT_DATE),
    migration_start = ifelse(month >= 9, year, year - 1),
    migration_period = paste0(migration_start, "\u2013", migration_start + 1)
  ) %>%
  filter(migration_start >= 2016) %>%  
  arrange(INCIDENT_DATE)

# =============================================================================
# 📊 3. Summary: Strikes & Damage by Airport ----------------------------------
#   • Total strikes
#   • Damage indicated events
# =============================================================================

df_summary <- df %>%
  group_by(AIRPORT) %>%
  summarise(
    Count           = n(),
    Damage_True_Sum = sum(INDICATED_DAMAGE == TRUE, na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  arrange(desc(Count))

write.csv(df_summary, "SnowyStrikes_FAA_2016-2024.csv", row.names = FALSE)

# =============================================================================
# 📅 4. Summary: Strikes by Migration Period (All Airports) --------------------
#   • Sept–Aug seasonal summaries
# =============================================================================

df_year_summary <- df %>%
  group_by(migration_period) %>%
  summarise(
    Count           = n(),
    Damage_True_Sum = sum(INDICATED_DAMAGE == TRUE, na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  arrange(migration_period)

write.csv(df_year_summary, "SnowyStrikes_FAA_byYear.csv", row.names = FALSE)

# =============================================================================
# 🛬 5. DTW-Only Summaries ------------------------------------------------------
#   • Filter to DTW
#   • Strikes and damage by migration period
# =============================================================================

dtw_summary <- df %>%
  filter(AIRPORT == "DETROIT METRO WAYNE COUNTY ARPT") %>%
  group_by(migration_period) %>%
  summarise(
    Count           = n(),
    Damage_True_Sum = sum(INDICATED_DAMAGE == TRUE, na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  arrange(migration_period)

# =============================================================================
# 📈 6. Stacked Bar Plot: DTW vs All Other Airports ----------------------------
#   • DTW = steelblue
#   • All other airports = light grey
# =============================================================================

plot_df <- df %>%
  mutate(Source = if_else(
    AIRPORT == "DETROIT METRO WAYNE COUNTY ARPT",
    "DTW", "All other"
  )) %>%
  group_by(migration_period, Source) %>%
  summarise(Count = n(), .groups = "drop")

plot_df <- plot_df %>%
  mutate(migration_period = factor(
    migration_period,
    levels = sort(unique(migration_period))
  ))

ggplot(plot_df, aes(x = migration_period, y = Count, fill = Source)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("All other" = "lightgray", "DTW" = "steelblue")) +
  labs(
    x = "Migration Period",
    y = "Number of snowy owl strikes",
    fill = "Airport"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold")
  )

ggsave("SnowyStrikes_FAA_StackedBar.tiff",
       width = 7, height = 5, dpi = 600)
