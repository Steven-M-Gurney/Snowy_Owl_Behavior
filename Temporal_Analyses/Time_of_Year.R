###############################################################################
# Snowy Owl Activity Analyses (Sept–Aug Fiscal Year)
# Author: Steven M. Gurney
# Last updated: 15 NOV 2025
#
# Purpose:
#   • Clean and merge WCAA activity and historical datasets
#   • Assign migration periods (Sept–Aug)
#   • Compute fiscal day-of-year (Sept 1 = 1) for all plots and summaries
#   • Create density plots, boxplots, ridge plots, and summary tables
#
###############################################################################

# 📦 Load Required Packages ----------------------------------------------------
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggridges)
library(readr)

# =============================================================================
# 📁 1. Read Cleaned Snowy Owl Data
# -----------------------------------------------------------------------------
# Load merged and prepped dataset containing mmdd, year, and other relevant fields
# =============================================================================
dat <- read.csv("SNOW_prepped_15Nov2025.csv", stringsAsFactors = FALSE)

# =============================================================================
# 📁 2. Prepare Migration Periods & Fiscal Day
# -----------------------------------------------------------------------------
# Convert mmdd to numeric month/day
# Define migration_start and migration_label (Sept–Aug fiscal year)
# Calculate fiscal day-of-year for plots and summaries
# =============================================================================
dat <- dat %>%
  mutate(
    month = as.numeric(str_sub(mmdd, 1, 2)),
    day   = as.numeric(str_sub(mmdd, 4, 5)),
    
    # Determine migration period start year (Sept = same year, Oct–Dec = same year, Jan–Aug = previous year)
    migration_start = ifelse(month >= 9, year, year - 1),
    
    # Migration period label (e.g., "2020-2021")
    migration_label = paste0(migration_start, "\u2013", migration_start + 1),
    
    # Dummy date for reference year 2000 to compute day-of-year
    dummy_date = as.Date(paste0("2000-", month, "-", day)),
    
    # Fiscal day-of-year relative to Sept 1
    fiscal_day_of_year = yday(dummy_date) - yday(as.Date("2000-09-01")) + 1,
    fiscal_day_of_year = ifelse(fiscal_day_of_year <= 0,
                                fiscal_day_of_year + 366,
                                fiscal_day_of_year)
  )

# Filter out events before the first migration period (2016-2017)
dat <- dat %>% filter(migration_start >= 2016)

# =============================================================================
# 📁 3. Define Month/Week Breaks for Plots
# -----------------------------------------------------------------------------
# Align x-axis for Sept–Aug fiscal year
# =============================================================================
month_starts <- as.Date(c(
  "2000-09-01","2000-10-01","2000-11-01","2000-12-01",
  "2001-01-01","2001-02-01","2001-03-01","2001-04-01",
  "2001-05-01","2001-06-01","2001-07-01","2001-08-01"
))
month_breaks <- yday(month_starts) - yday(as.Date("2000-09-01")) + 1
month_breaks <- ifelse(month_breaks <= 0, month_breaks + 366, month_breaks)
month_labels <- month.abb[c(9:12,1:8)]
week_breaks <- seq(1, 366, by = 7)

# Ensure migration periods are in chronological order
migration_levels <- dat %>%
  distinct(migration_label, migration_start) %>%
  arrange(migration_start) %>%
  pull(migration_label)

# =============================================================================
# 📁 4. Density Plot of Activity
# -----------------------------------------------------------------------------
# Smooth density plot of snowy owl activity over fiscal year
# =============================================================================
ggplot(dat, aes(x = fiscal_day_of_year)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  scale_x_continuous(breaks = month_breaks, labels = month_labels) +
  labs(x = "Month", y = "Density of snowy owl activity") +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold")
  )

ggsave("snowy_owl_activity_density.tiff", width = 6.5, height = 4, dpi = 600)

# =============================================================================
# 📁 5. Boxplot by Migration Period
# -----------------------------------------------------------------------------
# Shows distribution of events by fiscal day across migration periods
# =============================================================================
ggplot(dat, aes(y = factor(migration_label, levels = migration_levels),
                x = fiscal_day_of_year)) +
  geom_boxplot(outlier.shape = NA, fill = "skyblue", alpha = 0.5,
               median.colour = "purple", median.linewidth = 2) +
  geom_jitter(height = 0.2, alpha = 0.6, color = "darkblue", size = 1.3) +
  scale_x_continuous(
    breaks = month_breaks,
    minor_breaks = week_breaks,
    labels = month_labels,
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(x = "Month", y = "Migration period") +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold")
  )

ggsave("snowy_owl_activity_by_migration_period.tiff", width = 6.5, height = 5, dpi = 600)

# =============================================================================
# 📁 6. Summary Table of Migration Timing
# -----------------------------------------------------------------------------
# Summarize min, max, median, quartiles, and IQR for each migration period
# =============================================================================
fiscal_day_to_date <- function(day_fiscal, ref_year = 2020) {
  as.Date(paste0(ref_year, "-09-01")) + days(round(day_fiscal) - 1)
}

summary_winter <- dat %>%
  group_by(migration_label, migration_start) %>%
  summarise(
    n       = n(),
    min     = min(fiscal_day_of_year, na.rm = TRUE),
    q1      = quantile(fiscal_day_of_year, 0.25, na.rm = TRUE),
    median  = median(fiscal_day_of_year, na.rm = TRUE),
    mean    = mean(fiscal_day_of_year, na.rm = TRUE),
    q3      = quantile(fiscal_day_of_year, 0.75, na.rm = TRUE),
    max     = max(fiscal_day_of_year, na.rm = TRUE),
    iqr     = IQR(fiscal_day_of_year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    date_min    = fiscal_day_to_date(min, migration_start),
    date_q1     = fiscal_day_to_date(q1, migration_start),
    date_median = fiscal_day_to_date(median, migration_start),
    date_q3     = fiscal_day_to_date(q3, migration_start),
    date_max    = fiscal_day_to_date(max, migration_start)
  ) %>%
  arrange(migration_start) %>%
  select(migration_label, n, min:max, date_min:date_max)

write_csv(summary_winter, "SNOW_BoxSummary_TrueDates_ByMigration.csv")

# =============================================================================
# 📁 7. Ridge Plots by Migration Period
# -----------------------------------------------------------------------------
# Visualize the density of events by fiscal day across migration periods
# =============================================================================
ggplot(dat, aes(x = fiscal_day_of_year, y = factor(migration_label, levels = migration_levels))) +
  geom_density_ridges(
    scale = 1.2, alpha = 0.6, fill = "skyblue", color = "grey40"
  ) +
  scale_x_continuous(
    breaks = month_breaks[2:8],  # Oct → Apr
    minor_breaks = week_breaks[week_breaks >= min(dat$fiscal_day_of_year) &
                                 week_breaks <= max(dat$fiscal_day_of_year)],
    labels = month_labels[2:8],
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(x = "Month", y = "Density of snowy owl activity\nby migration period") +
  theme_classic(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold")
  )

ggsave("snowy_owl_activity_ridges.tiff", width = 6.5, height = 5, dpi = 600)
