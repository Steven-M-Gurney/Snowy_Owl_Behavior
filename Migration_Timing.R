###############################################################################
# Snowy Owl Migration Timing
# Author: Steven M. Gurney
# Last updated: 15 NOV 2025
#
# Purpose:
#   • Visualize migration timing and estimate average migration start and end
#
###############################################################################

# 📦 Load required packages ----------------------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

# =============================================================================
# 📁 1. Load Precomputed Summaries
# -----------------------------------------------------------------------------
summary_winter  <- read_csv("SNOW_BoxSummary_TrueDates_ByMigration.csv")

# Compute overall averages across all migration periods
summary_overall <- summary_winter %>%
  summarise(
    avg_min_day = mean(min, na.rm = TRUE),
    avg_max_day = mean(max, na.rm = TRUE)
  )

# =============================================================================
# 📁 2. Helper: Convert fiscal day to reference-year date
# -----------------------------------------------------------------------------
fiscal_day_to_date <- function(day_fiscal, ref_year = 2020) {
  as.Date(paste0(ref_year, "-09-01")) + days(round(day_fiscal) - 1)
}

# =============================================================================
# 📁 3. Prepare plotting data
# -----------------------------------------------------------------------------
plot_df <- summary_winter %>%
  mutate(
    date_min_plot = fiscal_day_to_date(min),
    date_max_plot = fiscal_day_to_date(max)
  )

avg_start_date <- fiscal_day_to_date(summary_overall$avg_min_day)
avg_end_date   <- fiscal_day_to_date(summary_overall$avg_max_day)

# =============================================================================
# 📁 4. Plot migration windows (cleaned)
# -----------------------------------------------------------------------------
## Ensure 'min' is numeric for correct ordering
summary_winter <- summary_winter %>%
  mutate(min = as.numeric(min),
         max = as.numeric(max))

# Prepare plotting data
plot_df <- summary_winter %>%
  mutate(
    date_min_plot = fiscal_day_to_date(min),
    date_max_plot = fiscal_day_to_date(max)
  )

# Calculate average start and stop dates
avg_start_date <- fiscal_day_to_date(mean(summary_winter$min, na.rm = TRUE))
avg_end_date   <- fiscal_day_to_date(mean(summary_winter$max, na.rm = TRUE))

# Define migration periods in chronological order (bottom → top)
migration_levels_ordered <- c(
  "2016–2017",
  "2017–2018",
  "2018–2019",
  "2019–2020",
  "2020–2021",
  "2021–2022",
  "2022–2023",
  "2023–2024",
  "2024–2025"
)

# Apply above to plot_df
plot_df <- plot_df %>%
  mutate(migration_label = factor(migration_label, levels = migration_levels_ordered))


# Plot
ggplot(plot_df, aes(y = migration_label)) +
  geom_segment(aes(x = date_min_plot, xend = date_max_plot, yend = migration_label),
               color = "grey40", linewidth = 3, alpha = 0.6) +
  geom_point(aes(x = date_min_plot), color = "green4", size = 3) +
  geom_point(aes(x = date_max_plot), color = "firebrick", size = 3) +
  geom_vline(xintercept = avg_start_date, linetype = "dashed", color = "green4", linewidth = 1) +
  geom_vline(xintercept = avg_end_date, linetype = "dashed", color = "firebrick", linewidth = 1) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    limits = c(as.Date("2020-10-01"), as.Date("2021-05-22")),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(x = "Month", y = "Migration period") +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold")
  )

# Save high-definition figure scaled for Word doc
ggsave("snowy_owl_migration_windows.tiff", width = 6.5, height = 5, dpi = 600)
