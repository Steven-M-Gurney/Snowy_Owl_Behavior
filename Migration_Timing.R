###############################################################################
# Snowy Owl Migration Timing
# Author: Steven M. Gurney
# Last updated: 25 NOV 2025
#
# Purpose:
#   • Load winter migration summaries
#   • Estimate mean migration start/end dates with uncertainty
#   • Calculate migration-period duration and error
#   • Visualize migration windows
###############################################################################

# 📦 Load Required Packages ----------------------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

# =============================================================================
# 📁 1. Load Precomputed Summaries --------------------------------------------
#   Contains: migration_label, min (start DOY), max (end DOY)
# =============================================================================
summary_winter <- read_csv("SNOW_BoxSummary_TrueDates_ByMigration.csv")

# Ensure correct numeric formatting
summary_winter <- summary_winter %>%
  mutate(
    min = as.numeric(min),
    max = as.numeric(max)
  )

# Number of migration periods
n_years <- nrow(summary_winter)

# =============================================================================
# 🧮 2. Helper Function --------------------------------------------------------
#   Convert fiscal-day-of-year to actual date (1 Sep = day 1)
# =============================================================================
fiscal_day_to_date <- function(day_fiscal, ref_year = 2020) {
  as.Date(paste0(ref_year, "-09-01")) + days(round(day_fiscal) - 1)
}

# =============================================================================
# 📊 3. Summary Statistics: Start & End Dates ----------------------------------
#   Compute mean, SD, SE, and 95% CI for start and end (fiscal DOY)
# =============================================================================

# ---- Start date stats ----
start_stats <- summary_winter %>%
  summarise(
    mean_start = mean(min, na.rm = TRUE),
    sd_start   = sd(min, na.rm = TRUE),
    se_start   = sd_start / sqrt(n_years),
    ci_low     = mean_start - 1.96 * se_start,
    ci_high    = mean_start + 1.96 * se_start
  )

# ---- End date stats ----
end_stats <- summary_winter %>%
  summarise(
    mean_end = mean(max, na.rm = TRUE),
    sd_end   = sd(max, na.rm = TRUE),
    se_end   = sd_end / sqrt(n_years),
    ci_low   = mean_end - 1.96 * se_end,
    ci_high  = mean_end + 1.96 * se_end
  )

# Convert stats to calendar dates
start_stats_dates <- start_stats %>%
  mutate(across(everything(), fiscal_day_to_date))

end_stats_dates <- end_stats %>%
  mutate(across(everything(), fiscal_day_to_date))

# =============================================================================
# 📏 4. Migration-Period Duration ----------------------------------------------
#   Duration = max – min (days)
#   Compute mean duration, SD, SE, CI, min, and max durations
# =============================================================================
summary_winter <- summary_winter %>%
  mutate(mig_length_days = max - min)

length_stats <- summary_winter %>%
  summarise(
    mean_length = mean(mig_length_days, na.rm = TRUE),
    sd_length   = sd(mig_length_days, na.rm = TRUE),
    se_length   = sd_length / sqrt(n_years),
    ci_low      = mean_length - 1.96 * se_length,
    ci_high     = mean_length + 1.96 * se_length,
    min_duration = min(mig_length_days, na.rm = TRUE),
    max_duration = max(mig_length_days, na.rm = TRUE)
  )

# =============================================================================
# 🗂️ 5. Prepare Data for Plotting ----------------------------------------------
#   Convert min/max to reference-year dates for visualization
# =============================================================================
plot_df <- summary_winter %>%
  mutate(
    date_min_plot = fiscal_day_to_date(min),
    date_max_plot = fiscal_day_to_date(max)
  )

# Order migration periods (bottom → top)
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

plot_df <- plot_df %>%
  mutate(migration_label = factor(migration_label, levels = migration_levels_ordered))

# Mean start/end dates for vertical lines
avg_start_date <- fiscal_day_to_date(start_stats$mean_start)
avg_end_date   <- fiscal_day_to_date(end_stats$mean_end)

# =============================================================================
# 📉 6. Plot Migration Windows -------------------------------------------------
# =============================================================================
ggplot(plot_df, aes(y = migration_label)) +
  geom_segment(aes(
    x = date_min_plot, 
    xend = date_max_plot, 
    yend = migration_label
  ),
  color = "grey40", linewidth = 3, alpha = 0.6) +
  geom_point(aes(x = date_min_plot), color = "green4", size = 3) +
  geom_point(aes(x = date_max_plot), color = "firebrick", size = 3) +
  geom_vline(xintercept = avg_start_date, linetype = "dashed",
             color = "green4", linewidth = 1) +
  geom_vline(xintercept = avg_end_date, linetype = "dashed",
             color = "firebrick", linewidth = 1) +
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

# =============================================================================
# 💾 7. Save Figure -------------------------------------------------------------
# =============================================================================
ggsave("snowy_owl_migration_windows.tiff",
       width = 6.5, height = 5, dpi = 600)

# =============================================================================
# 📄 8. Save Summary Table (numeric DOY values) --------------------------------
# =============================================================================
summary_table <- tibble(
  metric = c("start_mean", "start_sd", "start_se", "start_ci_low", "start_ci_high",
             "end_mean",   "end_sd",   "end_se",   "end_ci_low",  "end_ci_high",
             "length_mean","length_sd","length_se","length_ci_low","length_ci_high",
             "length_min", "length_max"),
  
  value = c(
    start_stats$mean_start,
    start_stats$sd_start,
    start_stats$se_start,
    start_stats$ci_low,
    start_stats$ci_high,
    
    end_stats$mean_end,
    end_stats$sd_end,
    end_stats$se_end,
    end_stats$ci_low,
    end_stats$ci_high,
    
    length_stats$mean_length,
    length_stats$sd_length,
    length_stats$se_length,
    length_stats$ci_low,
    length_stats$ci_high,
    length_stats$min_duration,
    length_stats$max_duration
  )
)

write_csv(summary_table, "SNOW_Migration_SummaryStats.csv")

# =============================================================================
# 📄 9. Save Summary Table (Calendar Dates) ------------------------------------
# =============================================================================
summary_table_dates <- tibble(
  metric = c("start_mean_date", "start_ci_low_date", "start_ci_high_date",
             "end_mean_date",   "end_ci_low_date",   "end_ci_high_date"),
  
  value = c(
    fiscal_day_to_date(start_stats$mean_start),
    fiscal_day_to_date(start_stats$ci_low),
    fiscal_day_to_date(start_stats$ci_high),
    
    fiscal_day_to_date(end_stats$mean_end),
    fiscal_day_to_date(end_stats$ci_low),
    fiscal_day_to_date(end_stats$ci_high)
  )
)

write_csv(summary_table_dates, "SNOW_Migration_SummaryStats_Dates.csv")

# =============================================================================
# 📄 10. Save Duration Table (Mean + Error + Min/Max) ---------------------------
# =============================================================================
duration_table <- tibble(
  metric = c("length_mean", "length_sd", "length_se",
             "length_ci_low", "length_ci_high",
             "length_min", "length_max"),
  
  value = c(
    length_stats$mean_length,
    length_stats$sd_length,
    length_stats$se_length,
    length_stats$ci_low,
    length_stats$ci_high,
    length_stats$min_duration,
    length_stats$max_duration
  )
)

write_csv(duration_table, "SNOW_Migration_DurationStats.csv")
