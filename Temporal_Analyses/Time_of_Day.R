###############################################################################
# Snowy Owl Time-of-Day Analyses
# Author: Steven M. Gurney
# Last updated: 14 NOV 2025
#
# Purpose:
#   • Load and clean WCAA snowy owl activity data from multiple sources
#   • Standardize time formatting
#   • Merge datasets and harmonize labels
#   • Prepare for analysis of time-of-day activity
#   • Estimate peak activity and capture times
#   • Produce density plots for observations and captures
#
###############################################################################

# 📦 Load Required Packages ----------------------------------------------------
# Load all R packages needed for data manipulation, date/time handling, plotting
library(dplyr)
library(lubridate)
library(stringr)
library(readr)
library(ggplot2)
library(ggridges)


# =============================================================================
# 📁 1. Load Merged and Prepared WCAA Data
# -----------------------------------------------------------------------------
# Read the merged and prepared dataset, which includes WCAA's historical and Survey123 snowy owl data.
# =============================================================================

# Read in clean, merged WCAA data (historical + Survey123)
dat <- read.csv("SNOW_prepped_15Nov2025.csv")


# =============================================================================
# 🕒 2. Prepare Time-of-Day Data
# -----------------------------------------------------------------------------
# Filter records with valid time data, convert to POSIX for plotting,
# and compute decimal hours for density estimation.
# =============================================================================

dat_time <- dat %>%
  filter(!is.na(time) & time != "") %>%
  mutate(
    time_posix = parse_date_time(time, orders = c("HM", "HMS"), tz = "UTC"),
    hour = hour(time_posix) + minute(time_posix) / 60
  )

cat("Number of records with valid time:", nrow(dat_time), "\n")


# =============================================================================
# 📊 3. Explore Time-of-Day Distribution
# -----------------------------------------------------------------------------
# Visualize the distribution of time-of-day activity for each source
# using ridgeline plots to identify patterns in observation times.
# =============================================================================

ggplot(dat_time, aes(x = hour, y = source, fill = source)) +
  geom_density_ridges(alpha = 0.7, color = "gray20", scale = 1.2) +
  scale_x_continuous(
    name = "Time of Day (hour, 24h format)",
    breaks = seq(0, 24, by = 3),
    labels = c("Midnight", "3 AM", "6 AM", "9 AM", "Noon", "3 PM", "6 PM", "9 PM", "Midnight")
  ) +
  labs(
    title = "Time of Day of Snowy Owl Activity Records (All Data)",
    y = "Data Source",
    fill = "Source"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank()
  )


# =============================================================================
# 🔍 4. Estimate Activity Peaks (All Records)
# -----------------------------------------------------------------------------
# Use density estimation to identify the top two peak activity times
# in decimal hours for all records with valid time.
# =============================================================================

d <- density(dat_time$hour, bw = "nrd0", from = 0, to = 24)
peaks_all <- d$x[which(diff(sign(diff(d$y))) == -2)]
peak_values <- d$y[which(diff(sign(diff(d$y))) == -2)]
top2_idx <- order(peak_values, decreasing = TRUE)[1:2]
peaks_top2 <- sort(peaks_all[top2_idx])

cat("Top 2 activity peaks (24-hour format):\n")
for (p in peaks_top2) {
  cat(sprintf(" • %02d:%02d\n", floor(p), round((p %% 1) * 60)))
}


# =============================================================================
# 📈 5. Plot Time-of-Day Density (All Records)
# -----------------------------------------------------------------------------
# Visualize overall activity density with vertical lines marking
# the top two peak activity times.
# =============================================================================

ggplot(dat_time, aes(x = hour)) +
  geom_density(color = "skyblue", size = 1.5, fill = NA) +
  geom_vline(xintercept = peaks_top2, linetype = "dashed", color = "grey60", linewidth = 1) +
  scale_x_continuous(
    name = "Time of day (24 hour)",
    limits = c(0, 24),
    breaks = seq(0, 24, by = 2),
    labels = sprintf("%02d:00", seq(0, 24, by = 2))
  ) +
  labs(y = "Density of snowy owl\nactivity") +
  theme_classic(base_size = 14)

ggsave("snowy_owl_time_density.tiff", width = 6.5, height = 4, dpi = 600)


# =============================================================================
# 🎯 6. Estimate Capture Peaks (Translocated Birds)
# -----------------------------------------------------------------------------
# Filter only records for captured/translocated birds and estimate
# peak capture times using density analysis.
# =============================================================================

dat_trans <- dat %>%
  filter(result == "Translocated" & !is.na(time) & time != "") %>%
  mutate(
    time_posix = parse_date_time(time, orders = c("HMS", "HM"), tz = "UTC"),
    hour = hour(time_posix) + minute(time_posix) / 60
  )

d <- density(dat_trans$hour, bw = "nrd0", from = 0, to = 24)
peaks_all <- d$x[which(diff(sign(diff(d$y))) == -2)]
peak_values <- d$y[which(diff(sign(diff(d$y))) == -2)]
top2_idx <- order(peak_values, decreasing = TRUE)[1:2]
peaks_top2 <- sort(peaks_all[top2_idx])

cat("Top 2 activity peaks for Translocated snowy owls (24-hour format):\n")
for (p in peaks_top2) {
  cat(sprintf(" • %02d:%02d\n", floor(p), round((p %% 1) * 60)))
}


# =============================================================================
# 📈 7. Plot Capture Density Curve
# -----------------------------------------------------------------------------
# Plot density of capture times for translocated birds and mark
# the top two peaks.
# =============================================================================

ggplot(dat_trans, aes(x = hour)) +
  geom_density(color = "skyblue", size = 1.5, fill = NA) +
  geom_vline(xintercept = peaks_top2, linetype = "dashed", color = "grey60", linewidth = 1) +
  scale_x_continuous(
    name = "Time of day (24 hour)",
    limits = c(0, 24),
    breaks = seq(0, 24, by = 2),
    labels = sprintf("%02d:00", seq(0, 24, by = 2))
  ) +
  labs(y = "Density of snowy owl\ncaptures") +
  theme_classic(base_size = 14)

ggsave("snowy_owl_time_density_translocated.tiff", width = 6.5, height = 4, dpi = 600)

