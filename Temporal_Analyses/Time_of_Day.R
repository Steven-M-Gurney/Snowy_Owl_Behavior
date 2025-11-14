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


# =============================================================================
# 🕒 4. Prepare Time-of-Day Data
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
# 📊 5. Explore Time-of-Day Distribution
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
# 🔍 6. Estimate Activity Peaks (All Records)
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
# 📈 7. Plot Time-of-Day Density (All Records)
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
# 🎯 8. Estimate Capture Peaks (Translocated Birds)
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
# 📈 9. Plot Capture Density Curve
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

