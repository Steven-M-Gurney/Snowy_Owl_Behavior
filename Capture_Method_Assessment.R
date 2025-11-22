###############################################################################
# Snowy Owl Capture-Method Assessment 
# Author: Steven M. Gurney
# Last updated: 15 NOV 2025
#
# Purpose:
#   • Summarize the use of snowy owl capture methods
#   • Visualize percent of successful captures by method used.
#
###############################################################################

# 📦 Load Required Packages ----------------------------------------------------
# Load all R packages needed for data manipulation and plotting
library(dplyr)
library(ggplot2)
library(stringr)


# =============================================================================
# 📁 1. Load Prepped Dataset
# -----------------------------------------------------------------------------
# This section imports the merged snowy owl dataset used for analysis.
# =============================================================================

# Load cleaned WCAA data (historical + Survey123)
dat <- read.csv("SNOW_prepped_15Nov2025.csv")   # Read dataset from CSV



# =============================================================================
# 📊 2. Summarize Capture Success by Method
# -----------------------------------------------------------------------------
# This section produces tables summarizing counts and percentages for each 
# method used during successful snowy owl captures.
# =============================================================================

#Print how many captures we have based on outcome (translocated)
dat %>% count(result == "Translocated") # 97

# Summarize counts by capture method (management)
translocated_summary <- dat %>%                     # Begin with dataset
  filter(result == "Translocated") %>%              # Keep only translocated birds
  group_by(management) %>%                          # Group by capture method
  summarise(n = n(), .groups = "drop") %>%          # Count occurrences
  arrange(desc(n))                                  # Sort descending

# Print the summary table to view results
translocated_summary                                 

# Calculate percentages by capture method (management)
translocated_summary_pct <- dat %>%                 # Begin with dataset
  filter(result == "Translocated") %>%              # Keep only translocated birds
  count(management) %>%                             # Count each method
  mutate(percent = n / sum(n) * 100) %>%            # Convert to %
  arrange(desc(n))                                  # Sort descending

# Print the summary table to view results
translocated_summary_pct                            

# Write results to CSV
write.csv(translocated_summary_pct,                 # export table with %
          "translocated_management_counts_pct.csv") # output file name



# =============================================================================
# 🎨 3. Prepare Labels for Plotting
# -----------------------------------------------------------------------------
# This section standardizes and cleans capture-method labels for readability 
# before visualization.
# =============================================================================

# Create a new column without parentheses for labeling
translocated_summary_pct <- translocated_summary_pct %>% 
  mutate(management_label = str_remove(management, " \\(.*\\)"))   # Remove parentheses text

# Put label in sentence case and rename "Snare" as "Phai trap"
translocated_summary_pct <- translocated_summary_pct %>% 
  mutate(
    management_label = str_to_sentence(management_label),          # Convert to sentence case
    management_label = str_replace_all(management_label,           # Rename "Snare" as "Phai trap"
                                       "\\bSnare\\b", "Phai trap")
  )



# =============================================================================
# 📈 4. Plot Percent of Capture Methods
# -----------------------------------------------------------------------------
# This section generates a bar chart showing the relative frequency of 
# translocation capture methods (%).
# =============================================================================

# Plot 
ggplot(translocated_summary_pct,
       aes(x = reorder(management_label, -percent), y = percent)) +    # order bars by %
  geom_col(fill = "skyblue", alpha = 0.7) +                             # draw bars
  geom_text(aes(label = round(percent, 1)),                             # add %
            vjust = -0.5, size = 4) +                                   # adjust text position
  labs(
    x = "Capture method",                                               # x-axis label
    y = "Percent of snowy owl captures (%)"                             # y-axis label
  ) +
  theme_classic(base_size = 14) +                                       # clean theme
  theme(
    axis.text.x = element_text(hjust = 1, angle = 45),                  # rotate labels
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),   # title styling
    axis.title.x = element_text(size = 16, face = "bold"),              # axis label styling
    axis.title.y = element_text(size = 16, face = "bold")
  )



# =============================================================================
# 💾 5. Save High-Resolution Figure
# -----------------------------------------------------------------------------
# This section exports the capture-method plot as a high-resolution TIFF file.
# =============================================================================

# Save the plot in high definition, scaled to fit nicely in Word document
ggsave("snowy_owl_methods.tiff",       # Output filename
       width = 6.5,                    # Width in inches
       height = 5.5,                   # Height in inches
       dpi = 600)                      # High resolution for publication
