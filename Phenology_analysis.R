if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(readr)) install.packages("readr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(lubridate)) install.packages("lubridate")

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(ggpubr)
library(scales)
library(lubridate)

# Load the data
df <- read_csv("Parnassius_NA_Harmonized_Full.csv")

df_pheno <- df %>%
  filter(!is.na(year)) %>%
  mutate(era = case_when(
    year < 1990 ~ "Pre-1990",
    year >= 1990 ~ "Post-1990"
  )) %>%
  # Ensure Era is a factor with correct chronological order
  mutate(era = factor(era, levels = c("Pre-1990", "Post-1990"))) %>%
  # Create a clean Date object
  mutate(clean_date = make_date(year, month, day)) %>%
  # Calculate Day of Year (1 = Jan 1st, 365 = Dec 31st)
  mutate(doy = yday(clean_date)) %>%
  # Filter out rows with missing dates or missing Era
  filter(!is.na(doy) & !is.na(era))

species_list <- unique(df_pheno$species)

# ==============================================================================
# T-Test
# loop through each species to test the hypothesis:
# There are significant date of flying differences between eras.
# ==============================================================================

sink("TTest_Phenology_Results.txt")

print("-------------------------------------------------------------")
print(" HYPOTHESIS TEST: Do species fly EARLIER (Lower Day of Year)?")
print("-------------------------------------------------------------")

for (sp in species_list) {
  
  sp_data <- df_pheno %>% filter(species == sp)
  
  print(paste("Analyzing Species:", sp))
  
  # Check sample size per era
  counts <- table(sp_data$era)
  if(length(unique(sp_data$era)) < 2) {
    print("  -> Not enough data across eras for T-Test. Skipping.")
    print(counts)
    next
  }
  
  # TWO-SAMPLE T-TEST
  # Formula: Day of Year ~ Era
  t_test_model <- t.test(doy ~ era, data = sp_data)
  p_val <- t_test_model$p.value
  
  print(paste("  -> T-test p-value:", format(p_val, scientific = FALSE)))
  
  # Check Direction of Shift
  if(p_val < 0.05) {
    print("  -> SIGNIFICANT SHIFT DETECTED.")
    
    # Extract means (Group 1 is Historical, Group 2 is Modern)
    mean_hist <- t_test_model$estimate[1]
    mean_mod  <- t_test_model$estimate[2]
    diff <- mean_mod - mean_hist
    
    if(diff < 0) {
      print(paste("  -> RESULT: Flies", round(abs(diff), 1), "days EARLIER (Supports Hypothesis)"))
    } else {
      print(paste("  -> RESULT: Flies", round(diff, 1), "days LATER (Rejects Hypothesis)"))
    }
  } else {
    print("  -> No significant phenological shift detected.")
  }
  print("-------------------------------------------------------------")
}

sink()
print("Stats saved to 'TTest_Phenology_Results.txt'.")

# ==============================================================================
# T-Test graph
# ==============================================================================

my_colors <- c("Pre-1990" = "#509579",     
               "Post-1990" = "#c6e3d8")    

my_comparisons <- list( c("Pre-1990", "Post-1990") )

plot_pheno <- ggplot(df_pheno, aes(x = era, y = doy, fill = era)) +
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.3) +
  
  stat_compare_means(comparisons = my_comparisons, 
                     method = "t.test", 
                     label = "p.signif") +
  
  facet_wrap(~species, scales = "free_y", nrow = 1) +
  
  scale_fill_manual(values = my_colors) +
  
  # OVERLAP FIX + Date Labels
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2)),
                     breaks = c(152, 182, 213, 244), 
                     labels = c("June 1", "July 1", "Aug 1", "Sept 1")) +
  
  labs(title = "Phenology Shifts: Pre-1990 vs. Post-1990", 
       subtitle = "Changes in Peak Flight Timing",
       y = "Flight Date") +
  
  theme_bw(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "italic", size = 11)
  )

ggsave("Parnassius_Phenology_Final_2Eras.png", plot_pheno, width = 14, height = 6)
print(plot_pheno)
print("Graph saved as 'Parnassius_Phenology_Final_2Eras.png'")

# ==============================================================================
# Chi-square
# ==============================================================================

df_chi <- df_pheno %>%
  mutate(season = ifelse(month %in% c(5, 6), "Early (May-June)", "Late (July-Sept)"))

# calculate p-value and stars
sig_df <- df_chi %>%
  group_by(species) %>%
  summarise(
    # Chi-Square and p-value
    p_val = tryCatch(chisq.test(table(era, season))$p.value, error = function(e) 1),
    .groups = 'drop'
  ) %>%
  mutate(
    # p-value to Stars
    label = case_when(
      p_val < 0.001 ~ "***",
      p_val < 0.01  ~ "**",
      p_val < 0.05  ~ "*",
      TRUE          ~ ""
    ),
    # Coordinates for the star
    x = 1.5, 
    y = 1.05 
  )

# Print the results table to console
print("Chi-Square Significance Results:")
print(sig_df)

# ==============================================================================
# Chi-square graph
# ==============================================================================
# Calculate CI
plot_data <- df_pheno %>%
  mutate(is_early = ifelse(month %in% c(5, 6), 1, 0)) %>%
  group_by(species, era) %>%
  summarise(
    total_count = n(),
    early_count = sum(is_early),
    prop = early_count / total_count,
    # Standard Error Formula for Proportions: sqrt( p*(1-p) / n )
    se = sqrt((prop * (1 - prop)) / total_count),
    # 95% Confidence Interval: +/- 1.96 * SE
    ci_lower = pmax(0, prop - 1.96 * se), # prevent negative bars
    ci_upper = prop + 1.96 * se,
    .groups = 'drop'
  )

# Match stars to plot data
sig_data <- plot_data %>%
  group_by(species) %>%
  summarise(max_y = max(ci_upper) * 1.2) %>% 
  left_join(sig_df, by = "species") 

# Graphing
plot_prop <- ggplot(plot_data, aes(x = era, y = prop, fill = era)) +
  
  # the bar chart
  geom_bar(stat = "identity", width = 0.7, color = "black", alpha = 0.8) +
  
  # 95% CI error bars
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, linewidth = 0.8, color = "gray20") +
  
  # stars
  geom_text(data = sig_data, aes(x = 1.5, y = max_y, label = label), 
            inherit.aes = FALSE, size = 6, fontface = "bold") +
  
  # Layout
  facet_wrap(~species, nrow = 1) +
  
  # Colors updated to 2-Era names
  scale_fill_manual(values = c("Pre-1990" = "#48597e", 
                               "Post-1990" = "#8d9fbb")) +
  
  # Labels
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     expand = expansion(mult = c(0, 0.2))) +
  
  labs(title = "Increase in Early Spring Emergence",
       subtitle = "Proportion of flights in May/June (with 95% Confidence Intervals)",
       y = "Percentage of Records",
       fill = "Era") +
  
  theme_bw(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "italic")
  )

ggsave("Parnassius_Proportion_ErrorBars_2Eras.png", plot_prop, width = 14, height = 6)
print(plot_prop)
print("Graph saved as 'Parnassius_Proportion_ErrorBars_2Eras.png'")

