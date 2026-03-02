if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(readr)) install.packages("readr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(ggpubr)) install.packages("ggpubr")

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(ggpubr)
library(scales)

# Load the data
df <- read_csv("Parnassius_NA_Harmonized_Full.csv")

df <- df %>%
  filter(!is.na(year)) %>% # Must have a year
  mutate(era = case_when(
    year < 1990 ~ "Pre-1990",
    year >= 1990 ~ "Post-1990"
  )) %>%
  # Convert to factor to lock in the chronological order
  mutate(era = factor(era, levels = c("Pre-1990", "Post-1990")))

# Get list of unique species
species_list <- unique(df$species)

# ==============================================================================
# T-Test
# loop through each species to test the hypothesis:
# There are significant elevation differences between eras.
# ==============================================================================

sink("TTest_Elevation_Results.txt") 

print("-------------------------------------------------------------")
print(" HYPOTHESIS TEST: Do species move UP in elevation over time? ")
print("-------------------------------------------------------------")

for (sp in species_list) {
  
  # Subset data for one species
  sp_data <- df %>% 
    filter(species == sp) %>%
    filter(!is.na(extracted_elevation))
  
  print(paste("Analyzing Species:", sp))
  
  # Check if we have data for both eras to compare
  if(length(unique(sp_data$era)) < 2) {
    print("  -> Not enough eras for T-test. Skipping.")
    print("-------------------------------------------------------------")
    next
  }
  
  # TWO-SAMPLE T-TEST
  # Formula: Elevation depends on Era
  t_test_model <- t.test(extracted_elevation ~ era, data = sp_data)
  
  # Extract p-value
  p_val <- t_test_model$p.value
  print(paste("  -> T-test p-value:", format(p_val, scientific = FALSE)))
  
  # Check for hypothesis direction if significant
  if(p_val < 0.05) {
    print("  -> SIGNIFICANT DIFFERENCE FOUND.")
    
    # Extract the means for both groups from the t-test object
    mean_hist <- t_test_model$estimate[1] # Mean of pre1990
    mean_mod <- t_test_model$estimate[2]  # Mean of post1990
    
    diff <- mean_mod - mean_hist
    
    if(diff > 0) {
      print(paste("  -> RESULT: Moved UP by", round(diff, 2), "meters (Supports Hypothesis)"))
    } else {
      print(paste("  -> RESULT: Moved DOWN by", round(abs(diff), 2), "meters (Rejects Hypothesis)"))
    }
  } else {
    print("  -> No significant shift detected.")
  }
  print("-------------------------------------------------------------")
}

sink() # Stop saving to file
print("Statistical analysis complete. Results saved to 'TTest_Elevation_Results_Log.txt'.")

# ==============================================================================
# DATA VISUALIZATION (THE "NICE" PLOT)
# ==============================================================================

my_colors <- c("Pre-1990" = "#509579",     
               "Post-1990" = "#c6e3d8")    

# Define the specific comparison you want to highlight
my_comparisons <- list( c("Pre-1990", "Post-1990") )

plot_elev <- ggplot(df, aes(x = era, y = extracted_elevation, fill = era)) +
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.3) +
  
  # Add Significance Stars (Uses T-test automatically)
  stat_compare_means(comparisons = my_comparisons, 
                     method = "t.test", 
                     label = "p.signif") + 
  
  facet_wrap(~species, scales = "free_y", nrow = 1) +
  
  scale_fill_manual(values = my_colors) +
  
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  
  labs(title = "Elevation Shifts: Pre-1990 vs. Post-1990", 
       subtitle = "Significance: * p<0.05, ** p<0.01, *** p<0.001, ns = Not Significant",
       y = "Elevation (meters)") +
  
  theme_bw(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "italic", size = 11)
  )


# Save the plot
ggsave("Parnassius_Elevation_Final_2Eras.png", plot_elev, width = 14, height = 6)
print(plot_elev)

print("Graph saved as 'Parnassius_Elevation_Final_2Eras.png'")
