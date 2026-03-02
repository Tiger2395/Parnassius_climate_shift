library(dplyr)
library(readr)
library(stringr) 
library(elevatr)
library(sf)

# Define the columns to keep
cols_to_keep <- c("gbifID", "species", "infraspecificEpithet", 
                  "scientificName", "verbatimScientificName",
                  "decimalLatitude", "decimalLongitude", 
                  "coordinateUncertaintyInMeters",
                  "year", "month", "day", "eventDate", 
                  "basisOfRecord", "recordedBy")

p_clodius <- read_tsv("Parnassius_clodius.csv", quote = "") %>% select(any_of(cols_to_keep))
p_eversmanni <- read_tsv("Parnassius_eversmanni.csv", quote = "") %>% select(any_of(cols_to_keep))
p_phoebus <- read_tsv("Parnassius_phoebus.csv", quote = "") %>% select(any_of(cols_to_keep))
p_smintheus_raw <- read_tsv("Parnassius_smintheus.csv", quote = "") %>% select(any_of(cols_to_keep))
p_behrii_raw <- read_tsv("Parnassius_behrii.csv", quote = "") %>% select(any_of(cols_to_keep))

behrii_indices <- which(
  str_detect(p_smintheus_raw$infraspecificEpithet, regex("behrii", ignore_case = TRUE)) |
    str_detect(p_smintheus_raw$scientificName, regex("behrii", ignore_case = TRUE)) |
    str_detect(p_smintheus_raw$verbatimScientificName, regex("behrii", ignore_case = TRUE))
)

# Extract and Rename
behrii_rescued <- p_smintheus_raw[behrii_indices, ] %>%
  mutate(species = "Parnassius behrii")

# Clean smintheus and merge behrii
p_smintheus_clean <- p_smintheus_raw[-behrii_indices, ]
p_behrii_final <- bind_rows(p_behrii_raw, behrii_rescued) %>%
  mutate(species = "Parnassius behrii")

# Merge into one raw dataset
raw_data <- bind_rows(p_clodius, p_eversmanni, p_phoebus, p_smintheus_clean, p_behrii_final)

# Removing records with same Collector + Date + Location
deduped_data <- raw_data %>%
  distinct(recordedBy, eventDate, decimalLatitude, decimalLongitude, species, .keep_all = TRUE)

print(paste("Removed", nrow(raw_data) - nrow(deduped_data), "duplicate records."))

# SPATIAL FILTERING 
# Must have coordinates
# Uncertainty must be known AND <= 10,000m (10km)
spatial_clean_data <- deduped_data %>%
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
  filter(!is.na(coordinateUncertaintyInMeters) & coordinateUncertaintyInMeters <= 10000)

print(paste("Removed", nrow(deduped_data) - nrow(spatial_clean_data), "records with poor spatial accuracy."))

# ERA CATEGORIZATION 
era_data <- spatial_clean_data %>%
  filter(!is.na(year)) %>% 
  mutate(era = case_when(
    year < 1990 ~ "Pre-1990",
    year >= 1990 ~ "Post-1990"
  )) %>%
  # Make it a factor to ensure chronological order in all future graphs
  mutate(era = factor(era, levels = c("Pre-1990", "Post-1990")))

# Quick check to ensure all data is accounted for
print("Record counts per Era:")
print(table(era_data$era))

# ELEVATION EXTRACTION
# Requires internet

# Create a projection (WGS84)
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Fetch elevation
# elevation_df <- get_elev_point(locations = era_data, 
#                              src = "aws", 
#                               prj = prj_dd, 
#                               z = 10) 

era_data_sf <- st_as_sf(era_data, 
                        coords = c("decimalLongitude", "decimalLatitude"), 
                        crs = 4326, 
                        remove = FALSE)

elevation_results <- get_elev_point(locations = era_data_sf, 
                                    src = "aws", 
                                    z = 5)


# convert output from spatial object to a regular Dataframe
final_dataset <- as.data.frame(elevation_results) %>%
  select(-geometry) %>% # Remove the weird geometry column (not needed for CSV)
  rename(extracted_elevation = elevation) # Rename to be clear

head(final_dataset$extracted_elevation)
# SAVE FINAL FILE
write_csv(final_dataset, "Parnassius_NA_Harmonized_Full.csv")

print("Processing Complete. Final columns included:")
print(names(final_dataset))
print(table(final_dataset$era)) # Show how many records are in each Era
