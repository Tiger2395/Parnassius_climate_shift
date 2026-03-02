install.packages("rgbif")
library(rgbif)

# Search for the species key
species_key <- name_backbone(name='Parnassius smintheus')$usageKey

# Request the download (requires a GBIF account)
occ_download(
  pred("taxonKey", species_key),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus", "PRESENT"),
  pred("continent", "North America"), 
  format = "SIMPLE_CSV",
  user = "photoleek", 
  pwd = "Yanmo@1689929055", 
  email = "photoleek@gmail.com"
)
