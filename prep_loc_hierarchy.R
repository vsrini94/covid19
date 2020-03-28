## Script to prep location hierarchy for assigning cases based on published LADPH mapping
## Original file (also saved in repo): http://publichealth.lacounty.gov/media/Coronavirus/ncovid19_regions_communities.pdf
## Converted to excel table (saved in repo) for easy import with https://smallpdf.com/ and cleaneda column headers for import as csv
## Author: Vinay Srinivasan
## Date Created: 27 March 2020

rm(list = ls())

## Package management
if(!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, tidyverse, assertable)

locs_df <- fread("data/hierarchy/ncovid19_regions_communities.csv")

## Split and expand included communities into appropriate regions
child_locs_df <- locs_df[, str_split(included_communities, ";"), by = region_number]
setnames(child_locs_df, "V1", "location_label")
locs_df <- merge(locs_df[, !c("population", "included_communities")], child_locs_df, by = "region_number") #dropping pop column because because these only apply to region overall. can pull in from original csv if needed

# Create unique numeric location id for locations
locs_df <- locs_df[order(region_number, location_label)]
locs_df[, location_number := length(unique(region_number)) + seq(.N)][, level := 2]
locs_df[, region_label := str_remove(region_label, "--.*")]

# Add row for cases under investigation
locs_df <- rbind(locs_df, data.table(region_label = "Under Investigation", region_number = 9999, location_number = 9999, location_label = "Under Investigation", level = 2), use.names = T, fill = T)

# Append region level
parent_locs_df <- locs_df[, .(region_number, region_label, location_number = region_number, location_label = region_label, level = 1)] %>% unique

# Join levels 1 and 2
locs_df <- rbind(parent_locs_df, locs_df, use.names = T, fill = T)

# Clean names
locs_df[, location_label := str_remove(location_label, "\n")][, region_label := str_remove(region_label, "\n")]
locs_df[location_label %like% "Ka gel", location_label := str_replace(location_label, "Ka ", "Ka")] # Spot correcting mistake in this 
locs_df[location_label %like% "Monrovia", location_label := str_replace(location_label, "Unincorp ", "Unincorp")] # Spot correcting mistake in this 
locs_df[location_label %like% "Playa Vista", location_label := str_replace(location_label, "Los", "Los ")] # Spot correcting mistake in this 
locs_df[location_label %like% "Signal", location_label := str_replace(location_label, "Signal", "Signal ")] # Spot correcting mistake in this 
locs_df[location_label %like% "Southeast Antelope", location_label := str_replace(location_label, "Southeast Antelope", "Southeast Antelope ")] # Spot correcting mistake in this 
locs_df[location_label %like% "La HabraHeights", location_label := str_replace(location_label, "Habra", "Habra ")] # Spot correcting mistake in this 

locs_df[, location_label := str_replace(location_label, "(?<=Angeles|Unincorporated)(-)", " - ")][, region_label := str_replace(region_label, "(?<=Angeles|Unincorporated)(-)", " - ")]

# Reorder and Save
setcolorder(locs_df, c("level", "region_number", "region_label", "location_number", "location_label"))
write.csv(locs_df, "data/hierarchy/ncovid19_hierarchy_prepped.csv", row.names = F)
