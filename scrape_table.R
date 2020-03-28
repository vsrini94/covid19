## Script to scrape case table updated by LADPH daily providing current information about covid19
## This needs to be repeated daily to derive incidence, as previous days totals are only released in the form of non-standardized media reports
## Author: Vinay Srinivasan
## Date Created: 27 March 2020

rm(list = ls())

## Package management
if(!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, tidyverse, rvest, stringr, assertable)

## Functions

## Read in location hierarchy
locs_df <- fread("data/hierarchy/ncovid19_hierarchy_prepped.csv")[level == 2]

## Set date
datestamp <- Sys.Date() %>% as.character()

message(sprintf("Scraping LADPH COVID 19 Cases for %s", datestamp))

## Read in table
url <- "http://publichealth.lacounty.gov/media/Coronavirus/locations.htm"
covid_table <- read_html(url) 

covid_cities <- html_nodes(covid_table, css = 'th') %>% html_text()
covid_cases <- html_nodes(covid_table, css = 'td') %>% html_text()

scraped_table <- data.table(location_label = covid_cities[-c(1:2)],  n_cases = covid_cases)

# One-off fixes
scraped_table[location_label %like% "Azuza", location_label := "Azusa"] # Misspelled on website. This should still run even if fixed

# Clean table
scraped_table <- scraped_table[(grep("COMMUNITY", location_label)+1):grep("Under Investigation", location_label),]
scraped_table[, location_label := str_remove_all(location_label, "\\*|City of |-  ")]
scraped_table[, n_cases := str_replace(n_cases, "--", "-1")][, n_cases := as.integer(n_cases)] # --, which represents 1-4 cases in communities of pop < 25k will be coded as -1 whereas true zeroes will be zero, areas not reported but in our hierarchy will be coded as NA

# Attempt a merge and see what doesn't fit
data_df <- merge(scraped_table, locs_df, by = "location_label", all = T)

# Validate that no locations with reported cases have been improperly merged
assert_values(data_df, colnames = "region_label", test = "not_na") # checks that all reported locations have been assigned to a region we have in the hierarchy

# Add/drop necessary columns
data_df[, date_reported := datestamp]
setcolorder(data_df, c('location_number', 'location_label', 'level', 'region_number', 'region_label', 'date_reported', 'n_cases'))

# Write out
write.csv(data_df, sprintf("data/prepped/ladph_covid_cases_prepped_%s.csv", str_remove_all(datestamp, "-")), row.names = F)
