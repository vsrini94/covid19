## Script to scrape daily report data released by LADPH on reported cases of covid 19 in the city
## Author: Vinay Srinivasan
## Date Created: 18 March 2020

rm(list = ls())

## Package management
if(!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, tidyverse, rvest, stringr)

## Next: Figure out how to ID and grab URLs from reported release

# For right now will do basic scrape for one page
url <- "http://publichealth.lacounty.gov/phcommon/public/media/mediapubhpdetail.cfm?prid=2272"
#url <- "http://www.publichealth.lacounty.gov/phcommon/public/media/mediapubhpdetail.cfm?prid=2271"

page <- read_html(url)

# Select chunk containing report date
release_date <- html_nodes(page, css = "td") %>% html_text
release_date <- grep(release_date, pattern="March [[:digit:]]{1,2}.*|April [[:digit:]]{1,2}.*", value = T) %>%
  str_extract(.,pattern = "March.*|April.*") %>% as.Date(., format = "%B %d, %Y")
  
# Select chunk corresponding to reported cases
cases <- html_nodes(page, css = "ul") %>% html_text
cases <- grep(cases, pattern = "Hollywood", value = T) %>% trimws
cases <- str_replace_all(cases, "(?<=\\d)( *?)(?=[A-Z])", "SPLIT") %>% str_split(., "SPLIT") # To match spaces between city elements and make fixed width
cases <- tstrsplit(unlist(cases), "--", names = c("city", "n_cases")) %>% as.data.table

# Clean up data frame
cases[, city := str_remove(city, "\\\t")]
cases[, names(cases) := lapply(.SD, trimws, which = "both"), .SDcols = names(cases)]
cases[, n_cases := as.integer(n_cases)]
cases[, date_reported := release_date]

p <- p[order(-n_cases)]


