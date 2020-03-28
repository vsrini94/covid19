## Script to scrape daily report data released by LADPH on reported cases of covid 19 in the city
## Author: Vinay Srinivasan
## Date Created: 18 March 2020

rm(list = ls())

## Package management
if(!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, tidyverse, rvest, stringr, assertable)

## Functions

## SCRAPABLE
is_scrapable <- function(page.obj) {
  
  # page is scrapable if contains tabulated list of cases by location. alhambra and we ho present from first
  # tabulated release so this will be litmus test
  page_text <- html_nodes(page.obj, css = "ul") %>% html_text()
  scrapable <- any(grepl("Alhambra.*West Hollywood", page_text))
  message(sprintf("PAGE SCRAPABLE: %s", scrapable))
  
  return(scrapable)
}

## NEVER SCRAPED
never_scraped <- function(url, prev.scraped.urls){
  
  unscraped <- !(url %in% prev.scraped.urls)
  message(sprintf("NEVER SCRAPED: %s", unscraped))
  
  return(unscraped)
}

## Scrape Media releases
url <- "http://publichealth.lacounty.gov/media/Coronavirus/"
page <- read_html(url)
releases <- html_nodes(page, css = "a.purple-link") 
releases_df <- data.table(text = html_text(releases), links = html_attr(releases, "href"))

# Keep only the media releases that potentially contain covid data: have pattern prid
releases_df <- releases_df[links %like% "prid"]

## March 16 (prid = 2268) was first release that has cases broken down by location so drop
## all releases prior to that
releases_df[, prid := str_extract(links, "(?<=prid=)(\\d{4})") %>% as.numeric]
releases_df <- releases_df[prid >= 2268]

## Extract release dates from press release title
releases_df[, date := str_extract(text, "March [[:digit:]]{1,2},.* 2020|April [[:digit:]]{1,2},.* 2020")]
releases_df[, date := as.Date(date, format = "%B %d, %Y")]

## Loop through and scrape pages

## Load in already scraped data, so we're only scraping new releases
scraped_urls <- import_files(list.files("data/releases", pattern = "scraped", full.names = T)) %>% .[, unique(url)]

for (release in 1:nrow(releases_df)){
  
  release_row <- releases_df[release,]
  message(sprintf("Attempting to scrape %s release", release_row$date))
  
  page.obj <- read_html(release_row$links)
  
  if(is_scrapable(page.obj) & never_scraped(release_row$links, scraped_urls)){
    
    # Select chunk corresponding to reported cases
    cases <- html_nodes(page.obj, css = "ul") %>% html_text
    cases <- grep(cases, pattern = "Hollywood", value = T) %>% trimws
    
    # Hacky fix for 3/16 report
    if(release_row$prid == 2268) cases <- str_replace(cases, "1<   City", "1  Culver City")
    
    # Hacky fix for 3/16 and 3/18 reports
    if(release_row$prid %in% c(2268,2272)){
      cases <- str_replace_all(cases, "(?<=\\d)( *?)(?=[[A-Z]])", "SPLIT") %>% str_split(., "SPLIT") # To match spaces between city elements and make fixed width
      
    } else {
      cases <- str_replace_all(cases, "(?<=\\d)( *?)(?=[[A-Z],\\-?])", "SPLIT") %>% str_split(., "SPLIT") # To match spaces between city elements and make fixed width
    }
    
    city_case_splitter <- if (all(grepl("--", unlist(cases)))) "--" else if (all(grepl("\\\t", unlist(cases)))) "\\\t" else "--|\\\t"
    
    cases <- tstrsplit(unlist(cases), city_case_splitter, names = c("city", "n_cases")) %>% as.data.table
    
    # Clean up data frame
    cases[, city := str_remove(city, "\\\t")][, city := str_remove(city, "[^[:alpha:][:space:]]")]
    cases[, n_cases := str_remove(n_cases, "[^[:digit:]]")]
    cases[, names(cases) := lapply(.SD, trimws, which = "both"), .SDcols = names(cases)]
    cases[, n_cases := as.integer(n_cases)]
    cases[, date_reported := release_row$date]
    cases[, url := release_row$links]
    
    write.csv(cases, sprintf("data/ladph_covid_cases_scraped_%s.csv", str_remove_all(release_row$date, "-")), row.names = F)
    
    ## Try not to get blacklisted
    Sys.sleep(sample(10, 1))
    
  } else {
    
    message("Moving to next url")
  }
}

## Report for 3/21 needs separate scrape because slightly different formatting
if(never_scraped(releases_df[prid == 2275, links], scraped_urls)){
  
  page.obj <- read_html(release_row$links)
  cases <- html_nodes(page.obj, css = "li") %>% html_text
  cases <- cases[grep("Alhambra", cases):grep("Under Investigation", cases)]
  cases <- str_extract(cases, "^.*[:digit:]+")
  cases <- tstrsplit(cases, "\t", names = c("city", "n_cases")) %>% as.data.table
  cases[, n_cases := str_remove_all(n_cases, "[^[:digit:]]")]
  cases[, city := str_remove_all(city, "[^[:alpha:][:space:]]")]
  cases[, names(cases) := lapply(.SD, trimws, which = "both"), .SDcols = names(cases)]
  cases[, n_cases := as.integer(n_cases)]
  cases[, date_reported := releases_df[prid == 2275, date]]
  cases[, url := release_row[prid == 2275, links]]
  
  write.csv(cases, sprintf("data/releases/ladph_covid_cases_scraped_%s.csv", str_remove_all(unique(cases$date_reported), "-")), row.names = F)
  
}