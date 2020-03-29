## Utility functions for LA County COVID-19 Dashboard
## Author: Vinay Srinivasan
## Date Created: 28 March 2020

# data_df <- import_files(list.files("data/prepped", full.names = T))
# pop_df <- fread("data/hierarchy/ncovid19_regions_communities.csv")[, .(region_number, population)]

## Incident cases (over custom range)
## Requires data.table
calc_incident_cases <- function(data.df, start.date, end.date){
  
  inc_df <- data.df[date_reported %in% c(start.date, end.date)] %>% dcast(., ...~date_reported, value.var = "n_cases")
  inc_df[, incident_cases := get(end.date) - get(start.date)][, date_range := sprintf("%s to %s", start.date, end.date)]
  return(inc_df)
  
}

## Aggregate cases into covid regions: treat -1s as 2.5 (midpoint of 1-4 range); ignore NAs
## Requires data.table and assertable 
agg_cases <- function(data.df, agg.id.df = NULL, map = F){
  
  data_df <- copy(data.df) # preserves redacted and missing values in original data frame
  data_df[n_cases == -1, n_cases := 2.5][is.na(n_cases), n_cases := 0]
  
  if(map == T){
    
    data_df <- merge(data_df, agg.id.df)
    
  } else {
    
    data_df <- data_df[, .(n_cases = sum(n_cases)), by = .(region_number, region_label, date_reported)]
    
  }
  
  assert_values(data_df, "n_cases", test = "not_na")
  assert_ids(data_df, id_vars = list(region_number = c(1:171, 9999), date_reported = unique(data.df$date_reported)))
  
  return(data_df)
  
}

## Attack rate (Incident cases/susceptible population: assume recovered cases are not susceptible)
## For now can only do this for aggregate covid regions, since populations not available at more granular level

calc_attack_rate <- function(inc.cases, pop, start.date.cases) {
  return(inc.cases/(pop-start.date.cases))
}

# For shiny
# inc_df[, attack_rate := calc_attack_rate(incident_cases, population, get(substr(date_range, 1, 10)))]
# inc_df[, inc_rate := calc_incidence_rate(attack_rate, substr(date_range, 1, 10), substr(date_range, 15, 24)))]

## Incidence rate (calc incidence rate per 10k person-years)
calc_incidence_rate <- function(attack.rate, start.date, end.date){
  
  exp_yrs <- as.numeric(difftime(as.Date(end.date), as.Date(start.date), units = "days"))/365
  return(1e4*attack.rate/exp_yrs)
}
