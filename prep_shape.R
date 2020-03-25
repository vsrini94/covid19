## Script to Prep Shapefile
## Author: Patty Liu
## Date Created: 18 March 2020

rm(list = ls())
if(!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, stringr, maptools, sf, pdftools, splitstackshape)

# Cases reported by scrape (http://publichealth.lacounty.gov/media/Coronavirus/locations.htm) are listed by 
# City/Community within LA County. Cannot identify a shapefile with both city/community borders, so will use 
# Hierarchy listed here: and shapefile from

## Prep Location Hierarchy 
data <- pdf_data("data/hierarchy/ncovid19_regions_communities.pdf")
hf <- NULL # hierarchy-frame
for (i in 1:length(data)) {
  df <- data[[i]]
  ## Clean pdf_data (from Stackoverflow post: https://stackoverflow.com/questions/60127375/using-the-pdf-data-function-from-the-pdftools-package-efficiently)
  df <- df %>% 
    mutate(x = round(x/3), y = round(y/3)) %>% #reduce resolution to minimise inconsistent coordinates
    arrange(y, x) %>%                        #sort in reading order
    mutate(group = cumsum(!lag(space, default = 0))) %>%  #identify text with spaces and paste
    group_by(group) %>% 
    summarise(x = first(x),
              y = first(y),
              text = paste(text, collapse = " ")) %>% 
    group_by(y) %>% 
    mutate(colno = row_number()) %>%         #add column numbers for table data 
    ungroup() %>% 
    select(text, colno, y) %>% 
    pivot_wider(names_from = colno, values_from = text) 
  
  ## Clean further: looks good, but line issue when ID has multiple rows of subunits
  df <- df %>% data.table
  df[, y:=NULL]
  df <- df[!1:2,]
  names(df) <- c("id", "label", "subunit", "pop")
  
  ## Some rows are shifted over by two columns starting at the end of "Region: n) x"
  df[grepl(") ", label), `:=` (pop = subunit, subunit=tstrsplit(label, ") ")[[2]], label=tstrsplit(label, ") ")[[1]])]
  
  ## Fix when either label or subunit split across multiple lines
  df[grepl("--", id), `:=` (label = id, id = NA)]
  df[is.na(pop) & is.na(subunit) & is.na(label), `:=` (subunit = id, id=NA)]
  ## Concatenate
  df <- fill(df, id) %>% data.table ## Fill in ID by last non-NA value
  df[, label := paste(label[!is.na(label)], collapse=" "), by=id] # Concatenate by id
  df[, subunit := paste(subunit[!is.na(subunit)], collapse=" "), by=id] # Concatenate across subunit
  df <- df[!is.na(pop)] # Rows that were concatenated have pop = NA, drop these
  
  df[, label := tstrsplit(label, "--")[[1]]]

  hf <- rbind(hf, df) %>% data.table
}
 
## City = Los Angeles if subunit label has Los Angeles, otherwise City = name
hf[grepl("Los Angeles-", subunit), city := "Los Angeles"]
hf[is.na(city), city := label]

## Expand Units by delimiter ";"
hf <- cSplit(hf, "subunit", ";", "long")

## Create new column for unit type to try and match shapefile, clean subunit name
hf <- hf[grepl("Unincorporated-", subunit), `:=` (type = "unincorporated", subunit = gsub("Unincorporated-", "", subunit))]
hf <- hf[grepl("Los Angeles-", subunit), `:=` (type = "segment-of-a-city", subunit = gsub("Los Angeles-", "", subunit))]


#----------------

## Load shapefile
shp <- readShapePoly("data/shp/l.a. county neighborhood (v6).shp")

## Drop Orange County
shp <- subset(shp, county != "orange")

## Create temp frame to assess merge
sf <- shp@data %>% data.table
sf <- sf[, .(name, city, type)]
sf[, map.id := .I]

## Attempt to slot in shapefile in to the reporting hierarchy
hf <- merge(hf, sf, by.x="subunit", by.y="name", all.x=T)

