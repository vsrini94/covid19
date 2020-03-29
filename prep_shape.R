## Script to Prep Shapefile
## Author: Patty Liu
## Date Created: 18 March 2020

rm(list = ls())
if(!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, rio, tidyverse, stringr, maptools, sf, sp, pdftools, splitstackshape, mapview)

# Cases reported by scrape (http://publichealth.lacounty.gov/media/Coronavirus/locations.htm) are listed by 
# City/Community within LA County. Cannot identify a shapefile with both city/community borders, so will use 
# Hierarchy listed here: and shapefile from

str.cap <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

#--Setup-----------------------------------------

## Load Location hierarchy
lf <- fread("data/hierarchy/ncovid19_hierarchy_prepped.csv")

## Load data
df <- fread("data/prepped/ladph_covid_cases_prepped_20200327.csv")

## Renames
setnames(df, c("location_number", "region_number"), c("location_id", "region_id"))

## Clean names
df[grepl("Unincorporated -", location_label), `:=` (type = "unincorporated-area", location_label = gsub("Unincorporated - ", "", location_label))]
df[grepl("Los Angeles -", location_label), `:=` (type = "segment-of-a-city", location_label = gsub("Los Angeles - ", "", location_label))]
df[is.na(type), type := "standalone-city"]

## Cap for merge
df[, location_label := toupper(location_label)]

## Drop "Under Investigation"
df <- df[location_id!=9999]

#--Combine LA City and County shapefiles----------

##Lets get LA County Borders
cty <- st_read("data/shp/l.a. county neighborhood (current).shp")  %>% data.table
cty[, location_label := toupper(name)]
cty <- cty[, .(location_label, type, geometry)]
## Drop LA City within this, will plan to swap with other shapefile 
cty <- cty[type!="segment-of-a-city", .(location_label, type, geometry)] ## Drop LA City
cty <- cty %>% st_sf

## Grab LA City Neighborhoods
la <- st_read("/Users/patrick/Downloads/LACITY_COMMUNITIES/LACITY_COMMUNITIES.shp") %>% data.table
la <- la[, `:=` (location_label=COMTY_NAME, type="segment-of-a-city")]
la <- la[, .(location_label, type, geometry)]
la <- la %>% st_sf
la <- st_transform(la, crs=4269) ## Conver to lat/long projection
la <- st_cast(la, "MULTIPOLYGON")

## Combine these two back to a single shapefile
shp <- rbind(cty, la)

## Merge
shp <- shp %>% data.table
shp[, shp_id := .I]

## Locations witout names
shp <- shp[!is.na(location_label)]

## Rename shapefile to Covid names
shp[location_label=="ALONDRA PARK", location_label := "EL CAMINO VILLAGE"] ## Rename Alondra Park -> El Camino Village
shp[location_label=="ANGELES CREST", location_label := "ANGELES NATIONAL FOREST"] 
shp[location_label=="CASTAIC CANYONS", location_label := "CASTAIC"] 
shp[location_label=="CHARTER OAK", location_label := "CHARTEROAK"]
shp[location_label=="EAST SAN GABRIEL", location_label := "NORTHEAST SAN GABRIEL"]
shp[location_label=="LOPEZ/KAGEL CANYONS", location_label := "KAGEL/LOPEZ CANYONS"]
shp[location_label=="SOUTH SAN JOSE HILLS", location_label := "SAN JOSE HILLS"]
shp[location_label=="UNINCORPORATED CATALINA ISLAND", location_label := "SANTA CATALINA ISLAND"]
shp[location_label=="UNINCORPORATED SANTA MONICA MOUNTAINS", location_label := "SANTA MONICA MOUNTAINS"]
shp[location_label=="VIEW PARK-WINDSOR HILLS", location_label := "VIEW PARK/WINDSOR HILLS"]
shp[location_label == "WEST WHITTIER-LOS NIETOS", location_label := "WEST WHITTIER/LOS NIETOS"]

## Merge to df
df <- merge(df, shp, by=c("location_label", "type"), all=T)

#--Merge------------------------------------------

## Aggregate shapes within a region_id if > 1 non-missing shape for that region

## Re-slot shapes into regions
df[location_label %in% c("ATHENS", "WESTMONT"), `:=` (region_id = 75, region_label="Athens-Westmont")]
df[shp_id == 161, `:=`(region_id = 144, region_label="San Fernando")] # San Fernando -> San Fernando
df[shp_id == 138, `:=`(region_id = 137, region_label="Studio City")] # Studio City -> Studio City
df[shp_id == 27,  `:=`(region_id = 13, region_label="Azusa") ] # Citrus -> Azusa
df[shp_id == 39,  `:=`(region_id = 63, region_label="Compton")] # East Compton -> Compton
df[shp_id == 51,  `:=`(region_id = 2, region_label="Castaic")] # Green Valley -> Castaic
df[shp_id == 53,  `:=`(region_id = 2, region_label="Castaic")]  # Hasley Canyon -> Castaic 
df[shp_id == 84,  `:=`(region_id = 14, region_label="Arcadia")] # Mayflower Village -> Arcadia
df[shp_id == 89,  `:=`(region_id = 7, region_label="Lake Los Angeles")]  # Northeast Antelope Valley -> Lake Los Angeles
df[shp_id == 90,  `:=`(region_id = 27, region_label="El Monte")] # North El Monte -> El Monte
df[shp_id == 91,  `:=`(region_id = 5, region_label="Lancaster")] # Northwest Antelope Valley -> Lancaster
df[shp_id == 92,  `:=`(region_id = 6, region_label="Palmdale")] # Northwest Palmdale -> Palmdale
df[shp_id == 102,  `:=`(region_id = 40, region_label="Walnut")] # Ramona -> Walnut
df[shp_id == 106,  `:=`(region_id = 2, region_label="Castaic")] # Ridge Route -> Castaic
df[shp_id == 121,   `:=`(region_id = 41, region_label="Diamond Bar")] # South Diamond Bar -> Diamond Bar  
df[shp_id == 132,   `:=`(region_id = 155, region_label="Pacific Palisades")] # Topanga -> Pacific Palisades
df[shp_id == 134,   `:=`(region_id = 141, region_label="Tujunga")] # Tujunga Canyon -> Tujunga
df[shp_id == 137,  `:=`(region_id = 4, region_label="Stevenson Ranch")] # Unincorp. Santa Susana Mountains -> Stevenson Ranch
df[shp_id == 142,  `:=`(region_id = 152, region_label="Brentwood")] # Veterans Administration -> Brentwood
df[shp_id == 144,  `:=`(region_id = 24, region_label="Covina")] # Vincent -> Covina 
df[shp_id == 148,  `:=`(region_id = 63, region_label="Compton")] # West Compton -> Compton
df[shp_id == 154,  `:=`(region_id = 19, region_label="San Dimas")] # West San Dimas -> San Dimas
df[shp_id == 312,  `:=`(region_id = 115, region_label="Highland Park")] # Arroyo View Estates -> Highland Park
df[shp_id == 307,  `:=`(region_id = 99, region_label="Crestview")] # Little Ethiopia-> Crestview
df[shp_id == 302,  `:=`(region_id = 80, region_label="San Pedro")] # Terminal Island -> San Pedro
df[shp_id == 197, `:=`(region_id = 137, region_label="Studio City")] # Universal City -> Studio City

df[region_id==23, region_id := 24] # Unincorporated Covina ->  Covina

## Regions where I'm going to aggregate shapefiles
df[, n := sum(!is.na(shp_id)), by=region_id]
ids <- df[is.na(shp_id) | is.na(location_id) & n >= 1]$region_id %>% unique
## Save these somewhere
export(df[region_id %in% ids, .(region_id, region_label, location_id, location_label)], "data/hierarchy/agg_ids.csv")

## Loop through region ids and aggregate the shapes within
for (id in ids) {
  to_merge <- df[region_id==id & !is.na(shp_id)]$shp_id
  region <- df[region_id==id]$region_label[[1]] %>% toupper
  n <- sum(df[region_id==id]$n_cases, na.omit=T)
  ## Create new aggregated shape
  agg <- df[shp_id %in% to_merge] %>% st_sf %>% st_combine %>% data.table
  names(agg) <- "geometry"
  agg[, `:=` (n_cases=n, region_id = id, region_label = region, location_id = id, location_label = region, shp_id = to_merge[[1]], type=NA)]
  ## Drop smaller units from main shapefile and rbind this back
  df <- df[region_id != id | is.na(region_id)]
  df <- rbind(df, agg, fill=T)
}

## Save shapefile
df <- df[, .(location_label, location_id, region_id, shp_id, geometry)]
export(df, "data/shp/shp_clean.rds")

