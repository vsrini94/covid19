#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


pacman::p_load(data.table, tidyverse, 
               leaflet, maptools, sf, sp, mapview, 
               shiny, shinydashboard, shinythemes, shinyWidgets, DT,
               plotly, viridis)

source("../utilities.R")

## Read data
data_df <- import_files(list.files("../data/prepped/", full.names = T))
pop_df <- fread("../data/hierarchy/ncovid19_regions_communities.csv")[, .(region_number, population)]
shp_df <- readRDS("../data/shp/shp_clean.rds")
locs_df <- fread("../data/hierarchy/ncovid19_hierarchy_prepped.csv")
agg_id_df <- fread("../data/hierarchy/agg_ids.csv")

## Format data
setnames(data_df, c("location_number", "region_number"), c("location_id", "region_id"))
shp_df <- merge(data_df[date_reported %like% "28", .(location_id, n_cases)], shp_df, by = "location_id", all.y = T)
shp_df_sp <- SpatialPolygonsDataFrame(as(shp_df$geometry, "Spatial"), shp_df[, !"geometry"], match.ID = F)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$covidMap <- renderLeaflet({
        
        bins <- c(-2, quantile(shp_df_sp@data$n_cases, probs = seq(.25, .75, .25), na.rm = T), Inf)
        pal <- colorBin(viridis_pal()(length(bins)-1), domain = shp_df_sp@data$n_cases, bins = bins)
        labels <- sprintf(
            "<strong>%s</strong><br/>%g cases",
            shp_df_sp@data$location_label, shp_df_sp@data$n_cases
        ) %>% lapply(htmltools::HTML)
        
        leaflet() %>% setView(lat = 34.0642873, lng = -118.4445449, zoom = 10) %>% 
            addProviderTiles(providers$CartoDB.Positron) %>% addPolygons(data = shp_df_sp, 
                                                                         fillColor = ~pal(n_cases),
                                                                         weight = 2,
                                                                         opacity = 1,
                                                                         color = "blue",
                                                                         fillOpacity = .8,
                                                                         label = labels,
                                                                         labelOptions = labelOptions(
                                                                             style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                             textsize = "15px",
                                                                             direction = "auto")) %>% addLegend(data = shp_df_sp, position = "bottomleft", pal = pal, values = ~n_cases, opacity = .7)
        

    })

})
