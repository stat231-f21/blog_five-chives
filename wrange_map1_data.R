library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(urbnmapr)
library(ggplot2)
library(RColorBrewer)
library(ggiraph)
library(naniar)
library(sf)
library(leaflet)
library(dplyr)
library(geojsonio)
library(maptools)
library(tmap)

# Grabbing final data
final_data <- read_csv("data/final_data.csv")

counties <- geojsonio::geojson_read("data/us-counties.json", what = "sp")

final_data <- final_data %>%
  mutate(GEO_ID = paste("0500000US", county_code, sep = ""))
final_data <- final_data %>%
  group_by(GEO_ID) %>%
  select(GEO_ID, county, ln_hh_inc_x, eqi_2jan2018_vc, Total, 
         population, superfund, insecticide_ln_x, gini_est_x, std_coal_prim_pop_ln_y,
         hc_env_rate_ln_x)

#"insecticide_ln_x", "gini_est_x", "std_coal_prim_pop_ln_y","hc_env_rate_ln_x"

counties@data <- left_join(counties@data, final_data, by = 
                                     c("GEO_ID" = "GEO_ID"))

counties@data <- counties@data[!duplicated(counties@data$GEO_ID), ]

final_data_markers <- filter(final_data, 
                             "TRUE" %in% superfund)

counties_markers <- read_csv("data/uscounties.csv") %>%
  mutate(county = paste(county_full, state_id, sep = ", "))

counties_markers <- final_data_markers %>%
  left_join(counties_markers, by = "county")

#creates a fast and nice looking plot / lots of configuration available
pal <- colorQuantile("Spectral", counties$eqi_2jan2018_vc, n = 10)

labels <- sprintf(
  "<strong>%s</strong><br/>%g",
  counties$county, counties$eqi_2jan2018_vc
) %>% lapply(htmltools::HTML)

counties_df <- as.data.frame(counties)
row.names(counties_df) <- sapply(slot(counties, "polygons"), 
                                 function(x) slot(x, "ID"))
counties <- SpatialPolygonsDataFrame(counties, counties_df)

write_csv(counties_markers, "leaflet_map_1/counties_markers.csv")
geojsonio::geojson_write(input = counties, file = "leaflet_map_1/counties_map.geojson",
                         what = "sp", overwrite = T)

# Example map
#m <- leaflet(counties) %>%
 # setView(-96, 37.8, 4) %>%
#  addProviderTiles("MapBox", options = providerTileOptions(
 #   id = "mapbox.light",
  #  accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
#  addPolygons(
 #   fillColor = ~pal(eqi_2jan2018_vc),
  #  opacity = 0.7,
  #  color = "#444444", 
  #  weight = 0.7, 
  #  smoothFactor = 0.5,
  #  fillOpacity = 0.7,
  #  highlightOptions = highlightOptions(
  #    weight = 2,
  #    color = "white",
   #   fillOpacity = 0.9,
  #    bringToFront = TRUE),
  #  label = labels,
  #  labelOptions = labelOptions(
  #    style = list("font-weight" = "normal", padding = "3px 8px"),
  #    textsize = "15px",
  #    direction = "auto")) %>%
  #addCircleMarkers(data = counties_markers, lng = counties_markers$lng,
  #           lat = counties_markers$lat, radius = 3,
  #           color = "black", 
  #           label = paste("Superfund Site:", counties_markers$county))
#m
