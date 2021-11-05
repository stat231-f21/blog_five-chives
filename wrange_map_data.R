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
final_data <- read_csv("final_data.csv")

counties <- geojsonio::geojson_read("us-counties.json", what = "sp")

final_data <- final_data %>%
  mutate(GEO_ID = paste("0500000US", county_code, sep = ""))
final_data <- final_data %>%
  group_by(GEO_ID) %>%
  select(GEO_ID, county, ln_hh_inc_x, eqi_2jan2018_vc, Total, population, superfund)

counties@data <- left_join(counties@data, final_data, by = 
                                     c("GEO_ID" = "GEO_ID")) %>%
  unique(GEO_ID)

final_data_markers <- filter(final_data, 
                             "TRUE" %in% superfund)

counties_markers <- read_csv("uscounties.csv") %>%
  mutate(county = paste(county_full, state_id, sep = ", "))

counties_markers <- final_data_markers %>%
  left_join(counties_markers, by = "county")

#creates a fast and nice looking plot / lots of configuration available
pal <- colorQuantile("Spectral", counties$eqi_2jan2018_vc, n = 10)

labels <- sprintf(
  "<strong>%s</strong><br/>%g",
  counties$county, counties$eqi_2jan2018_vc
) %>% lapply(htmltools::HTML)

write_csv(counties_markers, "counties_markers.csv")
geojson_write(counties, "counties_map.json", what = "sp")

m <- leaflet(counties) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
  addPolygons(
    fillColor = ~pal(eqi_2jan2018_vc),
    opacity = 0.7,
    color = "#444444", 
    weight = 0.7, 
    smoothFactor = 0.5,
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "white",
      fillOpacity = 0.9,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addCircleMarkers(data = counties_markers, lng = counties_markers$lng,
             lat = counties_markers$lat, radius = 3,
             color = "black", label = paste("Superfund Site:", 
                                            counties_markers$county))

m
