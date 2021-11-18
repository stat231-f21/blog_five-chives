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

# Grabbing Superfund data
super_counties <- read.csv("counties_markers.csv")

# Grabbing counties map data
counties_all <- geojsonio::geojson_read("counties_map.geojson", what = "sp")
counties_df <- as.data.frame(counties_all)
counties_df <- counties_df %>%
  mutate(deaths = (Total / (population / 100000)))

row.names(counties_df) <- sapply(slot(counties_all, "polygons"), 
                                 function(x) slot(x, "ID"))
counties_all <- SpatialPolygonsDataFrame(counties_all, counties_df)

# Inputs
type_choice_names <- c("ln_hh_inc_x", "deaths", "eqi_2jan2018_vc", "insecticide_ln_x", "gini_est_x", "std_coal_prim_pop_ln_y","hc_env_rate_ln_x")
type_choice_values <- c("Median Income", "Mortality Rate", "EQI", "Insecticide Applied","Income Inequality", "Proportion of Coal Mines", "Healthcare-related Businesses")
#notes_choice_values <- c("From CDC data, log-transformed", "From CDC data, per 100,000 persons",
#                         "From EPA data, EQI (Environmental Quality Index), 2018 measurement")
names(type_choice_names) <- type_choice_values
#names(notes_choice_values) <- type_choice_values

#, "insecticide_ln_x", "gini_est_x", "std_coal_prim_pop_ln_y","hc_env_rate_ln_x"
# , "Insecticide Applied","Income Inequality", "Proportion of Coal Mines", "Healthcare-related Businesses"
# , "From PCA EPA data, Insecticide applied in pounds, log-transformed","From PCA EPA data, Measurement of income inequality in proportion",
# "From PCA EPA data, Mines per county population, log-transformed","From PCA EPA data, Rate of healthcare-related businesses, log-transformed")

# UI
ui <- 
                   fluidPage(titlePanel("Choropleth Map of Environmental/Health Factors with Superfund Sites"),
                       # Input variable they want displayed
                       selectInput(inputId = "typevar",
                                   label = "Choose a variable of interest to plot:",
                                   choices = type_choice_values,
                                   selected = "EQI"),
                       
                       # Input superfund sites if wanted
                       checkboxInput(inputId = "supervar", 
                                     label = "View Superfund sites", 
                                     value = FALSE, 
                                     width = NULL),
                     
                     mainPanel(leafletOutput(outputId = "map")))



server <- function(input, output, session){

  # Map output ggplot
  output$map <- renderLeaflet({
    
    leaflet(counties_all) %>%
      setView(-96, 37.8, 4) 
    })
  
  observeEvent(input$typevar, {
    counties_all@data$selected <- counties_all@data[, paste(type_choice_names[type_choice_values == input$typevar])]
    
    if(input$typevar != "Proportion of Coal Mines") {
      palette <- colorQuantile("RdYlGn", domain = counties_all@data$selected, n = 10)
      counties_all@data$color <- palette(counties_all@data$selected)
      
      pal_colors <- unique(palette(sort(counties_all@data$selected)))
      pal_labs <- quantile(counties_all@data$selected, seq(0, 1, .1), na.rm = T)
      pal_labs <- paste(round(lag(pal_labs), digits = 3), round(pal_labs, digits = 3), sep = " - ")[-1] 
    }
    
    if(input$typevar == "Proportion of Coal Mines") {
      palette <- colorBin("YlOrRd", domain = counties_all@data$selected, 2)
      counties_all@data$color <- palette(counties_all@data$selected)
      pal_colors <- unique(palette(sort(counties_all@data$selected)))
      pal_labs <- c(-Inf, unique(quantile(counties_all@data$selected, na.rm = T)), Inf)
      pal_labs <- paste(round(lag(pal_labs), digits = 3), round(pal_labs, digits = 3), sep = " - ")[-1] 
    }
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s",
      counties_all@data$COUNTY, counties_all@data$selected
    ) %>% lapply(htmltools::HTML)
    
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
      addPolygons(
        data = counties_all,
        fillColor = counties_all@data$color,
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
      addLegend(colors = pal_colors, labels = pal_labs, 
                position = "bottomright", title = input$typevar)
  })
    

      observe({
        if(!input$supervar) {
         leafletProxy("map") %>%
            clearMarkers() }
        else {
          leafletProxy("map") %>%
            clearMarkers() %>%
            addCircleMarkers(data = super_counties,
                           lng = super_counties$lng,
                             lat = super_counties$lat, 
                           radius = 3,
                             color = "black", 
                             label = paste("Superfund Site:", super_counties$county))
          }
        })
}

shinyApp(ui = ui, server = server)