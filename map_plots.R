#Load necessary packages
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

# Import data on variables of interest as well as county geography
county_data <- read_csv("county_data.csv")

# Create choices for states of interest
statenames <- read_csv("states.csv") %>% 
  janitor::clean_names()

state_choice_values <- c("ALL", unique(statenames$state))

# Create choices for variables of interest
var_choice_values <- c("total_deaths", "carbon_monoxide", "particulate_matter", 
                       "vice_businesses", "education_businesses", "service_agencies", 
                       "healthcare_businesses", "commute_time", "fungicide", 
                       "herbicide", "insecticide", "pct_unemp_total", "pct_poverty", 
                       "gini_coefficient")
var_choice_names <- c("Total Deaths", "Carbon Monoxide", "Particulate Matter", 
                      "Vice-Related Businesses", "Education-Related Businesses", 
                      "Service Agencies", "Healthcare-Related Businesses", 
                      "Commute Time", "Fungicides Applied", "Herbicides Applied",
                      "Insecticides Applied", "Percent Unemployed", 
                      "Percent in Poverty",
                      "Income Inequality")
names(var_choice_values) <- var_choice_names

############
#    ui    #
############
ui <- navbarPage(
  
  title = "Environmental Indices and Socioeconomic Factors For Counties in the U.S.",
  
  tabPanel(
    title = "Interactive Map Display",
    
    sidebarLayout(
      
      sidebarPanel(
        # Choose variables of interest
        selectInput(inputId = "index",
                    label = "Choose environmental index of choice:",
                    choices = var_choice_values,
                    selected = "pct_poverty"),
        
        # Choose states of interest to display
        selectInput(inputId = "state",
                    label = "Choose a state of interest:",
                    choices = state_choice_values,
                    selected = "Massachusetts")
      ),
      
      mainPanel(plotOutput(outputId = "mapvar"))
    )
  )

)
############
# server   #
############
server <- function(input, output) {
  
# Create data for map reacting to state filter
  data_for_map <- reactive({
    data <- {if (input$state != "ALL") filter(county_data, county_data$state_name == input$state)
      else county_data}
  })
  
  
# Create Interactive Map
  output$mapvar <- renderPlot({
    ggplot(data = data_for_map(), mapping = aes_string(x = "long", y = "lat", group = "group", 
                                                       fill = input$index)) +
      layer(data = data_for_map(), 
            geom = "polygon", stat = "identity", position = "identity") +
      geom_polygon(color = NA) +
      scale_fill_gradientn(colours = terrain.colors(10)) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      theme(legend.position="bottom") +
      labs(fill = var_choice_names[var_choice_values == input$index],
             title = paste(var_choice_names[var_choice_values == input$index], "For Counties in the US")) 
  })
}

## CHANGE AESTHETICS LATER ON 


####################
# call to Shiny App #
####################
shinyApp(ui = ui, server = server)

