# Load packages 
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggplot2)
library(RColorBrewer)
library(ggiraph)
library(naniar)
library(dplyr)
library(mosaic)
library(plotly)
library(rmarkdown)
library(knitr)
library(pander)

# Import data on variables of interest
county_data <- read_csv("interactive_state_app/county_data.csv") 

# Create choices for variables of interest
slrvar_choice_values <- c("total_deaths", "carbon_monoxide", "particulate_matter", 
                           "vice_businesses", "education_businesses", "service_agencies", 
                           "healthcare_businesses", "commute_time", "fungicide", 
                           "herbicide", "insecticide", "pct_unemp_total", "pct_poverty", 
                           "gini_coefficient")
slrvar_choice_names <- c("Total Deaths", "Carbon Monoxide", "Particulate Matter", 
                          "Vice-Related Businesses", "Education-Related Businesses", 
                          "Service Agencies", "Healthcare-Related Businesses", 
                          "Commute Time", "Fungicides Applied", "Herbicides Applied",
                          "Insecticides Applied", "Percent Unemployed", 
                          "Percent in Poverty",
                          "Income Inequality")
names(slrvar_choice_values) <- slrvar_choice_names

############
#    ui    #
############
ui <- fluidPage(
  
  titlePanel("Simple Linear Regression of 2 Variables of Interest"),
  
  sidebarLayout(
    sidebarPanel(
      # Choose variable 1 of interest
      selectInput(inputId = "predictor",
                  label = "Choose predictor variable of choice (x-axis):",
                  choices = slrvar_choice_values,
                  selected = "fungicide"),
      
      # Choose variable 2 of interest
      selectInput(inputId = "response",
                  label = "Choose response variable of choice (y-axis):",
                  choices = slrvar_choice_values,
                  selected = "total_deaths")
      
    ),
    
    mainPanel(verbatimTextOutput("summary"),
              uiOutput(outputId = "results"),
              plotlyOutput(outputId = "regression"))
    
  )
)


############
# server   #
############
server <- function(input, output) {
  
  # Create reactive linear model formula
  lm1 <- reactive({
    lm(reformulate(input$predictor, input$response), data = county_data)
    })

  output$summary <- renderPrint({summary(lm1())})
  
  
  
}



####################
# call to Shiny App #
####################
shinyApp(ui = ui, server = server)





