#Load packages 
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggplot2)
library(RColorBrewer)
library(ggiraph)
library(naniar)
library(dplyr)
library(plotly)

# Import data on variables of interest
county_data <- read_csv("county_data.csv")

# Create choices for variables of interest
linevar_choice_values <- c("total_deaths", "carbon_monoxide", "particulate_matter", 
                           "vice_businesses", "education_businesses", "service_agencies", 
                           "healthcare_businesses", "commute_time", "fungicide", 
                           "herbicide", "insecticide", "pct_unemp_total", "pct_poverty", 
                           "gini_coefficient", "ln_hh_inc_x", "eqi_2jan2018_vc")
linevar_choice_names <- c("Total Deaths", "Carbon Monoxide", "Particulate Matter", 
                          "Vice-Related Businesses", "Education-Related Businesses", 
                          "Service Agencies", "Healthcare-Related Businesses", 
                          "Commute Time", "Fungicides Applied", "Herbicides Applied",
                          "Insecticides Applied", "Percent Unemployed", 
                          "Percent in Poverty",
                          "Income Inequality", "Median Income", "EQI")
names(linevar_choice_values) <- linevar_choice_names

# For button to distinguish variables by color
linecolor_choice_values <- c("superfund", "na")
linecolor_choice_names <- c("National Priority Sites", "Don't differentiate points")
names(linecolor_choice_values) <- linecolor_choice_names

############
#    ui    #
############
ui <- fluidPage(
  
  titlePanel("Associations of Different Variables Differentiated by National Priority Sites"),
  
  sidebarLayout(
    sidebarPanel(
      # Choose variable 1 of interest
      selectInput(inputId = "index1",
                  label = "Choose index 1 of choice (x-axis):",
                  choices = linevar_choice_values,
                  selected = "fungicide"),
      
      # Choose variable 2 of interest
      selectInput(inputId = "index2",
                  label = "Choose index 2 of choice (y-axis):",
                  choices = linevar_choice_values,
                  selected = "total_deaths"),
      
      radioButtons(inputId = "pt_color",
                   label = "Color line graphs by:",
                   choices = linecolor_choice_values,
                   selected = NULL)
      
    ),
    
    mainPanel(plotlyOutput(outputId = "linegraphcolor"))
    
  )
)


############
# server   #
############
server <- function(input, output) {
  
  
  
  output$linegraphcolor <- renderPlotly({
    q <- ggplot(data = county_data, aes_string(x = input$index1, y = input$index2, 
                                               color = ifelse(
                                                 input$pt_color == "superfund", 
                                                 "superfund", FALSE)))+
      geom_smooth(methods = "loess") +
      labs(x = linevar_choice_names[linevar_choice_values == input$index1],
           y = linevar_choice_names[linevar_choice_values == input$index2],
           title = paste(linevar_choice_names[linevar_choice_values == input$index2], 
                         "by",
                         linevar_choice_names[linevar_choice_values == input$index1]),
           caption = "Data is collected from counties in the US. 
              Refer to EQI website for a dictionary on the variables.",
           color = linecolor_choice_names[linecolor_choice_values == input$pt_color]) +
      theme(legend.position = "bottom",
            plot.title.position = "plot",
            panel.background = element_rect(fill = "black"))
    
    ggplotly(q) %>%
      layout(legend = list(x = 0.1, y = 0.9),
             annotations = 
               list(x = 0.52, y = 0.6, text = "*NA refers to sites that are not superfund", 
                                showarrow = F, xref='paper', yref='paper', 
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=12, color="grey")))
    
  }
  )
}



####################
# call to Shiny App #
####################
shinyApp(ui = ui, server = server, options = list(height = 400, width = 600))





