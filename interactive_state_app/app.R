#Load necessary packages
library(shiny)
library(tidyverse)
library(urbnmapr)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(dplyr)
library(stats)
library(graphics)

# Import data on variables of interest as well as county geography
county_data <- read_csv("county_data.csv")
scatter_data <- read_csv("scatter_data.csv")

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

# For button through which users can customize distinguishing variables by color
color_choice_values <- "superfund"
color_choice_names <- "National Priority Sites"
names(color_choice_values) <- color_choice_names

# Selectize counties
county_choices <- unique(scatter_data$county)

############
#    ui    #
############
ui <- fluidPage(
  
  titlePanel("Interactive Map by State"),
  
  sidebarLayout(
    sidebarPanel(
      # Choose variables of interest
      selectInput(inputId = "index",
                  label = "Choose environmental index of choice:",
                  choices = var_choice_values,
                  selected = "fungicide"),
      
      # Choose states of interest to display
      selectInput(inputId = "state",
                  label = "Choose a state of interest:",
                  choices = state_choice_values,
                  selected = "Texas"),
      
      # Choose to color points by presence of a super fund site or not
      radioButtons(inputId = "pt_color",
                   label = "Scatter plot is colored by:",
                   choices = color_choice_values,
                   selected = NULL),
      
      # Choose to view counties of interest in selected states
      selectizeInput(inputId = "county_name",
                     label = "Identify counties of interest in scatter plot:",
                     choices = NULL,
                     selected = NULL,
                     multiple = TRUE)
    ),
    
    mainPanel(plotlyOutput(outputId = "mapvar"),
              splitLayout(cellwidths = c("50%", "50%"), 
                          plotOutput(outputId = "scatter",
                                     click = "plot_click",
                                     dblclick = "plot_dblclick",
                                     hover = "plot_hover",
                                     brush = "plot_brush"
                          ),
                          plotOutput(outputId = "density",
                                     click = "density_click",
                                     dblclick = "density_dblclick",
                                     hover = "density_hover",
                                     brush = "density_brush")
              ),
              splitLayout(cellWidths = c("50%", "50%"),
                          verbatimTextOutput("scatter_info"),
                          verbatimTextOutput("density_info"))
              
              
    )
  )
)






############
# server   #
############
server <- function(input, output) {
  
  # Create data for map reacting to state filter
  
  data_for_map <- reactive({
    data <- {if (input$state != "ALL") 
      filter(county_data, county_data$state_name == input$state)
      else county_data}
  })
  
  # Create Interactive Map
  
  output$mapvar <- renderPlotly({
    m <-  ggplot(data = data_for_map(), mapping = aes_string(x = "long", 
                                                             y = "lat", 
                                                             group = "group", 
                                                             fill = input$index)) +
      layer(data = data_for_map(), 
            geom = "polygon", stat = "identity", position = "identity") +
      geom_polygon(color = NA) +
      scale_fill_gradientn(colours = terrain.colors(10)) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      theme(legend.position="bottom") +
      labs(x = "longitude",
           y = "latitude",
           fill = var_choice_names[var_choice_values == input$index],
           title = paste(var_choice_names[var_choice_values == input$index], 
                         "For ",
                         input$state)) +
      theme(panel.background = element_rect("black"))
    
    ggplotly(m)
    
  })
  
  # Create data for scatter plot reacting to filters
  # Will use this same reactive data set for density plot
  
  data_for_scatter <- reactive({
    if (input$state != "ALL") 
      filter(scatter_data, 
             scatter_data$state_name == input$state, 
             scatter_data$environ_characteristic == input$index)
    else filter (scatter_data, environ_characteristic %in% input$index)
  })
  
  observeEvent(data_for_scatter(), {
    county_state_choices <- unique(data_for_scatter()$county)
    updateSelectizeInput(inputId = "county_name", 
                         choices = county_state_choices, 
                         selected = NULL) 
  })
  
  # Create Interactive Scatter Plots 
  
  output$scatter <- renderPlot({
    ggplot(data = data_for_scatter(), aes_string(x = "median_income", y = "value", 
                                                 color = "superfund")) +
      geom_point(position = "jitter") +
      labs(x = "Median County Income",
           y = "Index Value",
           title = paste(var_choice_names[var_choice_values == input$index], 
                         "By Median Income"),
           subtitle = paste("For ", input$state),
           caption = "Colored by National Priority Sites.
                      NA refers to sites that are not superfund.
                      Data is collected from counties in the US. 
                      Refer to EQI website for a dictionary on the variables.",
           color = color_choice_names[color_choice_values]) +
      geom_label(data = filter(data_for_scatter(),
                               county %in% input$county_name),
                 aes(label = county), 
                 show.legend = FALSE
      ) +
      theme(legend.position = "bottom",
            plot.title.position = "plot",
            panel.grid = element_line(FALSE),
            panel.background = element_rect(fill = "black")) 
  })
  
  # Create Hover/ Click Interactivity for Scatter Plot 
  
  output$scatter_info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             "ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })
  
  
  # Create Interactive Density Plots
  
  output$density <- renderPlot({
    ggplot(data = data_for_scatter(), aes_string(x = "value")) +
      geom_density(fill = "red", alpha = 0.8) +
      #geom_vline(xintercept = mean("value"), color = "black") +
      labs(x = paste(var_choice_names[var_choice_values == input$index]),
           y = "Density",
           title = paste("Density Plot of ", 
                         var_choice_names[var_choice_values == input$index]),
           subtitle = paste("For ", input$state),
           caption = "Data is collected from counties in the US. 
           Refer to EQI website for a dictionary on the variables.") +
      theme(plot.title.position = "plot",
            panel.grid = element_line(FALSE),
            panel.background = element_rect("black"))
  })
  
  # Create Hover/ Click Interactivity for Density Plot
  
  output$density_info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             "ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0(
      "click: ", xy_str(input$density_click),
      "dblclick: ", xy_str(input$density_dblclick),
      "hover: ", xy_str(input$density_hover),
      "brush: ", xy_range_str(input$density_brush)
    )
  })
}


####################
# call to Shiny App #
####################
shinyApp(ui = ui, server = server)
