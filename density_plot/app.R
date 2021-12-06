library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(ggplot2)
library(RColorBrewer)
library(ggiraph)
library(dplyr)

total_df <- read_csv("final_data.csv")
type_choice_names <- c("ln_hh_inc_x", "Total / (population / 100000)", "eqi_2jan2018_vc", "insecticide_ln_x", "gini_est_x", "std_coal_prim_pop_ln_y","hc_env_rate_ln_x")
type_choice_values <- c("Median Income", "Mortality Rate", "EQI", "Insecticide Applied","Income Inequality", "Proportion of Coal Mines", "Healthcare-related Businesses")
names(type_choice_names) <- type_choice_values

super_data <- total_df %>%
    filter(superfund == "TRUE")
not_data <- total_df %>%
    filter(is.na(superfund))

ui <- fluidPage(
    
    titlePanel("Examining the Effects of Superfund Sites"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "typevar",
                        label = "Choose a variable of interest to plot:",
                        choices = type_choice_values,
                        selected = "Median Income")),
        mainPanel(plotOutput(outputId = "plot"))))

server <- function(input, output, session) {
    
    output$plot <- renderPlot({
        ggplot(total_df, aes(x = x)) + 
            geom_density(data = super_data, aes_string(x = type_choice_names[type_choice_values == input$typevar], 
                                                       y = "..density.."), fill="darkblue" ) +
            geom_density(data = not_data, aes_string(x = type_choice_names[type_choice_values == input$typevar], 
                                                     y = "-..density.."), fill = "lightblue") +
            xlab(input$typevar) +
            ylab("None Superfund vs. Superfund Sites")
    })
}

shinyApp(ui = ui, server = server)

