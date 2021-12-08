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
library(mathjaxr)
library(sjPlot)

# Import data on variables of interest
county_data <- read_csv("county_data.csv") 

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
    
    mainPanel(
      tags$b("Compute parameters in R:"),
      verbatimTextOutput("summary"),
      br(),
      br(),
      tags$b("Regression plot:"),
      uiOutput(outputId = "results"),
      plotlyOutput(outputId = "regression"),
      br(),
      br(),
      tags$b("Interpretation:"),
      uiOutput(outputId = "interpretation")
    )
    
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
  
  # Create summary output of a simple linear regression
  output$summary <- renderPrint({summary(lm1())})
  
  # Pull out and print results from SLR output
  output$results <- renderUI({
    withMathJax(
      paste0(
        "Adj. \\( R^2 = \\) ", round(summary(lm1())$adj.r.squared, 3),
        ", \\( \\beta_0 = \\) ", round(lm1()$coef[[1]], 3),
        ", \\( \\beta_1 = \\) ", round(lm1()$coef[[2]], 3),
        ", P-value ", "\\( = \\) ", signif(summary(lm1())$coef[2, 4], 3)
      )
    )
  })
  
  # Generate regression plot
  output$regression <- renderPlotly({
    r <- ggplot(county_data, aes_string(x = input$predictor, y = input$response)) +
      geom_point(alpha = 0.5) +
      stat_smooth(method = "lm", se = TRUE) +
      ylab(slrvar_choice_names[slrvar_choice_values == input$response]) +
      xlab(slrvar_choice_names[slrvar_choice_values == input$predictor]) +
      theme_minimal()
    
    ggplotly(r)
  }) 
  
  # Draw conclusions and print out interpretation
  output$interpretation <- renderUI({
    if (summary(lm1())$coefficients[1, 4] < 0.05 & 
        summary(lm1())$coefficients[2, 4] < 0.05) {
      withMathJax(
        paste0("*Make sure the assumptions for linear regression (independance, 
               linearity, normality and homoscedasticity) are met before 
               interpreting the coefficients."),
        br(),
        br(),
        paste0("For a (hypothetical) value of ", input$predictor, 
               " = 0, the mean of ", input$response, " = ", 
               round(lm1()$coef[[1]], 3), "."),
        br(),
        br(),
        paste0("For an increase of one unit of ", input$predictor, ", ", 
               input$response, ifelse(round(lm1()$coef[[2]], 3) >= 0, 
                                      " increases (on average) by ", 
                                      " decreases (on average) by "), 
               abs(round(lm1()$coef[[2]], 3)), ifelse(abs(round(lm1()$coef[[2]], 3)) >= 2, 
                                                      " units", " unit"), ".")
      )
    } else if (summary(lm1())$coefficients[1, 4] < 0.05 & 
               summary(lm1())$coefficients[2, 4] >= 0.05) {
      withMathJax(
        paste0("*Make sure the assumptions for linear regression (independance, 
               linearity, normality and homoscedasticity) are met before 
               interpreting the coefficients."),
        br(),
        br(),
        paste0("For a (hypothetical) value of ", input$predictor, " = 0, 
               the mean of ", input$response, " = ", round(lm1()$coef[[1]], 3), "."),
        br(),
        br(),
        paste0("\\( \\beta_1 \\)", " is not significantly different from 0 (p-value = ", 
               round(summary(lm1())$coefficients[2, 4], 3), ") so there is no significant 
               relationship between ", input$predictor, " and ", input$response, ".")
      )
    } else if (summary(fit)$coefficients[1, 4] >= 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
      withMathJax(
        paste0("*Make sure the assumptions for linear regression (independance, 
               linearity, normality and homoscedasticity) are met before 
               interpreting the coefficients."),
        br(),
        br(),
        paste0("\\( \\beta_0 \\)", " is not significantly different from 0 (p-value = ", 
               round(summary(lm1())$coefficients[1, 4], 3), ") so when ", input$predictor, 
               " = 0, the mean of ", input$response, " is not significantly different from 0."),
        br(),
        br(),
        paste0("For an increase of one unit of ", input$predictor, ", ", 
               input$response, ifelse(round(lm1()$coef[[2]], 3) >= 0, 
                                      " increases (on average) by ", 
                                      " decreases (on average) by "), 
               abs(round(lm1()$coef[[2]], 3)), ifelse(abs(round(lm1()$coef[[2]], 3)) >= 2,
                                                      " units", " unit"), ".")
      )
    } else {
      withMathJax(
        paste0("*Make sure the assumptions for linear regression (independance, 
               linearity, normality and homoscedasticity) are met before 
               interpreting the coefficients."),
        br(),
        br(),
        paste0("\\( \\beta_0 \\)", " and ", "\\( \\beta_1 \\)", " are not 
               significantly different from 0 (p-values = ", 
               round(summary(lm1())$coefficients[1, 4], 3), " and ", 
               round(summary(lm1())$coefficients[2, 4], 3), 
               ", respectively) so the mean of ", input$response, 
               " is not significantly different from 0.")
      )
    }
  })
  
}


####################
# call to Shiny App #
####################
shinyApp(ui = ui, server = server)





