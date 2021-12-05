library(tidyverse)
library(tidytext)
library(wordcloud)
library(textdata)
library(kableExtra)
library(ggnetwork)
library(igraph)
library(shiny)
library(dplyr)

#read in list of abstracts
abstract_words <- read_csv("abstract_words.csv") 

#set seed and theme
set.seed(83426)
theme_set(theme_classic())

######
#add categories
######


empty <- data.frame(word = character())
keep_words <- empty %>%
  add_row(word = c("mortality", "disease", "age", "cancer", "women", "population", "hiv", "social",
                   "clinical", "children", "infection", "protein", "genetic", "blood",
                   "cardiovascular", "community", "diabetes", "heart", "exposure",
                   "gene", "national", "drug", "chronic", "physical", "virus",
                   "hospital", "sex", "smoking", "therapy", "interventions", "white",
                   "genes", "family", "race", "weight", "education", "alcohol",
                   "breast", "maternal", "antibodies", "lung", "income", "disparities",
                   "environmental", "obesity", "tumor", "region", "african", "stroke",
                   "cardiac", "viral", "hypertension", "immune", "respiratory", "hispanic",
                   "food", "infections", "dna", "infant", "gender", "socioeconomic",
                   "history", "coronary", "racial", "demographic", "mental", "male",
                   "underlying", "child", "infants", "poor", "ethnic", "rural",
                   "antibody", "pulmonary", "renal", "policy", "efforts", "female",
                   "sexual", "depression", "behavioral", "influenza", "economic", "genome",
                   "urban", "ethnicity"))

interest_words <- empty %>%
  add_row(word = c("mortality", "social", "disparities", "cancer", "community",
                   "genetic", "environmental", "family",
                   "racial", "demographic", "economic", "exposure")) %>% 
  mutate(category = NA)
interest_words$category[interest_words$word %in% c("mortality", "cancer")] <- "mortality"
interest_words$category[interest_words$word %in% c("social", "disparities", "racial", "demographic", 
                                    "economic")] <- "sociodemographic"
interest_words$category[interest_words$word %in% c("community", "family")] <- "contextual"
interest_words$category[interest_words$word %in% c("genetic")] <- "genetic"
interest_words$category[interest_words$word %in% c("environmental", "exposure")] <- "exposure"

###THIS CODE WAS RUNNING INTO ERRORS
# interest_words <- interest_words %>% 
#   mutate(category = case_when(
#             word %in% c("mortality", "cancer") ~ "mortality", 
#             word %in% c("social", "disparities", "racial", "demographic", 
#                         "economic") ~ "sociodemographic", 
#             word %in% c("community", "family") ~ "contextual", 
#             word %in% c("genetic") ~ "genetic", 
#             word %in% c("environmental", "exposure"), ~ "exposure", 
#             TRUE ~ NA))

######
#define widgets
######


######
#ui
######

ui <- fluidPage(
  
  # tabPanel(
    titlePanel("Network of words used in research abstracts exploring mortality causes"),

    sidebarLayout(
      sidebarPanel(
        sliderInput("year_b", "Year:",
                   min = 1985, max = 2021, sep= "",
                   value = 2006), 
        selectizeInput(inputId = "interest_list", 
                       label = "Select words to include:", 
                       choices = keep_words$word, 
                       selected = interest_words$word,
                       multiple = TRUE
        )
      ),
      mainPanel(
        fluidRow(width = 12, 
                 plotOutput(outputId = "network")), 
        fluidRow(width = 5, 
                 tableOutput(outputId = "plot_info"))
        
      )
    # )
  )
)

######
#server
######

server <- function(input, output) {

  #words of interest not found in year
  interest_missing <- reactive({
    present <- abstract_words %>% 
      filter(publication_year == input$year_b) 
    missing <- setdiff(as.list(input$interest_list), as.list(present$word))
    if(length(missing) > 0) {data <- paste(missing, collapse = ", ")}
    else {data <- "NA"}
  })
  
  abst_igraph <- reactive({
    #choose interest words from year
    network_words <- abstract_words %>% 
      filter(publication_year == input$year_b) %>% 
      filter(word %in% input$interest_list) %>% 
      count(word, sort = TRUE) %>%  #get counts of words  
      left_join(interest_words, by = "word")
    #create dataframe
    df <- abstract_words %>%
      filter(publication_year == input$year_b) %>% #keeps only year of interest
      right_join(network_words, by = "word") %>% #only keeps interest words
      unique() %>% #remove repeats of connections in same abstract
      select(pmid, word) %>%
      table() %>%
      crossprod() #creates co-occurence matrix
    diag(df) <- 0 #sets connections between same word to 0
    df <- as.data.frame(df) #forces table to dataframe
    num_word <- ncol(df) #number of words = numbers of column
    
    #define vertices and edges
    ve <- network_words #interest words from years, including counts
    ed <- df %>%
      mutate(from = rownames(.)) %>% #create "from" w/ row names for gather function 
      #gathers co-occurences from matrix, column names to "to" column, number of co-occurences to "weight"
      tidyr::gather(to, weight, 1:num_word) %>% 
      mutate(weight = ifelse(weight == 0, NA, weight)) #weights of 0 to NA
    
    #create igraph
    graph_from_data_frame(d = ed,
                                         vertices = ve,
                                         directed = FALSE) %>%
      simplify() #remove duplicate edges
  })
  
  #network
  output$network <- renderPlot({
    set.seed(83426)
    
    #plot network
    abst_network <- ggnetwork(abst_igraph())
    ggplot(data = abst_network, aes(x = x, y = y,
                                   xend = xend, yend = yend)) +
      geom_edges(aes(size = weight), 
                    color = "lightgray", curvature = .1) +
      geom_nodes(aes(size = n, color = category), shape = 20)  +
      geom_nodetext_repel(aes(label = name, size = n), repel = TRUE, 
                    point.padding = unit(0.2, "lines"), color = "gray10") +
      ggraph::scale_edge_width(c(0.5, 5)) +
      # geom_nodes() +
      # geom_nodelabel(aes(label = name, size = n), nudge_x = .01) +
      theme_blank() +
      labs(size = "Number", color = "Category", 
           caption = paste("Words not found: ", interest_missing(), 
                           "\n Data source: PubMed")) 

  }, 
  width = 600, height = 400)
  
  output$plot_info <- renderText({
    abst_stat <- data.frame(name = vertex_attr(abst_igraph(), "name"),
                            strength = strength(abst_igraph(), 
                                                weights = edge_attr(abst_igraph(), "Weight"))) 
    abst_stat %>% 
      arrange(desc(strength)) %>% 
      head(5) %>% 
      knitr::kable("html", row.names = FALSE, col.names = c("Word", "Strength")) 
      
  })
}

shinyApp(ui = ui, server = server)


################

