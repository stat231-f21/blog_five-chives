library(tidyverse)
library(tidytext)
library(wordcloud)
library(textdata)
library(kableExtra)
library(ggnetwork)
library(igraph)
library(shiny)


#read in list of abstracts
abstract_list <- read_csv("abstract_list.csv") %>% 
  mutate(abstract = str_replace(abstract, "\n", ""))

set.seed(83426)
theme_set(theme_classic())

######
#data wrangling
######

#unnest tokens (individual words) and retain year
abstract_words_all <- abstract_list %>%
  select(publication_year, abstract) %>% 
  unnest_tokens(output = word, input = abstract)

#words to keep 
empty <- data.frame(word = character())
keep_words <- empty %>% 
  add_row(word = c("disease", "age", "cancer", "women", "population", "hiv", "social", 
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

#words of interest
interest_words <- empty %>% 
  add_row(word = c("social", "disparities", "age", "genetic", "cancer", "community", 
                   "genetic", "cancer", "community", "genetic", "environmental", "family", 
                   "racial", "demographic", "economic", "exposure"))

#only keep stop words in data
abstract_words <- abstract_words_all %>% 
  right_join(keep_words, by = "word")

# #word count
# abstract_count <- abstract_words %>% 
#   count(word, sort = TRUE) 

######
#define widgets
######


######
#ui
######

ui <- navbarPage(
  
  title = "Abstracts", 
  
  tabPanel(
    title = "Count of top words",
    
    sidebarLayout(
      
      sidebarPanel(
        sliderInput("year_a", "Year:",
                    min = 1964, max = 2021,
                    value = 2010)), 
      
      mainPanel(plotOutput(outputId = "count")) 
    )
  ), 
  
  tabPanel(
    title = "Network",

    sidebarLayout(
      sidebarPanel(
        sliderInput("year_b", "Year:",
                   min = 1990, max = 2021,
                   value = 2010)
      ),
      mainPanel(plotOutput(outputId = "network"))
    )
  )
)

server <- function(input, output) {
  
  output$count <- renderPlot({
    abstract_words %>% 
      filter(publication_year == input$year_a) %>% 
      count(word, sort = TRUE) %>% 
      slice(1:10) %>%
      ggplot(aes(x = reorder(word, n), y = n, 
                 color = word, fill = word)) +
      geom_col() +
      coord_flip() +
      guides(color = "none", fill = "none") +
      labs(x = NULL,
           y = "Number of instances",
           title = "The most common words in research abstracts exploring mortality")
  })
  

  
  output$network <- renderPlot({
    #choose interest words from year
    network_words <- abstract_words %>% 
      filter(publication_year == input$year_b) %>% 
      filter(word %in% interest_words$word) %>% 
      count(word, sort = TRUE)
    #create dataframe
    df <- abstract_list %>%
      select(pmid, publication_year, abstract) %>%
      filter(publication_year == input$year_b) %>% 
      unnest_tokens(output = word, input = abstract) %>% #unnests words
      right_join(network_words, by = "word") %>% #only keeps interest words
      unique() %>% #remove repeats of connections in same abstract
        select(pmid, word) %>%
        table() %>%
        crossprod() #creates co-occurence matrix
    diag(df) <- 0 #sets connections between same word to 0
    df <- as.data.frame(df) 
    
    num_word <- ncol(df) 

    #define vertices and edges
    ve <- network_words 
    ed <- df %>%
      dplyr::mutate(from = rownames(.)) %>%
      tidyr::gather(to, weight, 1:num_word) %>% #gathers co-instances from matrix
      dplyr::mutate(weight = ifelse(weight == 0, NA, weight))

    #create igraph
    abst_igraph <- graph_from_data_frame(d = ed,
                                         vertices = ve,
                                         directed = FALSE) %>%
      simplify() #remove duplicate edges

    #plot network
    abst_network <- ggnetwork(abst_igraph)
    ggplot(data = abst_igraph, aes(x = x, y = y,
                                   xend = xend, yend = yend)) +
      geom_edges(aes(size = weight), color = "lightgray") +
      geom_nodes() +
      geom_nodelabel(aes(label = name, size = n), nudge_x = .01) +
      theme_blank()

  })
}

shinyApp(ui = ui, server = server)


################




