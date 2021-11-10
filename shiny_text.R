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

######
#data wrangling
######

#unnest tokens (individual words) and retain year
abstract_words_all <- abstract_list %>%
  select(publication_year, abstract) %>% 
  unnest_tokens(output = word, input = abstract)

#load in stop wrods from tidytext package
data(stop_words)

#words to keep 
keep_empty <- stop_words %>% 
  select(word) %>% 
  filter(is.na(word))
keep_words <- keep_empty %>% 
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
    #choose top ten words from year
    ten_words <- abstract_words %>% 
      filter(publication_year == input$year_b) %>% 
      count(word, sort = TRUE) %>% 
      slice(1:10) 
    #create dataframe
    df <- abstract_list %>%
      select(pmid, publication_year, abstract) %>%
      filter(publication_year == input$year_b) %>% 
      unnest_tokens(output = word, input = abstract) %>%
      right_join(ten_words, by = "word") %>%
      unique() %>%
        select(pmid, word) %>%
        table() %>%
        crossprod()
    diag(df) <- 0
    df <- as.data.frame(df)

    #define vertices and edges
    ve <- ten_words 
    ed <- df %>%
      dplyr::mutate(from = rownames(.)) %>%
      tidyr::gather(to, weight, 1:10) %>%
      dplyr::mutate(weight = ifelse(weight == 0, NA, weight))

    #create igraph
    abst_igraph <- graph_from_data_frame(d = ed,
                                         vertices = ve,
                                         directed = FALSE) %>%
      simplify()

    #plot network
    abst_network <- ggnetwork(abst_igraph)
    ggplot(data = abst_igraph, aes(x = x, y = y,
                                   xend = xend, yend = yend)) +
      geom_edges(color = "lightgray") +
      geom_nodes() +
      geom_nodelabel(aes(label = name, size = n), nudge_x = .01) +
      theme_blank()

  })
}

shinyApp(ui = ui, server = server)


#note: filter by year and top number of words!


