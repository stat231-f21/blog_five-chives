library(rvest)
library(purrr)
library(tidyverse)
library(fulltext)

#read in csv's of citations (split iinto 4 csvs)
set1 <- read_csv("csv-mortalityT-set.csv")
set2 <- read_csv("csv-mortalityT-set-2.csv")
set3 <- read_csv("csv-mortalityT-set-3.csv")
set4 <- read_csv("csv-mortalityT-set-4.csv")

#combine and clean up data
citations <- set1 %>% 
  rbind(set2, set3, set4) %>% 
  janitor::clean_names() %>% 
  arrange(desc(publication_year)) %>% 
  mutate(url = NA)

#define number of citations
n_cit <- nrow(citations)

#create urls for each citation
for (x in 1:n_cit) {
  string <- toString(citations[x, 1])
  citations[x, 12] <- paste0("https://pubmed.ncbi.nlm.nih.gov/", string, "/")
}

#create empty column for abstracts
citations <- citations %>% 
  mutate(abstract = NA)

#populate dataframe with abstracts
#NOTE: code takes a LONG time to run
for (x in 1:37076) {
  parag <- citations$url[x] %>% 
    read_html() %>% 
    html_elements("#enc-abstract > p") %>% 
    html_text()
  paragraph <- paste(parag, collapse = "")
  citations$abstract[x] <- paragraph
}

# #check to make sure all citations have associated abstract
# check <- citations %>% 
#   filter(is.na(abstract))

#write csv
write_csv(citations, "abstract_list.csv")

