library(tidyverse)
library(tidytext)
library(textdata)
library(kableExtra)
library(gganimate)
library(gifski)

abstract_words <- read_csv("data/abstract_words.csv") %>% 
  mutate(abstract = str_replace(abstract, "\n", ""))

theme_set(theme_classic())

abstract_count <- abstract_words %>% 
  filter(publication_year >= 1985) %>% #earlier years insufficient data
  group_by(publication_year) %>% 
  count(word, sort = TRUE) %>% #calculate counts
  slice(1:10) %>% 
  mutate(rank = rank(-n)) %>% #add rank 
  ungroup()

anim <- ggplot(abstract_count, aes(rank, group = word, 
                                   fill = as.factor(word), 
                                   color = as.factor(word))) + 
  geom_tile(aes(y = n/2,
                height = n,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(word, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = n, label = trunc(n), hjust = 0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.margin = margin(2, 2, 2, 2, "cm")
        ) +
  transition_states(publication_year, 
                    transition_length = 6, state_length = 2, wrap = FALSE) +
  view_follow(fixed_x = TRUE)  +
  ease_aes('sine-in-out') +
  labs(title = 'Words in research abstracts exploring mortality: {closest_state}',  
       subtitle  =  "Top 10 Words",
       caption  = "Data Source: PubMed") 

animate(anim, 400, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("data/gganim.gif"), end_pause = 15, start_pause =  15) 





