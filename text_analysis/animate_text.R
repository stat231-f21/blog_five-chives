library(tidyverse)
library(tidytext)
library(textdata)
library(kableExtra)
library(gganimate)
library(gifski)

#read in data
abstract_words <- read_csv("data/abstract_words.csv") %>% 
  filter(word != "mortality") 

#set theme
theme_set(theme_classic())

###NOTE: change color assignment and highlight sociodemographic words of interest

#calculate count of words 
abstract_count <- abstract_words %>% 
  filter(publication_year >= 1985) %>% #earlier years insufficient data
  group_by(publication_year) %>% 
  count(word, sort = TRUE) %>% #calculate counts
  slice(1:10) %>% 
  mutate(rank = rank(-n), n = as.integer(n)) %>% #add rank 
  ungroup()

#create animation
anim <- ggplot(abstract_count, aes(rank, group = word, 
                                   fill = as.factor(word), 
                                   color = as.factor(word))) + 
  geom_tile(aes(y = n/2,
                height = n,
                width = 0.9), alpha = 0.8, color = NA) + #bars
  #horizontal axis label
  geom_text(aes(y = 0, label = paste(word, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = n, label = as.integer(n), hjust = 0)) + #labels of counts
  coord_flip(clip = "off", expand = FALSE) + #flip coordinates
  #set scales
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) + #remove legend
  #remove axes, add vertical gridlines
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none", #no legend
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.margin = margin(2, 2, 2, 2, "cm") #margins
        ) +
  #add transitioning for animation
  transition_states(publication_year, 
                    transition_length = 8, state_length = 4, wrap = FALSE) +
  view_follow(fixed_x = TRUE) + #fix x scale
  ease_aes('sine-in-out') + #smooth out animations
  labs(title = 'Words in research abstracts exploring mortality: {closest_state}',  
       subtitle  =  "Top 10 Words",
       caption  = "Data Source: PubMed") 

#create gif
animate(anim, 500, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("data/gganim.gif"), end_pause = 15, start_pause =  15) 

