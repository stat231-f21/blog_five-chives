# Load packages 
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(urbnmapr)
library(ggplot2)
library(RColorBrewer)
library(ggiraph)
library(naniar)
library(sf)
library(leaflet)
library(dplyr)
library(geojsonio)
library(maptools)
library(tmap)

# Import data on variables of interest
var_data <- read_csv("final_data.csv") %>%
  mutate("crude_rate" = Total/ (population / 100000)) %>%
  rename("median_income" = "ln_hh_inc_y",
         "total_deaths" = "Total",
         "carbon_monoxide" = "ln_co_x",
         "particulate_matter" = "pm10_x",
         "vice_businesses" = "al_pwn_gm_env_rate_ln_x", 
         "education_businesses" = "ed_env_rate_ln_y",
         "service_agencies" = "ss_env_rate_ln_y",
         "healthcare_businesses" = "hc_env_rate_ln_x",
         "commute_time" = "commute_time_y",
         "fungicide" = "fungicide_ln_y",
         "herbicide" = "herbicide_ln_y",
         "insecticide" = "insecticide_ln_y",
         "pct_unemp_total" = "pct_unemp_total_y",
         "pct_poverty" = "pct_fam_pov_y",
         "gini_coefficient" = "gini_est_y")

# Join variable data with county data from urbanmapr package
county_names <- urbnmapr::counties %>%
  unite("county_state", c(county_name, state_abbv), sep = ", ") %>%
  mutate(county_join = tolower(county_state))

county_data <- left_join(var_data, county_names, by = "county_join")

write_csv(county_data, "county_data.csv")

# #Create map data
# county_map <- maps::map("county", regions = ".", plot = FALSE, fill = TRUE) %>% 
#   st_as_sf() %>%
#   rename("county" = ID)
# 
# head(county_map)
# 
# state_map <- maps::map("state", regions = ".", plot = FALSE, fill = TRUE) %>% 
#   st_as_sf() 

