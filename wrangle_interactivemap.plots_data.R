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


# Create data for shiny app. Use it for map and mutate it for plots.
county_data <- left_join(var_data, county_names, by = "county_join")

county_data <- county_data %>%
  select(-c("blood", "endocrine", "mental", "nervous",
            "circulatory", "respiratory", "digestive", "musculoskeletal", 
            "infectious", "neoplasms", "genitourinary", "perinatal", 
            "abnormal", "external", "skin", "congenital", "pregnancy"))

write_csv(county_data, "interactive_state_app/county_data.csv")

# Create data for scatter plots and density plots
scatter_data <- county_data %>%
  pivot_longer(-c("county", "stfips", "state", "county_name", "county_join", 
                  "county_code", "median_income", "superfund", 
                  "eqi_level", "income_level", "eqi", "income", "county_state.x", 
                  "state_name", "piece", "group", "county_fips", "county_state.y",
                  "state_fips", "fips_class"),
               names_to = "environ_characteristic",
               values_to = "value") %>%
  select(-c("stfips", "county_code", "county_state.x", "eqi_level", "income_level",
            "eqi", "income", "piece", "county_fips", "state_fips", "fips_class"))

write_csv(scatter_data, "interactive_state_app/scatter_data.csv")


