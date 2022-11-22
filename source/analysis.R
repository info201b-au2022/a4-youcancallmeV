library(tidyverse)
library(dplyr)
# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

#read in data for our calculation, dataframe name: incarceration_trends
incarceration_trends_web <- read.csv(
  file = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", 
  stringsAsFactors = FALSE
  )
#read from download[delete later]
incarceration_trends <- read.csv(
  file = "~/Desktop/incarceration_trends.csv",
  stringsAsFactors = FALSE
)

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Following are the three values that I would like to calculated for Section 2: total_jail_pop, female_jail_pop, black_jail_pop
# For the sake of calculation, I will set two separate data frames: one set NA to zero, one ignore the NA values 
selected_values_df <- incarceration_trends %>%
  select(year, county_name, total_jail_pop, female_jail_pop, black_jail_pop)

selected_values_zero <- replace(selected_values_df, is.na(selected_values_df), 0)

selected_values_no_NA <- selected_values_df[complete.cases(selected_values_df), ]

# Question 1: What is the average value of my variable across all the counties (in a given year)?
# For this question, I will be using the one set to zero for the sake of calculation 
# Since the unit of these variables are 0.01,the calculation will round to two digits
average_values_all_counties <- selected_values_zero %>%
  group_by(year) %>%
  summarise(
    average_total_jail_pop = round(mean(total_jail_pop), digits = 2),
    average_female_jail_pop = round(mean(female_jail_pop), digits = 2),
    average_black_jail_pop = round(mean(black_jail_pop), digits = 2)
  )

# Question 2: Where is my variable the highest or lowest in 2018? 
# For these calculations, I will be using the data frame that ignores NA, and we will be specifying year 2018  

# Value 1: Highest/lowest of total_jail_pop in 2018
highest_total_jail_pop <- selected_values_no_NA %>%
  filter(year == "2018") %>%
  filter(total_jail_pop == max(total_jail_pop)) %>%
  select(county_name) 
lowest_total_jail_pop <- selected_values_no_NA %>%
  filter(year == "2018") %>%
  filter(total_jail_pop == min(total_jail_pop)) %>%
  select(county_name) 

# Highest/lowest of female_jail_pop in 2018
highest_female_jail_pop <- selected_values_no_NA %>%
  filter(year == "2018") %>%
  filter(female_jail_pop == max(female_jail_pop)) %>%
  select(county_name) 
lowest_female_jail_pop <- selected_values_no_NA %>%
  filter(year == "2018") %>%
  filter(female_jail_pop == min(female_jail_pop)) %>%
  select(county_name) 

# Highest/lowest of black_jail_pop
highest_black_jail_pop <- selected_values_no_NA %>%
  filter(year == "2018") %>%
  filter(black_jail_pop == max(black_jail_pop)) %>%
  select(county_name) 
lowest_black_jail_pop <- selected_values_no_NA %>%
  filter(year == "2018") %>%
  filter(black_jail_pop == min(black_jail_pop)) %>%
  select(county_name) 

#  How much has my variable change over the last 20 years?
# Value 1: total_jail_pop of the whole us 
values_20_years <- selected_values_zero %>%
  filter(year == "1998") %>%
  summarise(
    all_total_jail_pop = sum(total_jail_pop),
    all_female_jail_pop = sum(female_jail_pop),
    all_black_jail_pop = sum(black_jail_pop)
  )

values_now <- selected_values_zero %>%
  filter(year == max(year)) %>%
  summarise(
    all_total_jail_pop = sum(total_jail_pop),
    all_female_jail_pop = sum(female_jail_pop),
    all_black_jail_pop = sum(black_jail_pop)
  )

change_20_years <- values_now - values_20_years
#----------------------------------------------------------------------------#

## Section 3  ---- ???????
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function return a data frame that is suitable for visualization. 
# It takes not parameters
get_year_jail_pop <- function() {
  yearly_jail_pop <- incarceration_trends %>%
    group_by(year) %>%
    summarise(
      total_jail_pop_us = sum(total_pop)
    )
  return(yearly_jail_pop)   
}

# This function return the chart
plot_jail_pop_for_us <- function(yearly_jail_pop)  {
  labels <- labs(
    title = "Growth of the U.S. Prison Population",
    caption = "This is a bar gaph that shows the growth of U.S. prison population from 1970 to 2018.",
    x = "Year",
    y = "Total Jail Population",
    alt = "Growth of the U.S. Prison Population") 
  
  yearly_plot <- ggplot(
    data = yearly_jail_pop, 
    mapping = aes(x= year, y = total_jail_pop_us)) +
    geom_bar(stat = "identity") + labels
  return(yearly_plot)   
} 

df_for_plot_grwoth <- get_year_jail_pop()
plot_grwoth_us <- plot_jail_pop_for_us(df_for_plot_grwoth)
plot_grwoth_us

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
states = c("NY", "OR", "CA", "AL")
get_jail_pop_by_states <- function(states){
  state_jail_pop <- incarceration_trends %>%
    filter(state %in% states) %>%
    select(state, county_name, year, total_pop) %>%
    group_by(state, year) %>%
    summarise(
      state,
      year,
      total_state_pop = sum(total_pop)
    )
  return(unique(state_jail_pop))
}

df_for_plot_state <- get_jail_pop_by_states(states)

plot_jail_pop_by_states <- function(state_jail_pop){
  labels <- labs(
    title = "Growth of Prison Population by State",
    caption = "This is a bar gaph that shows the growth of 
               U.S. prison population from 1970 to 2018 
               based on four states: Alabama, California, 
               Oregon and Washington",
    x = "Year",
    y = "Total Jail Population",
    alt = "Growth of Prison Population by State") 
  state_chart <- ggplot(
    data = df_for_plot_state, 
    mapping = aes(x = year, y = total_state_pop, group = state)) +
    geom_line(
      aes(color = state)
    ) + labels 
}
growth_plot_state <- plot_jail_pop_by_states(states)
growth_plot_state

#----------------------------------------------------------------------------#
## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
df_NA_zero <- replace(incarceration_trends, is.na(incarceration_trends), 0)

get_comparison_df <- function(){
  comparison_df <- df_NA_zero %>%
    group_by(state) %>%
    filter(year == "2018") %>%
    summarise(
      male_jail_pop_us = sum(male_jail_pop),
      female_jail_pop_us = sum(female_jail_pop)
    ) 
  return(comparison_df)
}

df_for_comparison <- get_comparison_df()
df_for_comparison <- replace(df_for_comparison, is.na(df_for_comparison), 0)

get_comparison_graph <- function(){
  labels <- labs(
    title = "Female v.s Male Jail Population (2018)",
    caption = "This graph display the jail population of male 
               and female in 2018. Male jail population is 
               represented with blue bars while female 
               population is represented with purple bars.",
    x = "States",
    y = "Jail Population",
    alt = "Female v.s Male Jail Population (2018)") 
  
  comparison_graph <- ggplot(data = df_for_comparison) +
    geom_col(
      aes(x = state, y = male_jail_pop_us), 
      color = "white",
      fill = "cadetblue1") + 
    geom_col(
      aes(x = state, y = female_jail_pop_us), 
      color = "white",
      fill = "darkorchid1") + labels
}

plot_comparison <- get_comparison_graph()
plot_comparison

#----------------------------------------------------------------------------#
## Section 6  ---- 
#----------------------------------------------------------------------------#
get_balck_jail_df <- function(){
  black_jail_df <- df_NA_zero %>%
    group_by(state) %>%
    filter(year == "2018") %>%
    summarise(
      black_jail_pop_us = sum(black_jail_pop),
      all_jail_pop_us = sum(total_jail_pop)
    ) %>%
    mutate(black_jail_ratio = black_jail_pop_us/all_jail_pop_us)
  return(black_jail_df)
}

df_for_map <- get_balck_jail_df()
df_for_map <- replace(df_for_map, is.na(df_for_map), 0)

df_for_map$state <- tolower(state.name[match(df_for_map$state, state.abb)])

state_shape <- map_data("state") %>%
  group_by(region) %>%
  rename(state = region) %>%
  left_join(df_for_map, by = "state")

get_balck_ratio_map <- function(){
  labels <- labs(
    title = "Balck Jail Population Ratio Map (2018)",
    caption = ".....") 
  
  ratio_map <-ggplot(state_shape) +
    geom_polygon(
      mapping = aes(long, lat, group = group, fill = black_jail_ratio),
      color = "azure4"
    ) + 
    scale_fill_continuous(low = "cadetblue1", high = "darkblue") + labels
}

ratio_state_map <- get_balck_ratio_map()
ratio_state_map
#----------------------------------------------------------------------------#

