library(tidyverse)
library(dplyr)
# The functions might be useful for A4
source("../source/a4-helpers.R")


#read in data for our calculation, dataframe name: incarceration_trends
incarceration_trends <- read.csv(
  file = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", 
  stringsAsFactors = FALSE
  )

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Following are the three values that I would like to calculated for Section 2: 
# total_jail_pop, female_jail_pop, black_jail_pop [] Complete: At least three values are included 
# For the sake of calculation, I will set two separate data frames: one set NA to zero, one ignore the NA values 
# This is the selected data set that only includes: total_jail_pop, female_jail_pop, and black_jail_pop
# In section 2, I will be going to answer three questions regarding the chosen value and discuss the inequalities
# [] Complete: Values clarify chosen variables related to patterns of inequality 
selected_values_df <- incarceration_trends %>%
  select(year, county_name, total_jail_pop, female_jail_pop, black_jail_pop)
# selected data frame that sets all NA values to zero 
selected_values_zero <- replace(selected_values_df, is.na(selected_values_df), 0)
# selected data frame that ignores all NA values
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
  select(county_name) %>%
  pull()
lowest_total_jail_pop <- selected_values_no_NA %>%
  filter(year == "2018") %>%
  filter(total_jail_pop == min(total_jail_pop)) %>%
  select(county_name) 

# Value 2: Highest/lowest of female_jail_pop in 2018
highest_female_jail_pop <- selected_values_no_NA %>%
  filter(year == "2018") %>%
  filter(female_jail_pop == max(female_jail_pop)) %>%
  select(county_name) %>%
  pull()
lowest_female_jail_pop <- selected_values_no_NA %>%
  filter(year == "2018") %>%
  filter(female_jail_pop == min(female_jail_pop)) %>%
  select(county_name) 

# Value 3: Highest/lowest of black_jail_pop
highest_black_jail_pop <- selected_values_no_NA %>%
  filter(year == "2018") %>%
  filter(black_jail_pop == max(black_jail_pop)) %>%
  select(county_name) %>%
  pull()
lowest_black_jail_pop <- selected_values_no_NA %>%
  filter(year == "2018") %>%
  filter(black_jail_pop == min(black_jail_pop)) %>%
  select(county_name) 

# Question 3: How much has my variables change over the last 20 years in each state?
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

## Section 3
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function return a data frame that is suitable for visualization. 
# It takes not parameters
# [] Complete: Chart: Data wrangling function: A function for data wrangling 
get_year_jail_pop <- function() {
  yearly_jail_pop <- incarceration_trends %>%
    group_by(year) %>%
    summarise(
      total_jail_pop_us = sum(total_pop)
    )
  return(yearly_jail_pop)   
}

# This function return the chart
# [] Complete: Chart: A plotting function: A function for creating the chart
plot_jail_pop_for_us <- function(yearly_jail_pop)  {
# [] Complete: Chart: (1) Labels, scales, and legends are clear (visually and labeling choices); and (2) A chart caption 
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
# [] Complete: Chart: The plotting function calls the data wrangling function
plot_grwoth_us <- plot_jail_pop_for_us(df_for_plot_grwoth)
plot_grwoth_us

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
states = c("NY", "OR", "CA", "AL")
# [] Complete: Chart: Data wrangling function: A function for data wrangling 
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

# [] Complete: Chart: A plotting function: A function for creating the chart
plot_jail_pop_by_states <- function(state_jail_pop){
# [] Complete: Chart: (1) Labels, scales, and legends are clear (visually and labeling choices); and (2) A chart caption 
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
# [] Complete: Chart: The plotting function calls the data wrangling function
growth_plot_state <- plot_jail_pop_by_states(states)
growth_plot_state

#----------------------------------------------------------------------------#
## Section 5  ---- 
#----------------------------------------------------------------------------#
# This section will be comparing the difference between female and male jail population 
# For the sake of calculation, I will replace all NA values in the original data frame to zero
df_NA_zero <- replace(incarceration_trends, is.na(incarceration_trends), 0)
# [] Complete: Chart: Data wrangling function: A function for data wrangling 
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

# [] Complete: Chart: A plotting function: A function for creating the chart
get_comparison_graph <- function(){
# [] Complete: Chart: (1) Labels, scales, and legends are clear (visually and labeling choices); and (2) A chart caption 
# [] Complete: A concise and clear section heading (<variable comparison that reveals potential patterns of inequality>) 
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

# [] Complete: Chart: The plotting function calls the data wrangling function
plot_comparison <- get_comparison_graph()
plot_comparison

#----------------------------------------------------------------------------#
## Section 6  ---- 
#----------------------------------------------------------------------------#
# [] Complete: Map: Data wrangling function: A function for data wrangling 
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
# Set all NA in the data set to zero 
df_for_map <- replace(df_for_map, is.na(df_for_map), 0)
# Change al the state abbriviation in the data set to lowercase state name 
df_for_map$state <- tolower(state.name[match(df_for_map$state, state.abb)])

# State map that joins the data frame we will be using to create the map
state_shape <- map_data("state") %>%
  group_by(region) %>%
  rename(state = region) %>%
  left_join(df_for_map, by = "state")

# Define a minimalist theme for maps
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

# [] Complete: Map: A plotting function: A function for creating the chart
get_balck_ratio_map <- function(){
# [] Complete: Chart: (1) Labels, scales, and legends are clear (visually and labeling choices); and (2) A chart caption 
# [] Complete: A concise and clear section heading (<a map shows potential patterns of inequality that vary geographically>) 
  labels <- labs(
    title = "Balck Jail Population Ratio Map (2018)",
    caption = "This is a map that hows the black jail population ratio
               in different states. Darker color represents a higher 
               ratio of black indivisuals in jail, while lighter color
               represents a lower ratio of black people in jail.") 
  
  ratio_map <- ggplot(state_shape) +
    geom_polygon(
      mapping = aes(long, lat, group = group, fill = black_jail_ratio)
    ) + 
    scale_fill_continuous(low = "cadetblue1", high = "darkblue") + labels + blank_theme
}
# [] Complete: Map: The plotting function calls the data wrangling function
ratio_state_map <- get_balck_ratio_map()
ratio_state_map
#----------------------------------------------------------------------------#

