#### Desc ####
# Street racers are a plague on the nation!
# Recently, many street racers have gotten speeding tickets in both modified and
# unmodified cars. Furthermore, cars impounded at street racing events have been
# weighed and put on the dyno.
# This script gives you info about those cars, their modifications and those 
# tickets.

#### Dependencies ####
library(dplyr)
library(readxl)
library(tidycensus)
library(tidyr)
library(XML)

#### State fips location ####
# https://www2.census.gov/geo/docs/reference/codes/national_county.txt

#### Scripts for creating expanded mtcars data files ####

  #### Outputs a tidy version of mtcars, with weight in lbs.
    tidy_mtcars <- function(df = mtcars){
     tidy_mtcars <- df %>%
       tibble::rownames_to_column("model") %>%
       mutate(wt_lbs = wt * 1000,
              id = row_number()) %>%
       select(-wt)
     
     row.names(tidy_mtcars) <- 1:nrow(mtcars)
     
    
     return(tidy_mtcars)  
    }
  
  #### Gets state populations from tidycensus ####
    # Requires API loaded via census_api_key() from tidycensus.
    state_pop_fetch <- function(){
      state_pops <- get_acs(geography = "state", 
                           variables = "B01003_001",
                           geometry = FALSE) %>%
        rename(fips = GEOID,
               pop = estimate,
               state = NAME) %>%
        select(fips, pop, state)
        
      return(state_pops)
    }
    
  #### Creates dataframe with nrows based on state pop
    state_rows <- function(df = tidy_mtcars(), 
                           states = state_pop_fetch(),
                           numrows = nrow(states),
                           weights = quo(mpg),
                           pop_div = 10000){
      
      states <- states %>%
        mutate(pop = round(pop / pop_div, 0),
               weights = pop) %>%
        uncount(weights)
      
      car_rows <- df %>%
        select(-model) %>%
        sample_n(df,
                 replace = TRUE,
                 size = nrow(states),
                 weight = !!weights)
      
      final_df <- bind_cols(states, car_rows)
      
      return(final_df)
    }
    
  #### Add modification noise to long states data frame ####
    # Creates random vairable, assigns mods according to that.
    # Increases hp, decreases weight.
    add_mods <- function(df, hp_mean = 1.2, hp_sd = .05, wt_mean = .9,
                         wt_sd = .05, mod_cut = 1.5){
      
      df <- df %>%
        mutate(mod_num = rnorm(n = n()),
               hp_num = rnorm(n = n(), mean = hp_mean, sd = hp_sd),
               wt_num = rnorm(n = n(), mean = wt_mean, sd = wt_sd),
               mod_flag = ifelse(mod_num <= mod_cut & mod_num >= mod_cut * -1, 
                                 FALSE, TRUE),
               hp = ifelse(mod_flag == TRUE, hp * hp_num, hp),
               wt_lbs = ifelse(mod_flag == TRUE, wt_lbs * wt_num, wt_lbs),
               hp = round(hp, 0),
               wt_lbs = round(wt_lbs, 0)) %>%
        select(-mod_num, -hp_num, -wt_num)
      
      return(df)
    }
  
  #### Add tickets to long states data frame ####
    # Pulls speed limits from wikipedia.
    # Imputs the ones with annoying embedded chars as the mode (70)
    # Selects ticketed cars based on hp/lb ratio
    # Imputes speed of ticket based on speed limit + hp/lb ratio
    # Imputes price of ticket based on num rows, rounded to 100
    add_tickets <- function(df, ticket_mean = 1000, ticket_sd = 100,
                            speed_lim_loc = "https://en.wikipedia.org/wiki/Speed_limits_in_the_United_States"){
      
      speed_lim_table <- htmltab(speed_lim_loc, 2) %>%
        select(`State or territory`,
               `Freeway (rural)`) %>%
        rename(state = `State or territory`,
               speed_lim = `Freeway (rural)`) %>%
        mutate(speed_lim = as.numeric(substr(speed_lim, 1, 2)),
               speed_lim = ifelse(is.na(speed_lim), 70, speed_lim))
      
      df <- df %>% left_join(speed_lim_table) %>%
        mutate(ratio = hp / wt_lbs,
               ticket_num = rnorm(n = n(), 
                                  mean = mean(ratio),
                                  sd = sd(ratio)),
               ticket_flag = ifelse(ticket_num < .6 * ratio, TRUE, FALSE),
               ticket_amount_num = rnorm(n = n(),
                                         mean = ticket_mean,
                                         sd = ticket_sd),
               ticket_amount = ifelse(ticket_flag == TRUE, 
                                      round(ticket_amount_num, 0),
                                      0)) %>%
        select(-ticket_num, -ticket_flag, - ticket_amount_num, -ratio)
      
      return(df)
    }
    
  #### Long form wrapper function ####
    # Outputs long form data set, pre fracturing
    long_output <- function(){
      df <- state_rows() %>%
        add_mods() %>%
        add_tickets()
      
      return(df)
    }