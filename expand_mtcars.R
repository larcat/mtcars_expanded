#### Dependencies ####
library(dplyr)
library(readxl)
library(tidycensus)
library(tidyr)

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
                            states = state_pop_fetch()){
      
      states <- states %>%
        mutate(pop = round(pop / 10000, 0)) %>%
        uncount(pop)
      
      car_rows <- df %>%
        select(-model) %>%
        sample_n(df,
                 replace = TRUE,
                 size = nrow(states),
                 weight = mpg)
      
      final_df <- bind_cols(states, car_rows)
      
      return(final_df)
    }
    
  #### 
    
  