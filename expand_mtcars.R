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
library(htmltab)
library(openxlsx)

#### State fips location ####
# https://www2.census.gov/geo/docs/reference/codes/national_county.txt

#### Scripts for creating expanded mtcars data files ####

  #### Outputs a tidy version of mtcars, with weight in lbs.
    tidy_mtcars <- function(df = mtcars){
     tidy_mtcars <- df %>%
       tibble::rownames_to_column("model") %>%
       mutate(wt_lbs = wt * 1000,
              id = paste0("car_type_", row_number()))
     
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
                           pop_div = 10000,
                           seed_num = 314){
      
      set.seed(seed_num)
      
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
                         wt_sd = .05, mod_cut = 1.5,
                         seed_num = 314){
      
      set.seed(seed_num)
      
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
                            speed_lim_loc = "https://en.wikipedia.org/wiki/Speed_limits_in_the_United_States",
                            seed_num = 314){
      
      set.seed(seed_num)
      
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

    
    #### Fracture into individual files ####
    
    state_break <- function(df = long_output(),
                            seed_num){
      
      sed.seed(seed_num)
      
        print(names(df))
        states <- unique(df$state)

        state_list <- list()

        for(i in 1:length(states)){
          curstate <- paste0(states[[i]])
          state_list[[i]] <- df[which(df$state == curstate), ]
        }

        return(state_list)
      }

  #### Long form wrapper function ####
    # Outputs long form data set, pre fracturing
    long_output <- function(seed_num = 314){
      
      df <- state_rows(seed_num = seed_num) %>%
        add_mods(seed_num = seed_num) %>%
        add_tickets(seed_num = seed_num)
      
      return(df)
    }
    
  #### Function for determining how output will be setup ####
    output_master <- function(df = long_output(),
                              hold_outs = c("06"),
                              seed_num = 314){
      
      set.seed(seed_num)
      
      master_df <- df %>%
        select(fips, state) %>%
        filter(!(fips %in% hold_outs)) %>%
        distinct()
      
      return(master_df)  
    }
    
  #### Function for writing single .csvs ####
    write_singles <- function(df = long_output(),
                              master = output_master(),
                              seed_num = 314,
                              hold_outs = c("06")){
      
      set.seed(seed_num)
      
      master <- master %>%
        mutate(name_switch = sample(c(TRUE, FALSE), size = n(), replace = TRUE))
      
      for(i in 1:length(unique(master$name_switch))){
        
        temp <- master %>%
          filter(name_switch == unique(master$name_switch)[i])
        
        print(head(temp))
        
        for(j in 1:length(unique(temp$fips))){
          
          wt_random <- sample(c(TRUE, FALSE), size = 1)
          
          cur_fips_value <- unique(temp$fips)[j] 
          
          cur_temp <- df %>%
            filter(fips == cur_fips_value)
          
          print(head(cur_temp))

          if(wt_random == TRUE){
            cur_temp <- cur_temp %>%
              select(-wt)
          
          } else if(wt_random == FALSE){
            cur_temp <- cur_temp %>%
              select(-wt_lbs)
          }
                    
          if(unique(master$name_switch)[i] == TRUE){
            
            name_stem <- unique(cur_temp$state)
            cur_temp <- cur_temp %>%
              select(-fips, -state, -pop, -mod_flag)

            print(head(cur_temp))
            write.csv(cur_temp,
                      paste0("./output/", name_stem, ".csv"),
                      row.names = FALSE)
          }
          
          if(unique(master$name_switch)[i] == FALSE){
            
            name_stem <- unique(cur_temp$fips)
            cur_temp <- cur_temp %>%
              select(-state, -fips, -pop, -mod_flag)
            print(head(cur_temp))
            write.csv(cur_temp,
                      paste0("./output/", name_stem, ".csv"),
                      row.names = FALSE)
          }
        }
      }
    }
      
  #### Writes xlsx ####
    write_holdouts <- function(df = long_output(), hold_outs = c("06")){
      
      for(h in 1:length(hold_outs)){
        
        wb <- createWorkbook()
        
        temp <- df %>%
          filter(fips %in% hold_outs[h])
        
        ids <- unique(temp$id)
     
        for(i in 1:length(ids)){
          cur_rows <- temp %>%
            filter(id == ids[i])
          addWorksheet(wb, paste0(ids[i]))
          writeData(wb, paste0(ids[i]), cur_rows, rowNames = FALSE)
        }
        saveWorkbook(wb, paste0("./output/", hold_outs[h], ".xlsx"), overwrite = TRUE)
      }
    }
  
  #### Tidy mtcars for problem ####
    output_car_master <- function(df = tidy_mtcars()){
      
      df <- df %>%
        rename(car_type = id) %>%
        mutate(car_type = gsub("car_type_", "", car_type)) %>%
        select(-wt)
      
      write.csv(df, "./output/master_car_list.csv", row.names = FALSE)
    }
    
  #### Output wrapper ####
    mtcars_expanded_master <- function(){
      write_singles()
      write_holdouts()
      output_car_master()
    }