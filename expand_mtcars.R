#### Dependencies ####
library(dplyr)
library(readxl)

#### Scripts for creating expanded mtcars data files ####
tidy_mtcars <- function(df = mtcars){
 tidy_mtcars <- df %>%
   tibble::rownames_to_column("model") %>%
   mutate(wt_lbs = wt * 1000)
 
 row.names(tidy_mtcars) <- 1:nrow(mtcars)

 return(tidy_mtcars)  
}