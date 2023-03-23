################################################################################
##
## [ PROJ ] < Problem set #10 >
## [ FILE ] < ps10 >
## [ AUTH ] < Hui Huang >
## [ INIT ] < 03/20/2023 >
##
################################################################################

## ---------------------------
## libraries
library(tidyverse)
## ---------------------------

## ---------------------------
## directory paths
data_dir <- file.path('.', 'data')
## ---------------------------

## -----------------------------------------------------------------------------
## Part II: Parsing event locations
#2.1 load data
events <- readRDS(url('https://github.com/anyone-can-cook/rclass2/raw/main/data/ps10_events.RDS'))

#2.2
print(events$address)
writeLines(events$address)

#2.3
str_view(string = events$address, 
         pattern = "([a-zA-Z0-9\\s]*\\S)\\n([a-zA-Z\\s]+)\\,\\s{1}([A-Z]{2})\\s{1}(\\d{5})")

#2.4
loc_matches <- str_match(string = events$address, 
          pattern = "([a-zA-Z0-9\\s]*\\S)\\n([a-zA-Z\\s]+)\\,\\s{1}([A-Z]{2})\\s{1}(\\d{5})")

#2.5
events <- events %>% mutate(event_address = loc_matches[, 2],
                  event_city = loc_matches[, 3], 
                  event_state = loc_matches[, 4], 
                  event_zip = loc_matches[, 5], 
                  ) 
## Part III: Parsing event date and time
#3.1
str_view(string = events$date,
         pattern = "(\\d+)/(\\d+)/(\\d+)")
str_match(string = events$date,
          pattern = "(\\d+)/(\\d+)/(\\d+)")

#3.2
str_replace(string = events$date, pattern = "(\\d+)/(\\d+)/(\\d+)",
            replacement = "20\\3-\\1-\\2")
events <- events %>% mutate(event_date = str_replace(string = events$date, pattern = "(\\d+)/(\\d+)/(\\d+)",
                                                     replacement = "20\\3-\\1-\\2") )

#3.3
time_matches <-str_match(string = events$time,
                        pattern = "(\\d+):(\\d+)\\s([AP]M)")

#3.4
events <- events %>% mutate(hour = as.double(time_matches[, 2]),
                            minute = time_matches[, 3], 
                            ampm = time_matches[, 4], 
                           ) 

#3.5
events <- events %>% mutate(hour24 = if_else(ampm == "PM", hour+12, hour) )

#3.6
events$hour24 <- str_pad(events$hour24, 2, "left", "0")

#3.7
events <- events %>% mutate(event_time = str_c(hour24, minute, "00", sep = ":"),
                  event_datetime = str_c(event_date, event_time, sep = " ")
                 ) 

#3.8
results <- events %>% select(event_datetime, event_date, event_time, event_location, event_address, event_city, event_state, event_zip)

#3.9
write_csv(results, file.path(data_dir, 'events_hh.csv'))








## -----------------------------------------------------------------------------



## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
