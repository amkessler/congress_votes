library(tidyverse)
library(janitor)
library(httr)
library(jsonlite)
library(curl)
options(scipen = 999)
options(stringsAsFactors = FALSE)

source("00_functions.R")

#### specific vote call ####

# ENDPOINT STRUCTURE
# GET https://api.propublica.org/congress/v1/{congress}/{chamber}/sessions/{session-number}/votes/{roll-call-number}.json
# "https://api.propublica.org/congress/v1/115/senate/sessions/1/votes/17.json"


#set choices for the desired vote
congress <- "116"
chamber <- "house"
session_number <- "1"
roll_call_number <- "98"

#use stored function from step 00 to pull the results from PP's API
result_rollcall <- ppapi_download_rollcallvote(congress, 
                                               chamber, 
                                               session_number, 
                                               roll_call_number)


#save the result...

#generate file name to reflect vote details
filename <- str_c("output/rollcallvote_", chamber, "_", session_number, "_", roll_call_number, ".csv")
#write to csv
write_csv(result_rollcall, filename)


