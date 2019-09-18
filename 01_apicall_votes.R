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


result_rollcall <- ppapi_download_rollcallvote("116", "house", "1", "98")


write_csv(result_rollcall, "output/rollcallvote_98.csv")


