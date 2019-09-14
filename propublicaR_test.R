library(tidyverse)
library(janitor)
library(httr)
library(jsonlite)
library(curl)
library(ProPublicaR)
options(scipen = 999)
options(stringsAsFactors = FALSE)

my_api_key <- Sys.getenv("PROPUBLICA_API_KEY")

#pull list of all members of a congressional chamber
test <- list_members_chamber_congress(115, 'senate', my_api_key)

as.data.frame(test$results[[1]])
