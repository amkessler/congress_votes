library(tidyverse)
library(janitor)
library(httr)
library(jsonlite)
library(curl)
options(scipen = 999)
options(stringsAsFactors = FALSE)



#### get member information ####

ppapi_download_memberinfo <- function(congress_session, chamber) {
  
  myurl <- paste0("https://api.propublica.org/congress/v1/", congress_session, "/", chamber, "/members.json")
  
  get_memberinfo <- GET(myurl, 
                        add_headers(`X-API-Key` = Sys.getenv("PROPUBLICA_API_KEY"))) #key stored as environ variable
  this.raw.content <- rawToChar(get_memberinfo$content)
  this.content <- fromJSON(this.raw.content)
  #dataframe from just the 3 content 
  content3_df <- as.data.frame(this.content[[3]])
  #unnest
  result_memberlist <- content3_df %>%
    unnest()

  return(result_memberlist)
  
}
