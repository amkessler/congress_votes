library(tidyverse)
library(janitor)
library(httr)
library(jsonlite)
library(curl)
options(scipen = 999)
options(stringsAsFactors = FALSE)

#To obtain your own API key and for other documentation: https://projects.propublica.org/api-docs/congress-api/

#### GET MEMBER INFORMATION ####

ppapi_download_memberinfo <- function(congress_session, chamber) {
  
  memberlist_url <- paste0("https://api.propublica.org/congress/v1/", congress_session, "/", chamber, "/members.json")
  
  get_memberinfo <- GET(memberlist_url, 
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



#### SPECIFIC BILL - LIST OF COSPONSORS #### 

ppapi_download_cosponsors <- function(congress_session, billnum) {
  
  cosponsor_url <- paste0("https://api.propublica.org/congress/v1/", congress_session, "/bills/", billnum, "/cosponsors.json")

  get_cosponsors <- GET(cosponsor_url, 
                        add_headers(`X-API-Key` = Sys.getenv("PROPUBLICA_API_KEY")))
  
  this.raw.content <- rawToChar(get_cosponsors$content)
  #parse this json
  this.content <- fromJSON(this.raw.content)
  content3_df <- as_tibble(this.content[[3]])
  #unnest
  result_cosponsors <- content3_df %>%
    unnest(cosponsors)
  
  return(result_cosponsors)

}




#### VOTEs ON A BILL #### 

# ENDPOINT STRUCTURE
# GET https://api.propublica.org/congress/v1/{congress}/{chamber}/sessions/{session-number}/votes/{roll-call-number}.json
# "https://api.propublica.org/congress/v1/115/senate/sessions/1/votes/17.json"

ppapi_download_rollcallvote <- function(congress_session, chamber, session, voteid) {
  
  votes_url <- paste0("https://api.propublica.org/congress/v1/", congress_session, "/", chamber, "/sessions/", session, "/votes/", voteid, ".json")

  get_rollcall <- GET(votes_url, 
                      add_headers(`X-API-Key` = Sys.getenv("PROPUBLICA_API_KEY")))
  
  this.raw.content <- rawToChar(get_rollcall$content)
  this.content <- fromJSON(this.raw.content)
  this.content[[3]] #the data itself, or so it appears to be
  content3_df <- as_tibble(this.content[[3]])
  result_rollcall <- content3_df$votes$vote$positions %>%
    unnest()
  
  #create a new column for house_dist matching with census/elex data
  distcorrect <- if_else(str_length(result_rollcall$district)==1,
                         paste0("0",result_rollcall$district),
                         result_rollcall$district)
  
  result_rollcall$distcorrect <- distcorrect
  result_rollcall$house_dist <- paste0(result_rollcall$state, "-", result_rollcall$distcorrect)
  
  result_rollcall <- result_rollcall %>% 
    select(-distcorrect, -dw_nominate)
  
  return(result_rollcall)
  
}

  
  
