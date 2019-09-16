library(tidyverse)
library(janitor)
library(httr)
library(jsonlite)
library(curl)
options(scipen = 999)
options(stringsAsFactors = FALSE)



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
