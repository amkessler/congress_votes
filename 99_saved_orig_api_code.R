# this file contains saved code of original successful attempt at pulling
# data from Propublica API
# it is saved here for archive/referencing purposes

library(tidyverse)
library(janitor)
library(httr)
library(jsonlite)
library(curl)
options(scipen = 999)
options(stringsAsFactors = FALSE)


####get member information ####

get_memberinfo <- GET("https://api.propublica.org/congress/v1/116/house/members.json", 
                      add_headers(`X-API-Key` = Sys.getenv("PROPUBLICA_API_KEY"))) #key stored as environ variable

get_memberinfo$status_code

get_memberinfo$content
#convert from raw to characters
this.raw.content <- rawToChar(get_memberinfo$content)
#count how many characters
nchar(this.raw.content)
#look at first 100
substr(this.raw.content, 1, 100)
#parse this json
this.content <- fromJSON(this.raw.content)
#returns a list?
class(this.content)
#how long is the list
length(this.content)
this.content[[1]] #the first element - should be states if working
this.content[[3]] #the data itself, or so it appears to be
#dataframe from JUST the 3 content 
content3_df <- as.data.frame(this.content[[3]])
#inspect the list column named "members" for one record
str(content3_df$members[[1]], max.level = 1)
#unnest
z <- content3_df %>%
  unnest()
result_memberlist <- z

#see if any ids repeated
result_memberlist %>% 
  count(id) %>% 
  filter(n > 1)

#save result
# saveRDS(result_memberlist, "processed_data/result_memberlist_116th.rds")



#### SPECIFIC BILL - LIST OF COSPONSORS #### -----------------------

# https://projects.propublica.org/api-docs/congress-api/bills/#get-cosponsors-for-a-specific-bill

# here we'll pull HR1296
get_cosponsors <- GET("https://api.propublica.org/congress/v1/116/bills/hr1296/cosponsors.json", 
                      add_headers(`X-API-Key` = Sys.getenv("PROPUBLICA_API_KEY")))

get_cosponsors$status_code
get_cosponsors$content
#convert from raw to characters
this.raw.content <- rawToChar(get_cosponsors$content)
#count how many characters
nchar(this.raw.content)
#look at first 100
substr(this.raw.content, 1, 100)
#parse this json
this.content <- fromJSON(this.raw.content)
#returns a list?
class(this.content)
#how long is the list
length(this.content)
this.content[[1]] #the first element - should be states if working
this.content[[3]] #the data itself, or so it appears to be

content3_df <- as_tibble(this.content[[3]])
#inspect the list column nested several levels down
#in votes$vote$positions - where the full vote list located - for one record
str(content3_df$cosponsors[[1]], max.level = 1) 
#unnest it and save result
z <- content3_df %>%
  unnest(cosponsors)

result_cosponsors <- z

#save results
# saveRDS(result_cosponsors, "processed_data/result_cosponsors_hr1296.rds")
