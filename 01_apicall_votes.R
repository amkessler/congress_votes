library(tidyverse)
library(janitor)
library(httr)
library(jsonlite)
library(curl)
options(scipen = 999)
options(stringsAsFactors = FALSE)

# Sys.setenv(PROPUBLICA_API_KEY = <keygoeshere>)

####get member information ####

get_memberinfo <- GET("https://api.propublica.org/congress/v1/114/house/members.json", 
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
result <- z




#### specific vote call ####

# GET https://api.propublica.org/congress/v1/{congress}/{chamber}/sessions/{session-number}/votes/{roll-call-number}.json
# "https://api.propublica.org/congress/v1/115/senate/sessions/1/votes/17.json"

get_rollcall <- GET("https://api.propublica.org/congress/v1/116/house/sessions/1/votes/98.json", 
              add_headers(`X-API-Key` = Sys.getenv("PROPUBLICA_API_KEY")))

get_rollcall$status_code
get_rollcall$content
#convert from raw to characters
this.raw.content <- rawToChar(get_rollcall$content)
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
str(content3_df$votes$vote$positions[[1]], max.level = 1) 
#unnest it and save result
z <- content3_df$votes$vote$positions %>%
  unnest()
result <- z

write_csv(result, "output/rollcallvote_98.csv")

### next step, turning the above code into functions....


