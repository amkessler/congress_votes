library(tidyverse)
library(janitor)
library(httr)
library(jsonlite)
library(curl)
options(scipen = 999)
options(stringsAsFactors = FALSE)



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
result_rollcall <- z


#create a new column for house_dist matching with census/elex data
head(result_rollcall)

distcorrect <- if_else(str_length(result_rollcall$district)==1,
        paste0("0",result_rollcall$district),
        result_rollcall$district)

result_rollcall$distcorrect <- distcorrect
result_rollcall$house_dist <- paste0(result_rollcall$state, "-", result_rollcall$distcorrect)

result_rollcall <- result_rollcall %>% 
  select(-distcorrect, -dw_nominate)

head(result_rollcall)

write_csv(result_rollcall, "output/rollcallvote_98.csv")

### next step, turning the above code into functions....


