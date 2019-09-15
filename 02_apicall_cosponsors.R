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
saveRDS(result_memberlist, "processed_data/result_memberlist_116th.rds")


### ***if want to skip above, just load saved version here ** ####
result_memberlist <- readRDS("processed_data/result_memberlist_116th.rds")



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
saveRDS(result_cosponsors, "processed_data/result_cosponsors_hr1296.rds")


## ** to avoid re-downloading we can also load from saved version ** ###
result_cosponsors <- readRDS("processed_data/result_cosponsors_hr1296.rds")



#### look for members that are NOT co-sponsors ##### ---------------

glimpse(result_memberlist)

#pull just house democrats
house_dems <- result_memberlist %>% 
  select(id, first_name, middle_name, last_name, party, state, district, geoid, fec_candidate_id) %>% 
  filter(party == "D",
         !state %in% c("VI", "GU", "MP", "DC", "PR")) 

#see if any repeated IDs
house_dems %>% 
  count(id) %>% 
  filter(n > 1)

#see if any repeated districts
house_dems %>% 
  count(state, district) %>% 
  filter(n > 1)


#only democrats and rename id to ease join
dem_bill_cosponsors <- result_cosponsors %>% 
  filter(cosponsor_party == "D") %>% 
  rename(id = cosponsor_id)

#see if any repeated IDs
dem_bill_cosponsors %>% 
  count(id) %>% 
  filter(n > 1)

### do the anti-join
nonsponsors <- anti_join(house_dems, dem_bill_cosponsors)

#remove id of sponsor him/herself, who shouldn't be among the nonsponsor table
sponsor_of_bill <- unique(dem_bill_cosponsors$sponsor_id)

nonsponsors <- nonsponsors %>% 
  filter(id != sponsor_of_bill)







#create a new column for house_dist matching with census/elex data ####
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


