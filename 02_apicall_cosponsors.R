library(tidyverse)
library(janitor)
library(httr)
library(jsonlite)
library(curl)
options(scipen = 999)
options(stringsAsFactors = FALSE)


### to avoid making new api calls, we can used the previous saved versions here
result_memberlist <- readRDS("processed_data/result_memberlist_116th.rds")
result_cosponsors <- readRDS("processed_data/result_cosponsors_hr1296.rds")


### otherwise, we'll pull down info from the API here in these steps:

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




#### look for members that are NOT co-sponsors ##### ---------------

glimpse(result_memberlist)

#pull just house democrats
house_dems <- result_memberlist %>% 
  select(id, first_name, middle_name, last_name, party, state, district, geoid, fec_candidate_id) %>% 
  filter(party == "D",
         !state %in% c("VI", "GU", "MP", "DC", "PR", "AS")) 

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
  filter(cosponsor_party == "D",
         !cosponsor_state %in% c("VI", "GU", "MP", "DC", "PR", "AS")) %>% 
  rename(id = cosponsor_id)

#see if any repeated IDs
dem_bill_cosponsors %>% 
  count(id) %>% 
  filter(n > 1)

### do the anti-join
nonsponsors <- anti_join(house_dems, dem_bill_cosponsors)

#remove id of sponsor him/herself, who shouldn't be among the nonsponsor table
#remember when totaling up to 235 for checking, you'll need to add 1 to account for this
sponsor_of_bill <- unique(dem_bill_cosponsors$sponsor_id)
nonsponsors <- nonsponsors %>% 
  filter(id != sponsor_of_bill)



#### bring in the CD profile data #### --------------------------

workingtable <- readRDS("processed_data/workingtable.rds")

nonsponsors <- nonsponsors %>% 
  rename(GEOID = geoid)

#join
working_joined <- inner_join(nonsponsors, workingtable, by = "GEOID")


#analysis
glimpse(working_joined)

working_joined %>% 
  count(p16winningparty)

working_joined %>% 
  count(keyrace_rating)

working_joined %>% 
  count(flips)

working_joined %>% 
  count(pct.ed.college.all.abovebelow.natl)

working_joined %>% 
  count(medincome.abovebelow.natl)

working_joined %>% 
  count(pct.race.nonwhite.abovebelow.natl)


# how close were the 18 results?
working_joined %>% 
  select(
    live_D_pct,
    live_R_pct,
    live_winning,
    live_margin
  ) %>% 
  arrange(live_margin) %>% 
  View()




