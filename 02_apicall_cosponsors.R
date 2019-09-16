library(tidyverse)
library(janitor)
library(httr)
library(jsonlite)
library(curl)
options(scipen = 999)
options(stringsAsFactors = FALSE)

# source custom functions used below for pulling from PP's API
source("00_functions.R")


### to avoid making new api calls, we can used the previous saved versions here ####
### and then skip to the non-sponsor analysis section

# result_memberlist <- readRDS("processed_data/result_memberlist_116th.rds")
# result_cosponsors <- readRDS("processed_data/result_cosponsors_hr1296.rds")

####################################################################################


### otherwise, we'll pull down info from the API here:

# (the functions below found in file 00)

#### GET MEMBER INFORMATION ####
result_memberlist <- ppapi_download_memberinfo("116", "house")

#see if any ids repeated
result_memberlist %>% 
  count(id) %>% 
  filter(n > 1)

#save result
saveRDS(result_memberlist, "processed_data/result_memberlist.rds")



#### SPECIFIC BILL - LIST OF COSPONSORS #### 
result_cosponsors <- ppapi_download_cosponsors("116", "hr1296")

#see if any ids repeated
result_cosponsors %>% 
  count(cosponsor_id) %>% 
  filter(n > 1)

#save results
saveRDS(result_cosponsors, "processed_data/result_cosponsors_hr1296.rds")



#### LOOK FOR MEMBERS WHO ARE *NOT* COSPONSORS ##### 
glimpse(result_memberlist)

#pull just house democrats (omit no-voting members)
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

# only democrat cosponsors and rename id to ease join
dem_bill_cosponsors <- result_cosponsors %>% 
  filter(cosponsor_party == "D",
         !cosponsor_state %in% c("VI", "GU", "MP", "DC", "PR", "AS")) %>% 
  rename(id = cosponsor_id)

#see if any repeated IDs
dem_bill_cosponsors %>% 
  count(id) %>% 
  filter(n > 1)

### do the anti-join
dem_bill_nonsponsors <- anti_join(house_dems, dem_bill_cosponsors)

#remove id of sponsor him/herself, who shouldn't be among the nonsponsor table
#remember when totaling up to 235 for checking, you'll need to add 1 to account for this
sponsor_of_bill <- unique(dem_bill_cosponsors$sponsor_id)

dem_bill_nonsponsors <- dem_bill_nonsponsors %>% 
  filter(id != sponsor_of_bill)




#### BRING IN THE CONGRESSIONAL DISTRICT PROFILE DATA (FROM RB PROJECT) #### -----------
workingtable <- readRDS("processed_data/workingtable.rds")

nonsponsors <- nonsponsors %>% 
  rename(GEOID = geoid)

#join
working_joined <- inner_join(nonsponsors, workingtable, by = "GEOID")

#save results
writexl::write_xlsx(working_joined, "output/working_joined_hr1296.xlsx")


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


working_joined %>% 
  count(p16winningparty, pct.ed.college.all.abovebelow.natl)



# how close were the 18 results?
working_joined %>% 
  select(
    live_D_pct,
    live_R_pct,
    live_winning,
    live_margin
  ) %>% 
  arrange(live_margin)




