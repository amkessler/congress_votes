library(tidyverse)
library(janitor)
library(httr)
library(jsonlite)
library(curl)
library(summarytools)
options(scipen = 999)
options(stringsAsFactors = FALSE)

# source custom functions used below for pulling from PP's API
source("00_functions.R")


####################################################################################

### to avoid making new api calls after running code for first time ----
### we can used previously saved versions here 
### and then skip to the sponsor analysis section

# result_memberlist <- readRDS("processed_data/result_memberlist_116th.rds")

# result_cosponsors <- readRDS("processed_data/result_cosponsors_<bill_num>.rds")
# e.g.
# result_cosponsors <- readRDS("processed_data/result_cosponsors_hr1296.rds")

####################################################################################


### otherwise, we'll pull down info from the API here: ----

# (the functions below found in file 00)

#### GET MEMBER INFORMATION ####
result_memberlist <- ppapi_download_memberinfo("116", "house")

# any ids repeated?
result_memberlist %>% 
  count(id) %>% 
  filter(n > 1)

# see who repeats are, if any
dups <- result_memberlist %>% 
  count(id) %>% 
  filter(n > 1) %>% 
  pull(id)

result_memberlist %>% 
  filter(id %in% dups) 

#save member list results
saveRDS(result_memberlist, "processed_data/result_memberlist.rds")


#### SPECIFIC BILL - LIST OF COSPONSORS #### 

# set bill choice to pull
bill_num <- "hr1296"

# run function from step 00 to make API call, pull down the data
result_cosponsors <- ppapi_download_cosponsors("116", bill_num)

#see if any ids repeated
result_cosponsors %>% 
  count(cosponsor_id) %>% 
  filter(n > 1)


#save the result...

#generate file name to reflect vote details
filename2 <- str_c("processed_data/result_cosponsors_", bill_num, ".rds")
#write to rds
saveRDS(result_cosponsors, filename2)



#### DETERMINING THE NON-SPONSORS (DEMS ONLY) #### --------------------------


#### Look for members who are not sponsors ####
glimpse(result_memberlist)

#pull just house democrats (omit no-voting members)
house_dems <- result_memberlist %>% 
  select(id, first_name, middle_name, last_name, party, state, district, geoid, fec_candidate_id) %>% 
  filter(party == "D",
         !state %in% c("VI", "GU", "MP", "DC", "PR", "AS")) 

#save copies of the results
writexl::write_xlsx(house_dems, "processed_data/house_dems_all.xlsx")
saveRDS(house_dems, "processed_data/house_dems_all.rds")

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

#check to see who was pulled out of consponsors table because
#they were either from a non-voting district or a Republican
result_cosponsors %>% 
  filter(cosponsor_party == "R" |
         cosponsor_state %in% c("VI", "GU", "MP", "DC", "PR", "AS")) %>% 
  select(bill_slug, cosponsor_id, name, cosponsor_state, cosponsor_party) 


## Now let's build a single table that includes cosponsors, sponsor and nonsponsors ####

#start with data of voting democratic caucus
house_dems

#add a new column that we'll use to identify their position
house_dems <- house_dems %>% 
  mutate(
    position = "None listed"
  ) %>% 
  select(position, everything())

#now we'll split up and remerge using vector of IDs
cosponsor_ids_vector <- dem_bill_cosponsors %>% 
  pull(id)

zcospons <- house_dems %>% 
  filter(id %in% cosponsor_ids_vector) %>% 
  mutate(position = "cosponsor")

znospons <- house_dems %>% 
  filter(!id %in% cosponsor_ids_vector) %>% 
  mutate(
    position = case_when(
      id == sponsor_of_bill ~ "sponsor", #using the saved sponsor id from above
      TRUE ~ "not sponsor"
    )
  )

house_dems_wspons <- bind_rows(zcospons, znospons)

#now we'll add a field that makes a binary choice between sponsor/cosponsor Vs not
house_dems_wspons <- house_dems_wspons %>% 
  mutate(
    stance = case_when(
      position == "sponsor" ~ "sponsoring", 
      position == "cosponsor" ~ "sponsoring", 
      position == "not sponsor" ~ "not_sponsoring", 
      TRUE ~ "not_sponsoring"
    )
  ) %>% 
  select(stance, everything()) %>% 
  mutate(
    stance = str_squish(stance)
  )
  
#finally, we'll REMOVE PELOSI since the speaker doesn't really cosponsor bills
#that will give us 234 total dems
house_dems_wspons <- house_dems_wspons %>% 
  filter(id != "P000197")



#### BRING IN THE CONGRESSIONAL DISTRICT PROFILE DATA (FROM RB PROJECT) #### -----------
workingtable <- readRDS("processed_data/workingtable.rds")

house_dems_wspons <- house_dems_wspons %>% 
  rename(GEOID = geoid)

#join
working_joined <- inner_join(house_dems_wspons, workingtable, by = "GEOID")

#add flag columns for margin categorization
working_joined <- working_joined %>% 
  mutate(
    margin_flag = case_when(
      live_margin <= 5 ~ "5_points_or_less", 
      live_margin > 5 ~ "more_than_5_points", 
      TRUE ~ "more_than_5_points"
    )
  ) 

working_joined %>% 
  filter(margin_flag == "other") %>% 
  View()

#save results
writexl::write_xlsx(working_joined, "output/working_joined_hr1296.xlsx")
saveRDS(working_joined, "output/working_joined_hr1296.rds")




### ANALYSIS ####

#for crosstabs using summarytools
# print(ctable(tobacco$smoker, tobacco$diseased, prop = "r"), method = "render")

# trump districts vs hillary
summarytools::ctable(working_joined$p16winningparty, working_joined$stance, prop = "r")

#education
ctable(working_joined$pct.ed.college.all.abovebelow.natl, working_joined$stance, prop = "r")
ctable(working_joined$pct.ed.college.all.abovebelow.natl, working_joined$stance, prop = "c")

#GDP
ctable(working_joined$gdp_abovebelow_natlavg, working_joined$stance, prop = "r")
ctable(working_joined$gdp_abovebelow_natlavg, working_joined$stance, prop = "c")
ctable(working_joined$gdp_abovebelow_natlavg, working_joined$stance, prop = "n")



# summarytools::tb()

glimpse(working_joined)


# groupings for export to spreadsheet for gfx ####

prezresults2016 <- working_joined %>% 
  count(p16winningparty, stance)

gdp <- working_joined %>% 
  count(gdp_abovebelow_natlavg, stance)

college_degree <- working_joined %>% 
  count(pct.ed.college.all.abovebelow.natl, stance)

nonwhite_pop <- working_joined %>% 
  count(pct.race.nonwhite.abovebelow.natl, stance)

rural_area <- working_joined %>% 
  count(pct.rural.above20, stance)

margin_5_or_less <- working_joined %>% 
  count(margin_flag, stance)


#the same with prezresults

gdp_andprezresults <- working_joined %>% 
  count(p16winningparty, gdp_abovebelow_natlavg, stance)

college_degree_andprezresults <- working_joined %>% 
  count(p16winningparty, pct.ed.college.all.abovebelow.natl, stance)

nonwhite_pop_andprezresults <- working_joined %>% 
  count(p16winningparty, pct.race.nonwhite.abovebelow.natl, stance)

rural_area_andprezresults <- working_joined %>% 
  count(p16winningparty, pct.rural.above20, stance)

margin_5_or_less_withprez <- working_joined %>% 
  count(p16winningparty, margin_flag, stance)



#now make a list to feed to writexl
list_of_breakdowns <- list(prezresults2016 = prezresults2016,
                           gdp_vs_nationalavg = gdp,
                           college_vs_nationalavg = college_degree,
                           nonwhite_vs_nationalavg = nonwhite_pop,
                           rural_morethanfifth = rural_area,
                           margin_5_or_less = margin_5_or_less,
                           gdp_andprezresults = gdp_andprezresults,
                           college_degree_andprezresults = college_degree_andprezresults,
                           nonwhite_pop_andprezresults = nonwhite_pop_andprezresults,
                           rural_area_andprezresults = rural_area_andprezresults,
                           margin_5_or_less_withprez = margin_5_or_less_withprez
                           )

writexl::write_xlsx(list_of_breakdowns, "output/groupings_for_dems_hr1296.xlsx")




working_joined %>% 
  filter(margin_flag == "5_points_or_less") 


###

working_joined %>% 
  count(position)

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


