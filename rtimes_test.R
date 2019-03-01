# https://cran.rstudio.com/web/packages/rtimes/vignettes/rtimes_vignette.html

library(rtimes)
library(tidyverse)

#pull votes for one member of congress
# Member ID S001181 is Jeanne Shaheen
out <- cg_billscosponsor(memberid='S001181', type='cosponsored')
out$data


#pull member appearances
out <- cg_memberappear(memberid='S001181')
out$data


#get list of members of congress
senatemembers <- cg_memberslist(congress_no = 116, chamber = 'senate')
senatemembers_df <- senatemembers$data

housemembers <- cg_memberslist(congress_no = 116, chamber = 'house')
housemembers_df <- housemembers$data


# get a specific roll call vote
# cg_rollcallvote(congress_no = NULL, chamber = NULL, session_no = NULL,
#                 rollcall_no = NULL, key = NULL, ...)

rollcall <- cg_rollcallvote(congress_no = 116, chamber = 'house', session_no = 1, 
                rollcall_no = 98)

rollcall





