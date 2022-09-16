# AHS Linking -------------------------------------------------------------

setwd(".././AHS")

library(haven)
library(tidyverse)

#wps <- read_dta("WPS.dta")
women <- read_dta("women.dta", col_select = c(1:31, 46:49, 54:55, 85:104, 122:155))

wps <- read_dta("wps.dta", col_select = c(1:49, 59:62, 93, 126:127, 218:219, 249:264, 286:319))
#names(wps)

names(women)
names(women) <- tolower(names(women))

names(dlhsnfhs)

ahs <- women %>% select(c(state, district, rural, stratum_code, psu, house_no, house_hold_no,  
                            hhld_id, date_of_intr, month_of_intr, year_of_intr, intvw_yrmo, round, 
                            identifcation_code, member_identity, age, marital_status, ever_conceived, no_of_times_conceived, age_at_first_conception,
                            delivered_any_baby, born_alive_total, outcome_pregnancy, fp_method_used, date_of_birth,
                            month_of_birth, year_of_birth, religion, social_group_code, highest_qualification, drinking_water_source, 
                            toilet_used,
                            iscoveredbyhealthscheme, healthscheme_1, healthscheme_2, wt, as, nfhs4_census2011_district_id))

#putting leading 0s so all items have same number of digits. Trying this for unique for linking.
ahs$state <- str_pad(ahs$state, 2, "left", "0")
ahs$district <- str_pad(ahs$district, 2, "left", "0")
#stratum code is 0/1/2, leaving as is
#making psu 3 numbers
ahs$psu <- str_pad(ahs$psu, 3, "left", "0")

ahs$house_hold_no <- str_pad(ahs$house_hold_no, 2, "left", "0")

ahs <- ahs %>% mutate(caseid = paste0(state, district, stratum_code, psu, house_no, house_hold_no, identifcation_code))

names(wps)
names(wps) <- tolower(names(wps))

wps <- wps %>% mutate(caseid = paste0(state, district, stratum_code, psu, house_no, house_hold_no, member_identity))

#grouping pregnancies to mothers
wps <- wps %>% group_by(caseid)

# start with pregnancies. Limit to most recent pregnancy
wps$prev_stillbirth <- ifelse(wps$out_come_of_preg == 2, 1, 0)

#making most recent birth designation
wps <- wps %>% mutate(recent_birth = top_n(1, yob) ~ 1,
                      TRUE ~ 0)


#for women, combine all rounds so we measure all 5 years of women's birth history
names(women)

#group_by ID and then most recent birth (top_n) from that? can use LFS code for most recent
ahs <- ahs %>% group_by(caseid)



#looking at number of rounds
ahs <- ahs %>% add_count(caseid)



#becomes labeled as n, looking at number of rounds
table(ahs$n)
check <- subset(ahs, n > 3)

#making health insurance variables
ahs$esis <- ifelse(ahs$healthscheme_1 == 1, 1, 0)
ahs$rsby <- ifelse(ahs$healthscheme_1 == 2, 1, 0)
ahs$othergov <- ifelse(ahs$healthscheme_1 == 3, 1, 0)
ahs$reimburse <- ifelse(ahs$healthscheme_1 == 4, 1, 0)
ahs$chip <- ifelse(ahs$healthscheme_1 == 5, 1, 0)
ahs$mediclaim <- ifelse(ahs$healthscheme_1 == 6, 1, 0)
ahs$other_insurance <- ifelse(ahs$healthscheme_1 == 7, 1, 0)

length(which(ahs$healthscheme_1 != 2 & ahs$healthscheme_2 == 2)) #1317 individuals who report RSBY as health scheme 2

#looking at individuals who report RSBY as their second health insurance scheme
rsby2 <- ahs %>% filter(healthscheme_1 !=2 & healthscheme_2 == 2)
#768 report ESIS as their first and RSBY as their second


names(dlhsnfhs)

#making round id for linking
ahs <- ahs %>% mutate(roundid = paste0(state, district, stratum_code, psu, house_no, house_hold_no, identifcation_code, round))

ahs <- ahs %>% group_by(roundid)

ahs <- ahs %>% add_count(roundid)

#for round ID -- works almost entirely. Only 204 have more than one in the group. 99.999%. looking at outliers
linkingcheck <- subset(ahs, n > 1)

