# AHS Linking -------------------------------------------------------------

setwd("./AHS")

library(haven)
library(tidyverse)


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

#putting leading 0s so all items have same number of digits. Trying this for unique for linking.
wps$state <- str_pad(wps$state, 2, "left", "0")
wps$district <- str_pad(wps$district, 2, "left", "0")
#stratum code is 0/1/2, leaving as is
#making psu 3 numbers
wps$psu <- str_pad(wps$psu, 3, "left", "0")

wps$house_hold_no <- str_pad(wps$house_hold_no, 2, "left", "0")


wps <- wps %>% mutate(caseid = paste0(state, district, stratum_code, psu, house_no, house_hold_no, member_identity))

#grouping pregnancies to mothers

wps <- wps %>% group_by(caseid)

wps <- wps %>% add_count(caseid)


table(wps$n)
check <- subset(wps, n > 3)


# start with pregnancies. Limit to most recent pregnancy
wps$prev_stillbirth <- ifelse(wps$out_come_of_preg == 2, 1, 0)

#making most recent birth designation
#wps <- wps %>% mutate(recent_birth = top_n(1, yob) ~ 1,
#                      TRUE ~ 0)

### Sept 19 above most recent birth code does not work. Will come back to after full women's dataset created.

#for women, combine all rounds so we measure all 5 years of women's birth history
names(women)

#group_by ID and then most recent birth (top_n) from that? can use LFS code for most recent
ahs <- ahs %>% group_by(caseid)



#looking at number of rounds
ahs <- ahs %>% add_count(caseid)



#becomes labeled as n, looking at number of rounds
table(ahs$n)
check <- subset(ahs, n > 3)

#dropping 96 obs that have more than 3 rounds
ahs <- ahs %>% filter(n < 4)

#making health insurance variables.

#note 2045 observations where healthscheme_1 == healthscheme_2

ahs$esis <- ifelse(ahs$healthscheme_1 == 1 | ahs$healthscheme_2 == 1, 1, 0)
ahs$rsby <- ifelse(ahs$healthscheme_1 == 2 | ahs$healthscheme_2 == 2, 1, 0)
ahs$othergov <- ifelse(ahs$healthscheme_1 == 3 | ahs$healthscheme_2 == 3, 1, 0)
ahs$reimburse <- ifelse(ahs$healthscheme_1 == 4| ahs$healthscheme_2 == 4, 1, 0)
ahs$chip <- ifelse(ahs$healthscheme_1 == 5| ahs$healthscheme_2 == 5, 1, 0)
ahs$mediclaim <- ifelse(ahs$healthscheme_1 == 6 | ahs$healthscheme_2 == 6, 1, 0)
ahs$other_insurance <- ifelse(ahs$healthscheme_1 == 7 | ahs$healthscheme_2 == 7, 1, 0)

#dropping old health insurance variables
ahs <- ahs %>% select(-c(healthscheme_1, healthscheme_2))

names(dlhsnfhs)

dlhsnfhs <- dlhsnfhs %>% select(-c(X))

names(ahs)

#dropping stratum code
ahs <- ahs %>% select(-c(stratum_code))

#dropping id variables
ahs <- ahs %>% select(-c(house_no, house_hold_no, hhld_id, identifcation_code, member_identity))

#making round id for linking
ahs <- ahs %>% mutate(roundid = paste0(state, district, stratum_code, psu, house_no, house_hold_no, identifcation_code, round))

ahs <- ahs %>% group_by(roundid)

ahs <- ahs %>% add_count(roundid)

#for round ID -- works almost entirely. Only 204 have more than one in the group. 99.999%. looking at outliers
linkingcheck <- subset(ahs, n > 1)

#moving caseid to before state
ahs <- ahs %>% relocate(caseid, .before = state)

#moving wt id
ahs <- ahs %>% relocate(wt, .after = district)


#comparing WPS and AHS to see if we can get year from WPS into AHS by matching on caseid
#creating smaller datasets for comparison of year

ahs_id <- ahs %>% select(caseid, round, year_of_intr, intvw_yrmo)
wps_id <- wps %>% select(caseid, round, year_of_intr, intvw_yrmo)

check <- semi_join(wps_id, ahs_id, by = "caseid")

#Just use AHS for pregnancy outcomes since only WPS has year of survey?
#combine WPS with dlhs_nfhs_preg and add in all women variables to dlhs_nfhs_preg using merge, but unit is pregnancies.

#FIRST STEP: Limit AHS to most recent pregnancy across all rounds. Group --> Wide / top_n?
names(wps)
wps <- wps %>% relocate(caseid, .before = state)

wps_check <- wps %>% filter(n > 7)

#WPS -- filtering out any cases without PSUs (~10 %)

wps_full <- wps %>% filter(!is.na(psu))

table(wps_full$n)
wps_check <- wps_full %>% filter(n > 6)

#need a most recent outcome variable combining births + abortions so can take most recent occurrence
#filtering out variables not needed
names(wps_full)

wps_full <- wps_full %>% select(caseid, state, district, rural, stratum_code, psu, hhld_id, result_of_interview, date_of_intr, month_of_intr, year_of_intr,
                                intvw_yrmo, round, age, marital_status, ever_conceived, no_of_times_conceived, age_at_first_conception, delivered_any_baby, 
                                born_alive_total, surviving_total, mother_age_when_baby_was_born, outcome_pregnancy, preg_flag, out_come_of_preg, month_of_abortion,
                                year_of_abortion, abort_yrmo, abortion_month, anc_status, ultrsound_status, abortion_performed, abortion_performed_by, 
                                reason_for_abortion, kind_of_birth, previous_current_diff, dob, mob, yob, birth_yrmo, gender, no_of_anc, no_of_months_first_anc,
                                source_of_anc, where_del_took_place, who_conducted_del_at_home, check_up_with_48_hours_of_del, is_any_fp_methos_used, fp_method_used,
                                sex, usual_residance, date_of_birth, month_of_birth, year_of_birth, mom_birth_yrmo, religion, social_group_code, house_structure,
                                owner_status, drinking_water_source, is_water_filter, water_filteration, toilet_used, iscoveredbyhealthscheme, healthscheme_1,
                                healthscheme_2, wt, as, as_binned, nfhs4_census2011_district_id, n, prev_stillbirth)

#making wps outcome variable. If outcome is 3 or 4 (induced and spontaneous abortion) we use use year of abortion. For outcome 1 and 2 we use use year of birth.

wps_full <- wps_full %>% mutate(outcome_year = case_when(out_come_of_preg < 3 ~ yob,
                                               out_come_of_preg > 2 ~ year_of_abortion,
                                               TRUE ~ NA_real_))

wps_full$outcome <- ifelse(wps_full$out_come_of_preg < 3, wps_full$yob, NA)  
wps_full$outcome <- ifelse(wps_full$out_come_of_preg > 2, wps_full$year_of_abortion, wps_full$outcome) 

wps_full$outcome_month <- ifelse(wps_full$out_come_of_preg < 3, wps_full$mob, NA)
wps_full$outcome_month <- ifelse(wps_full$out_come_of_preg > 2, wps_full$month_of_abortion, wps_full$outcome_month)

#dropping all NAs for outcome year. Means it was missing either the type of preg outcome OR the year that the outcome occurred. 

wps_full <- wps_full %>% filter(!is.na(outcome))

#renaming outcome to be outcome year
wps_full <- rename(wps_full, outcome_year = outcome)

wpscheck <- wps_full %>% select(caseid, out_come_of_preg, mob, yob, month_of_abortion, year_of_abortion, outcome_year, outcome_month)

#writing csv
#write.csv(wps_full, "wpsfull.csv")

names(dlhs_nfhs_preg)
