# AHS Linking -------------------------------------------------------------

setwd("./AHS")

library(haven)
library(tidyverse)


women <- read_dta("women.dta", col_select = c(1:31, 46:49, 54:55, 85:104, 122:155))

wps <- read_dta("wps.dta", col_select = c(1:49, 59:62, 93, 126:127, 218:219, 249:267, 286:319))
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
#check <- subset(wps, n > 3)


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

#WPS -- filtering out any cases without PSUs (~10 %)

wps_full <- wps %>% filter(!is.na(psu))

#filtering out cases without member identity (~4k)
wps_full <- wps_full %>% filter(!is.na(member_identity))


table(wps_full$n)

#need a most recent outcome variable combining births + abortions so can take most recent occurrence
#filtering out variables not needed
names(wps_full)

wps_full <- wps_full %>% select(caseid, state, district, rural, stratum_code, psu, hhld_id, member_identity, result_of_interview, date_of_intr, month_of_intr, 
                                year_of_intr,
                                intvw_yrmo, round, age, marital_status, ever_conceived, no_of_times_conceived, age_at_first_conception, delivered_any_baby, 
                                born_alive_total, surviving_total, mother_age_when_baby_was_born, outcome_pregnancy, preg_flag, out_come_of_preg, month_of_abortion,
                                year_of_abortion, abort_yrmo, abortion_month, anc_status, ultrsound_status, abortion_performed, abortion_performed_by, 
                                reason_for_abortion, kind_of_birth, previous_current_diff, dob, mob, yob, birth_yrmo, gender, no_of_anc, no_of_months_first_anc,
                                source_of_anc, where_del_took_place, who_conducted_del_at_home, check_up_with_48_hours_of_del, is_any_fp_methos_used, fp_method_used,
                                sex, usual_residance, date_of_birth, month_of_birth, year_of_birth, mom_birth_yrmo, religion, social_group_code, highest_qualification, 
                                house_structure,
                                owner_status, drinking_water_source, is_water_filter, water_filteration, toilet_used, iscoveredbyhealthscheme, healthscheme_1,
                                healthscheme_2, wt, as, as_binned, nfhs4_census2011_district_id)

#making wps outcome variable. If outcome is 3 or 4 (induced and spontaneous abortion) we use use year of abortion. For outcome 1 and 2 we use use year of birth.

#too much memorty to run, going as if/else statement instead
#wps_full <- wps_full %>% mutate(outcome_year = case_when(out_come_of_preg < 3 ~ yob,
#                                               out_come_of_preg > 2 ~ year_of_abortion,
#                                               TRUE ~ NA_real_))

wps_full$outcome_year <- ifelse(wps_full$out_come_of_preg < 3, wps_full$yob, NA)  
wps_full$outcome_year <- ifelse(wps_full$out_come_of_preg > 2, wps_full$year_of_abortion, wps_full$outcome_year) 

wps_full$outcome_month <- ifelse(wps_full$out_come_of_preg < 3, wps_full$mob, NA)
wps_full$outcome_month <- ifelse(wps_full$out_come_of_preg > 2, wps_full$month_of_abortion, wps_full$outcome_month)

#dropping all NAs for outcome year. Means it was missing either the type of preg outcome OR the year that the outcome occurred. 

wps_full <- wps_full %>% filter(!is.na(outcome_year))

#writing csv
#write.csv(wps_full, "wpsfull.csv")

#wps_full <- read.csv("wpsfull.csv")
wps_full <- wps_full %>% select(-c(X))

wps_full <- wps_full %>% group_by(caseid)

names(dlhs_nfhs_preg)

table(wps_full$round, wps_full$year_of_intr)


#filtering out only most recent pregnancy outcomes to match dlhsnfhs_preg.

#checking to make sure it's grouped
is_grouped_df(wps_full) #Yes

#making previous stillbirth variable before limiting to most recent outcome
# start with pregnancies. Limit to most recent pregnancy

wps_full <- wps_full %>% mutate(stillbirths = length(which(out_come_of_preg ==2)))

# making most recent termination if there was termination before most recent birth

wps_full <- wps_full %>% mutate(nonlivebirths = length(which(out_come_of_preg > 1)))

wps_full$recent_mab_year <- ifelse(wps_full$out_come_of_preg == 2, wps_full$yob, NA)
wps_full$recent_mab_year <- ifelse(wps_full$out_come_of_preg == 3, wps_full$year_of_abortion, wps_full$recent_mab_year)
wps_full$recent_mab_year <- ifelse(wps_full$out_come_of_preg == 4, wps_full$year_of_abortion, wps_full$recent_mab_year)

# need to assign type of most recent termination to group. Probably need to do unique rows first, then:
# by group most recent outcome -- if termination what kind and DONE.
# if most recent outcome is birth then find most recent termination after.
# if an

#wps_full <- wps_full %>% group_by(caseid) %>% mutate(last_even_flag = case_when(outcome_year == max(outcome_year ) ~ 1,
                                                                                TRUE ~ 0))

# In NFHS and DLHS we have most recent birth outcome (outcome), and then, if there was at least 1 termination before the most recent outcome what that was.
# leaving for now because we are not including how many pregnancies there were between most recent pregnancy and outcome

#check2 <- check2 %>% mutate(mab = miscarriage_abortion_stillbirth[recent_term == 1L])

#creating previous stillbirths logic: If more than one stillbirth == YES, if == 1 stillbirth & year of stillbirth != latest outcome date == YES
# if == 1 stillbirth && stillbirth date == latest outcome date == NO previous stillbirth

#starting with if >1 stillbirth

wps_full$prev_stillbirth <- ifelse(wps_full$stillbirths > 1, 1, 0)

#Need to note index pregnancy (last most recent) and see if there was a stillbirth before index preg, if so stillbirth. Current code not working.
#Need year of most recent stillbirth if ANY stillbirths at all. 

wps_full$stillbirth_year <- ifelse(wps_full$out_come_of_preg == 2, wps_full$yob, NA)

## THIS CODE RUN ON CLUSTER. TOO MUCH FOR DESKTOP ##
wps_full <- wps_full %>% group_by(caseid) %>% mutate(maxyearsb = max(stillbirth_year, na.rm = TRUE))
## END CODE RUN ON CLUSTER ##

wpssb <- read.csv("wpssb.csv") #merge this wps_full to get sb years for groups

wpssb <- wpssb %>% select(-c(X))

wps_allsb <- read.csv("wps_all_sb.csv")



wpsstill <- wpssb %>% select(c(caseid, year_of_intr, round, out_come_of_preg, outcome_year, maxyearsb))

wpssb$caseid <- as.character(wpssb$caseid)
wps_full$state <- as.integer(wps_full$state)
wps_full$district <- as.integer(wps_full$district)
wps_full$psu <- as.integer(wps_full$psu)

wps_preg <- left_join(
  wps_full,
  wpssb,
#  by = c("caseid", "year_of_intr", "round", "ever_conceived",
#         "out_come_of_preg", "yob", "n", "outcome_year","stillbirths", "prev_stillbirth", "stillbirth_year")
)

#create previous stillbirth variable
wps_preg <- wps_preg %>% mutate(previous_sb = case_when(maxyearsb < outcome_year ~ 1,
                                                        TRUE ~ 0))

#write.csv(wps_preg, "wps_preg.csv")
wps_preg <- read.csv("ahs_preg.csv")

#grouping by caseid

wps_preg <- wps_preg %>% group_by(caseid)
wps_preg <- wps_preg %>% add_count(caseid)

#wpssb <- wps_full %>% filter(stillbirths > 0)
#wpssb <- wpssb %>% mutate(checkmax = max(stillbirth_year, na.rm = TRUE))

#wpssb <- wpssb %>% mutate(maxoutcome = max(outcome_year))

#mutate(marital_status = c7[femaleheadspouse == 1L])



# Limiting to most recent outcome -----------------------------------------

ahs_preg <- ahs_preg %>% arrange(desc(outcome_year)) %>% slice(1)

#write.csv(ahs_preg, "ahs_preg.csv")
ahs_preg <- read.csv("ahs_preg.csv")

# Combining with dlhs_nfhs_preg -------------------------------------------

names(dlhsnfhs_preg_mothercov)

names(ahs_preg)

#dropping ahs_preg variables we don't need
ahs_preg <- ahs_preg %>% select(-c(hhld_id, member_identity, result_of_interview, intvw_yrmo, abort_yrmo, n, stillbirths, prev_stillbirth, stillbirth_year, 
                                   maxyearsb, nn))

dlhsnfhs_preg_mothercov <- dlhsnfhs_preg_mothercov %>% relocate(state, .before = district)

ahs_preg$survey <- c("ahs")

#adding in education variable from full WPS dataset. Matching and merging.
wps_educ <- wps %>% select(c(caseid, state, district, rural, stratum_code, psu, date_of_intr, month_of_intr, year_of_intr, round, age, marital_status,
                             ever_conceived, no_of_times_conceived, age_at_first_conception, delivered_any_baby, born_alive_total, 
                             surviving_total, mother_age_when_baby_was_born, outcome_pregnancy, preg_flag, out_come_of_preg, month_of_abortion,
                             year_of_abortion, abortion_month, anc_status, ultrsound_status, abortion_performed, abortion_performed_by,
                             reason_for_abortion, kind_of_birth, previous_current_diff, dob, mob, yob, birth_yrmo, gender, no_of_anc, 
                             no_of_months_first_anc, source_of_anc, where_del_took_place, who_conducted_del_at_home, check_up_with_48_hours_of_del,
                             is_any_fp_methos_used, fp_method_used, sex, usual_residance, date_of_birth, month_of_birth, year_of_birth, mom_birth_yrmo,
                             religion, social_group_code, highest_qualification, house_structure, owner_status, drinking_water_source,
                             is_water_filter, water_filteration, toilet_used, iscoveredbyhealthscheme, healthscheme_1, healthscheme_2,
                             wt, as, as_binned, nfhs4_census2011_district_id))


ahs_preg$state <- str_pad(ahs_preg$state, 2, "left", "0")
ahs_preg$district <- str_pad(ahs_preg$district, 2, "left", "0")
#stratum code is 0/1/2, leaving as is
#making psu 3 numbers
ahs_preg$psu <- str_pad(ahs_preg$psu, 3, "left", "0")



ahs_preg_educ <- left_join(
  ahs_preg,
  wps_educ
)


#picking out distinct (some duplicates in wps that we removed when picking most recent outcome. Duplicates seem to be mostly round 1 and 2)
nrow(ahs_preg)
nrow(ahs_preg_educ)
nrow(distinct(ahs_preg_educ))

ahs_preg_educ <- distinct(ahs_preg_educ)


ahs_preg_match <- ahs_preg_educ %>% select(c(caseid, state, district, psu, out_come_of_preg, previous_sb, survey, year_of_abortion, yob, wt, year_of_intr, age,
                                  rural, religion, social_group_code, highest_qualification,drinking_water_source, toilet_used, healthscheme_1, 
                                  healthscheme_2, age_at_first_conception,
                                  born_alive_total, no_of_months_first_anc, no_of_anc, kind_of_birth, previous_current_diff, where_del_took_place, who_conducted_del_at_home,
                                  check_up_with_48_hours_of_del, abortion_month, fp_method_used, as, nfhs4_census2011_district_id))


#making health insurance variables
ahs_preg_match$esis <- ifelse(ahs_preg_match$healthscheme_1 == 1 | ahs_preg_match$healthscheme_2 == 1, 1, 0)
ahs_preg_match$rsby <- ifelse(ahs_preg_match$healthscheme_1 == 2 | ahs_preg_match$healthscheme_2 == 2, 1, 0)
ahs_preg_match$othergov <- ifelse(ahs_preg_match$healthscheme_1 == 3 | ahs_preg_match$healthscheme_2 == 3, 1, 0)
ahs_preg_match$reimburse <- ifelse(ahs_preg_match$healthscheme_1 == 4| ahs_preg_match$healthscheme_2 == 4, 1, 0)
ahs_preg_match$chip <- ifelse(ahs_preg_match$healthscheme_1 == 5| ahs_preg_match$healthscheme_2 == 5, 1, 0)
ahs_preg_match$mediclaim <- ifelse(ahs_preg_match$healthscheme_1 == 6 | ahs_preg_match$healthscheme_2 == 6, 1, 0)
ahs_preg_match$other_insurance <- ifelse(ahs_preg_match$healthscheme_1 == 7 | ahs_preg_match$healthscheme_2 == 7, 1, 0)

ahs_preg_match <- ahs_preg_match %>% select(-c(healthscheme_1, healthscheme_2))

names(dlhsnfhs_preg_mothercov)
names(ahs_preg_match)

#dropping dlhsnfhs_preg_mothercov variables not in ahs
dlhsnfhs_preg_mothercov <- dlhsnfhs_preg_mothercov %>% select(-c(ever_terminated, month_last_terminated, year_last_terminated, caste,
                                                                 fp_future, bpl, health_ins, other_terminations, miscarriage_abortion_stillbirth))




# fixing district to match NFHS -------------------------------------------






#looking at most recent year of outcome. Will also need to further filter by preg_flag to be most recent pregnancy as for some reaosn 
#there are some instances of multiple births per year

wps_try <- wps_full %>% group_by(caseid) %>% top_n(1, outcome_year)

wps_try <- wps_try %>% filter(preg_flag == 1)

wps_try <- wps_try %>% add_count(caseid)
table(wps_try$nn)


#looking at those with more than one preg
check <- wps_try %>% filter(nn > 1)

# Removing duplicates that are likely mistakes in entering rounds. I.e., if year of interview is 2011 but round is 3. 
# Round 1 was collected July 2010 - March 2011, Round 2 was collected Oct 2011 - April 2012,
# Round 3 was collected Nov 2012 - May 2013

# Removing all round 3s from 2010 & 2011
# Removing all Round 2s from 2010 & 2013
# Removing all round 1s from 2012

length(which(wps_try$round == 3 & wps_try$year_of_intr == 2010))
length(which(wps_try$round == 3 & wps_try$year_of_intr == 2011))

#filtering out NAs for year of interview
wps_try <- wps_try %>% filter(!is.na(year_of_intr))

#now filtering out rounds in wrong year and making NA
wps_try$round1 <- ifelse(wps_try$round == 3 & wps_try$year_of_intr < 2012, NA, wps_try$round)
wps_try$round1 <- ifelse(wps_try$round == 2 & wps_try$year_of_intr == 2010, NA, wps_try$round1)
wps_try$round1 <- ifelse(wps_try$round == 2 & wps_try$year_of_intr == 2013, NA, wps_try$round1)
wps_try$round1 <- ifelse(wps_try$round == 1 & wps_try$year_of_intr == 2012, NA, wps_try$round1)

#now filtering out these NAs
wps_try <- wps_try %>% filter(!is.na(round1))

wps_try1 <- wps_try %>% select(-c(nn, round1))
wps_try1 <- wps_try1 %>% add_count(caseid)

check <- wps_try1 %>% filter(nn > 1)

#should only be 1 pregnancy (most recent for caseid). Filtering out the ~1060 with more than one preganncy kept. On check it looked like errors (i.e. multiple entires for same pregnancy)

ahs_preg <- wps_try1 %>% filter(nn == 1)

names(ahs_preg)

