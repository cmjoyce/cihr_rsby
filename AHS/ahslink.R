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
#                                                                                TRUE ~ 0))

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

#write.csv(ahs_preg_match, "ahs_preg_match.csv")

names(dlhsnfhs_preg_mothercov)
names(ahs_preg_match)

#dropping dlhsnfhs_preg_mothercov variables not in ahs
dlhsnfhs_preg_mothercov <- dlhsnfhs_preg_mothercov %>% select(-c(ever_terminated, month_last_terminated, year_last_terminated, caste,
                                                                 fp_future, bpl, health_ins, other_terminations, miscarriage_abortion_stillbirth))


dlhsnfhs_preg_mothercov <- dlhsnfhs_preg_mothercov %>% select(-c(using_method))

ahs_preg_match <- ahs_preg_match %>% rename(outcome = out_come_of_preg, age_first_birth = age_at_first_conception, )

names(dlhsnfhs_preg_mothercov)
names(ahs_preg_match)

#renaming 
ahs_preg_match <- ahs_preg_match %>% rename(interval_preg = previous_current_diff,
                                            rural_urban = rural,
                                            prev_stillbirth = previous_sb,
                                            caste_group = social_group_code,
                                            source_water = drinking_water_source,
                                            toilet = toilet_used,
                                            tot_live_births = born_alive_total,
                                            month_received_anc = no_of_months_first_anc,
                                            num_anc_visits = no_of_anc,
                                            type_delivery = kind_of_birth,
                                            place_delivery = where_del_took_place,
                                            conducted_delivery = who_conducted_del_at_home,
                                            pp_checkup = check_up_with_48_hours_of_del,
                                            type_method = fp_method_used
                                            )

#Harmonizing schooling variables. Highest qualification does not give number of years of school,
#so we are using a binary >= primary school variable (1 == yes Primary, 2 == no primary)

table(dlhsnfhs_preg_mothercov$years_school)

#making 98 and 99 NAs
dlhsnfhs_preg_mothercov$years_school <- ifelse(dlhsnfhs_preg_mothercov$years_school > 97, 
                                               NA, dlhsnfhs_preg_mothercov$years_school)

#separating into at least primary (grade 8) and less than primary.
#could later seperate into lower primary or higher, lower primary is class 1-IV
length(which(dlhsnfhs_preg_mothercov$years_school > dlhsnfhs_preg_mothercov$age)) #42 women who have more years of school than their age. Must be error.
#making into NA

dlhsnfhs_preg_mothercov$years_school <- 
  ifelse(dlhsnfhs_preg_mothercov$years_school > dlhsnfhs_preg_mothercov$age, NA, 
         dlhsnfhs_preg_mothercov$years_school)

dlhsnfhs_preg_mothercov$primary <- ifelse(dlhsnfhs_preg_mothercov$years_school >= 8, 1, 0)
table(dlhsnfhs_preg_mothercov$primary)

#in AHS >= 3 is completed at least a primary education
table(ahs_preg_match$highest_qualification)
ahs_preg_match$primary <- ifelse(ahs_preg_match$highest_qualification > 2, 1, 0)

#dropping old schooling variables
dlhsnfhs_preg_mothercov <- dlhsnfhs_preg_mothercov %>% select(-c(years_school))
ahs_preg_match <- ahs_preg_match %>% select(-c(highest_qualification))

dlhsnfhs_preg_mothercov <- dlhsnfhs_preg_mothercov %>% select(-c(nonbirth))

#making mediclaim other private
ahs_preg_match <- ahs_preg_match %>% rename(other_private = mediclaim)
#making other gov cghsshis
ahs_preg_match <- ahs_preg_match %>% rename(cghsshis = othergov)

#dropping asset score
ahs_preg_match <- ahs_preg_match %>% select(-c(as))

#dropping original district variable in ahs and renaming nfhs4_match variable as district
ahs_preg_match <- ahs_preg_match %>% select(-c(district))
ahs_preg_match <- ahs_preg_match %>% rename(district = nfhs4_census2011_district_id)
ahs_preg_match <- ahs_preg_match %>% relocate(district, .after = state)

#dropping index mob and yob as the date is in outcome
dlhsnfhs_preg_mothercov <- dlhsnfhs_preg_mothercov %>% select(-c(index_mob, index_yob))

#renaming ahs to match on abortion month. Need to fix NFHS 4 and 5 ...this was asked for all terminations,
#but for DLHS and AHS it was asked for just induced abortions. Will come back to this.
ahs_preg_match <- ahs_preg_match %>% rename(month_last_termination_occurred = abortion_month)

#fixing dlhsnfhs date of outcome to just be year like in ahs
table(dlhsnfhs_preg_mothercov$date_outcome)

dlhsnfhs_preg_mothercov$year_outcome <- gsub("-.*", "", dlhsnfhs_preg_mothercov$date_outcome)
dlhsnfhs_preg_mothercov <- dlhsnfhs_preg_mothercov %>% select(-c(date_outcome))

names(dlhsnfhs_preg_mothercov)
names(ahs_preg_match)

#making ahs outcome year. Live birth == 1, Still birth == 1, if either we want year of birth
# induced and spontaneous abortion are 3 and 4, if either we want year of abortion
ahs_preg_match <- ahs_preg_match %>% mutate(outcome_year = 
                                              case_when(outcome < 3 ~ yob,
                                                        outcome > 2 ~ year_of_abortion,
                                                        TRUE ~ NA_real_))

#dropping old variables
ahs_preg_match <- ahs_preg_match %>% select(-c(yob, year_of_abortion))

#dropping date and month of interview in dlhsnfhspreg
dlhsnfhs_preg_mothercov <- dlhsnfhs_preg_mothercov %>% select(-c(date, month))

#moving ahs interval preg after outcome
ahs_preg_match <- ahs_preg_match %>% relocate(interval_preg, .after = outcome)

#renaming year
dlhsnfhs_preg_mothercov <- dlhsnfhs_preg_mothercov %>% rename(year_of_intr = year)

dlhsnfhs_preg_mothercov <- dlhsnfhs_preg_mothercov %>% rename(outcome_year = year_outcome)

#making ahs state numeric
ahs_preg_match$state <- as.numeric(ahs_preg_match$state)
ahs_preg_match$psu <- as.numeric(ahs_preg_match$psu)

dlhsnfhs_preg_mothercov$outcome_year <- as.numeric(dlhsnfhs_preg_mothercov$outcome_year)

df <- bind_rows(dlhsnfhs_preg_mothercov, ahs_preg_match)

#write.csv(df, "full_combined_preg.csv")

df <- read.csv("full_combined_preg.csv")


# Harmonizing all variables -----------------------------------------------

# Using combined codebook for harmonization. Excluding district harmonization for now.

df$survey <- toupper(df$survey)

# outcome of pregnancy
# 6 is T for termination. Making NAs.

df$outcome <- ifelse(df$outcome == 6, NA, df$outcome)

#previous stillbirth is matched.
table(df$prev_stillbirth)

#rural urban
table(df$rural_urban)

# correcting rural / urban coding. In DLHS 1 is rural and 2 is urban, in NFHS it is the opposite.

table(df$rural_urban, df$survey)

#there are no NAs so we can use code below to swap. Changing to 0 = rural and 1 = urban. Using casewhen to 
# change all rural codes by survey to 0, and making the rest 1s as they must be urbans.

df <- df %>% mutate(urban = case_when(survey == "DLHS3" & rural_urban == 1 ~ 0,
                                      survey == "DLHS4" & rural_urban == 1 ~ 0,
                                      survey == "NFHS4" & rural_urban == 2 ~ 0,
                                      survey == "NFHS5" & rural_urban == 2 ~ 0,
                                      survey == "AHS" & rural_urban == 1 ~ 0,
                                      TRUE ~ 1))

#dropping old rural_urban variable
df <- df %>% select(-c(rural_urban))
#renaming
df <- df %>% rename(rural_urban = urban)

#religion. 1 - 6 matches. Need to make dlhs and nfhs 7, 8, and 96 into one variable 7 "other religion" to match ahs
# make 9 in dlhs and nfhs into 8 to match AHS "no religion"

df$relig_merge <- df$religion

df$relig_merge <- ifelse(df$survey != "AHS" & df$religion == 8, 7, df$relig_merge)

df$relig_merge <- ifelse(df$survey != "AHS" & df$religion == 96, 7, df$relig_merge)

df$relig_merge <- ifelse(df$survey != "AHS" & df$religion == 9, 8, df$relig_merge)

#making three responses ((1) 21 and (2) 99) into NAs

df$relig_merge <- ifelse(df$relig_merge > 8, NA, df$relig_merge)

#removing old religion variable
df <- df %>% select(-c(religion))
#renaming relig_merge to be new religion
df <- df %>% rename(religion = relig_merge)

#scheduled caste/tribe. in all surveys 1 is scheduled caste and 2 is scheduled tribe. 
#making don't know (8) in NFHS into NA 

df$caste_group <- ifelse(df$caste_group == 8, NA, df$caste_group)

#making anything not 1, 2, or NA into 0 (not in a scheduled caste or tribe)
df$caste_group <- ifelse(df$caste_group > 2, 0, df$caste_group)

#education was fixed in first merge

#source of water. Matching on AHS coding.

#   1   piped-water-into-dwelling-yardplot
#2   public-tap-stand-pipe
#3   hand-pump
#4   tubewell-or-borehole
#5   protected-dugwell
#6   unprotected-dug-well
#7 tanker/truck/cart
#8 surface water
#9 other source

#Starting with DLHS (both) that mostly matches. Making current 7 into other (96)
df$water <- df$source_water

df$water <- ifelse(df$survey == "DLHS3" & df$water == 7, 96, df$water)
df$water <- ifelse(df$survey == "DLHS4" & df$water == 7, 96, df$water)

# now making 10 (tanker/truck) and 11 (cart) in DLHS surveys into 7 (tanker/truck/cart)

df$water <- ifelse(df$survey == "DLHS3" & df$water == 10 |df$survey == "DLHS3" & df$water == 11, 7, df$water)
df$water <- ifelse(df$survey == "DLHS4" & df$water == 10 |df$survey == "DLHS4" & df$water == 11, 7, df$water)

#now doing the same with current number 8 (unprotected spring) -- making it other 
df$water <- ifelse(df$survey == "DLHS3" & df$water == 8, 96, df$water)
df$water <- ifelse(df$survey == "DLHS4" & df$water == 8, 96, df$water)

#making surface water (12) match 8
df$water <- ifelse(df$survey == "DLHS3" & df$water == 12, 8, df$water)
df$water <- ifelse(df$survey == "DLHS4" & df$water == 12, 8, df$water)

#making DLHS anything over 8 into "other" group of 9
df$water <- ifelse(df$survey == "DLHS3" & df$water > 8, 9, df$water)
df$water <- ifelse(df$survey == "DLHS4" & df$water > 8, 9, df$water)

#now matching NFHS into AHS and DLHS

#making NFHS 11 and 12 into 1
df <- df %>% mutate(waternew = case_when(survey == "NFHS4" & water == 11 ~ 1,
                                         survey == "NFHS4" & water == 12 ~ 1,
                                         survey == "NFHS5" & water == 11 ~ 1,
                                         survey == "NFHS5" & water == 12 ~ 1,
                                         TRUE ~ water))

#NFHS4 13 and NFHS5 14 becomes 2

df <- df %>% mutate(waternew = case_when(survey == "NFHS4" & water == 13 ~ 2,
                                         survey == "NFHS5" & water == 14 ~ 2,
                                         TRUE ~ waternew))

# remove option 3 from DLHS and AHS -- not asked and move on from there. 

df$waternew <- ifelse(df$waternew == 3, 9, df$waternew)


# Remove surface water (8) from DLHS and AHS and make 9 (other) as well. also not asked.

df$waternew <- ifelse(df$waternew == 8, 9, df$waternew)


# In NFHS 21 becomes 4, 31 becomes 5, 32 becomes 6, 61-62 becomes 7
df <- df %>% mutate(waternew = case_when(survey == "NFHS4" & water == 21 ~ 4,
                                         survey == "NFHS5" & water == 21 ~ 4,
                                         survey == "NFHS4" & water == 31 ~ 5,
                                         survey == "NFHS5" & water == 31 ~ 5,
                                         survey == "NFHS4" & water == 32 ~ 6,
                                         survey == "NFHS5" & water == 32 ~ 6,
                                         survey == "NFHS4" & water == 61 ~ 7,
                                         survey == "NFHS5" & water == 61 ~ 7,
                                         survey == "NFHS4" & water == 62 ~ 7,
                                         survey == "NFHS5" & water == 62 ~ 7,
                                         TRUE ~ waternew))

#Making NFHS 97 (not a dejure resident) into NA

df <- df %>% mutate(waternew = case_when(survey == "NFHS4" & water == 97 ~ NA_real_,
                                         survey == "NFHS5" & water == 97 ~ NA_real_,
                                         TRUE ~ waternew))


#Making NFHS options 13 (NFHS5 only), 41, 42, 43, 51, 71, 72, 92, 96 into other (9)

#tried doing > 7 ~ 9 but made everything under 7 into 0s. Leaving as long now to fix this.

df <- df %>% mutate(watertry = case_when(survey == "NFHS5" & water == 13 ~ 9,
                                         survey == "NFHS4" & water == 41 ~ 9,
                                         survey == "NFHS5" & water == 41 ~ 9,
                                         survey == "NFHS4" & water == 42 ~ 9,
                                         survey == "NFHS5" & water == 42 ~ 9,
                                         survey == "NFHS4" & water == 43 ~ 9,
                                         survey == "NFHS5" & water == 43 ~ 9,
                                         survey == "NFHS4" & water == 51 ~ 9,
                                         survey == "NFHS5" & water == 51 ~ 9,
                                         survey == "NFHS4" & water == 71 ~ 9,
                                         survey == "NFHS5" & water == 71 ~ 9,
                                         survey == "NFHS4" & water == 72 ~ 9,
                                         survey == "NFHS5" & water == 72 ~ 9,
                                         survey == "NFHS5" & water == 92 ~ 9,
                                         survey == "NFHS4" & water == 96 ~ 9,
                                         survey == "NFHS5" & water == 96 ~ 9,
                                         TRUE ~ waternew))

# Now making 4 into 3 and 9 into 8 to make it consecutive.
df$watertry <- ifelse(df$watertry == 4, 3, df$watertry)
df$watertry <- ifelse(df$watertry == 9, 8, df$watertry)

# now making anything over 3 n-1 
df$watertry <- ifelse(df$watertry > 3, (df$watertry) -1, df$watertry)

#getting rid of old / other water variables and renaming watertry into water_source
df <- df %>% select(-c(waternew, water, source_water))

df <- df %>% rename(water_source = watertry)

# now doing type of toilet. Following this coding:
#0 OPEN DEFECATION/NO FACILITY/OPEN SPACE OR FIELD
#1 pour/flush latrine: connected to piped sewer system 
#2 pour/flush latrine: connected to septic tank 
#3 pour/flush latrine: connected to pit latrine 
#4 pour/flush latrine: connected to something else 
#5 pit latrine: ventilated improved pit 
#6 pit latrine: with slab 
#7 pit latrine: open or without slab 
#8 service latrine /dry toilet
#9 OTHER


# Starting with DLHS

#making DLHS 3 option 51 into 0

df$toilet_type <- df$toilet


df <- df %>% mutate(toiletnew = case_when(survey == "DLHS3" & toilet == 51 ~ 0,
                                            survey == "DLHS4" & toilet == 51 ~ 0,
                                            survey == "NFHS4" & toilet == 31 ~ 0,
                                            survey == "NFHS5" & toilet == 31 ~ 0,
                                            toilet == 11 ~ 1,
                                            toilet == 12 ~ 2,
                                            toilet == 13 ~ 3,
                                            toilet == 14 ~ 4,
                                            toilet == 21 ~ 5,
                                            toilet == 22 ~ 6,
                                            toilet == 23 ~ 7,
                                            survey == "DLHS3" & toilet == 41 ~ 8,
                                            survey == "DLHS4" & toilet == 41 ~ 8,
                                            survey == "NFHS4" & toilet == 44 ~ 8,
                                            survey == "NFHS5" & toilet == 44 ~ 8,
                                            toilet == 97 ~ NA_real_,
                                            TRUE ~ 9))

#now adding back in AHS values

df$toilet_type <- ifelse(df$survey == "AHS", df$toilet, df$toiletnew)

#removing extraneous toilet variables
df <- df %>% select(-c(toilet, toiletnew))


# type of delivery

table(df$survey, df$type_delivery)

#making 9s into NAs
df$type_delivery <- ifelse(df$type_delivery == 9, NA, df$type_delivery)

#Making 2s in AHS and DLHS into 1s, all else 0s
df <- df %>% mutate(delivery = case_when(survey == "DLHS3" & type_delivery == 2 ~ 1,
                                         survey == "DLHS4" & type_delivery == 2 ~ 1,
                                         survey == "AHS" & type_delivery == 2 ~ 1,
                                         TRUE ~ 0))

#now adding in original nfhs values
df$delivery <- ifelse(df$survey == "NFHS4", df$type_delivery, df$delivery)
df$delivery <- ifelse(df$survey == "NFHS5", df$type_delivery, df$delivery)

#dropping original type_delivery variable and then renaming delivery 
df <- df %>% select(-c(type_delivery))

df <- df %>% rename(type_delivery = delivery)

#now making place of delivery into coding 1 == Medical 2 == Non-medical

#will go survey by survey 

table(df$survey, df$place_delivery)

#making "other" (96 and 99) into NAs.
df$place_delivery <- ifelse(df$place_delivery > 95, NA, df$place_delivery)

#DLHS3 1-10 is medical, > 10 is not medical
# DLHS4 1-11 is is medical, > 11 is non-medical
#both NFHS > 13 is medical, <= 13 is non-medical
# AHS < 12 is medical, 12 is non-medical


df <- df %>% mutate(placedelivered = case_when(survey == "DLHS3" & place_delivery < 11 ~ 1,
                                               survey == "DLHS4" & place_delivery < 12 ~ 1,
                                               survey == "NFHS4" & place_delivery > 13 ~ 1,
                                               survey == "NFHS5" & place_delivery > 13 ~ 1,
                                               survey == "AHS" & place_delivery < 12 ~ 1,
                                               TRUE ~ 2))

#dropping old place_delivery variable and renaming new one to match
df <- df %>% select(-c(place_delivery))

df <- df %>% rename(place_delivery = placedelivered)

# Looking at who conducted delivery

# Harmonized coding will be 
#1 Medical
#2 Non-medical
#3 None

table(df$survey, df$conducted_delivery)

# DLHS 1 - 3 medical
# NFHS 1 is medical
# AHS 1 - 3 is medical

df <- df %>% mutate(delivered = case_when(survey == "DLHS3" & conducted_delivery < 4 ~ 1,
                                          survey == "DLHS4" & conducted_delivery < 4 ~ 1,
                                          survey == "NFHS4" & conducted_delivery == 1 ~ 1,
                                          survey == "NFHS5" & conducted_delivery == 1 ~ 1,
                                          survey == "AHS" & conducted_delivery < 4 ~ 1,
                                          is.na(conducted_delivery) ~ NA_real_,
                                          TRUE ~ 0))

table(df$delivered)

#removing and renaming
df <- df %>% select(-c(conducted_delivery))
df <- df %>% rename(conducted_delivery = delivered)


# Postpartum check-up

table(df$survey, df$pp_checkup)

#coding choice for "did you receive a post-partum check-up within 48 hours?" 0 == No 1 == Yes

#Making 9s ("don't know") into NA

df$pp_checkup <- ifelse(df$pp_checkup == 9, NA, df$pp_checkup)

#Making all 2s into 0s. then will match across surveys.
df$pp_checkup <- ifelse(df$pp_checkup == 2, 0, df$pp_checkup)

write.csv(df, "harmonized_variables.csv")


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

