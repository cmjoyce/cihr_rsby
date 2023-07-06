setwd("./DLHS")


library(data.table)
library(tidyverse)

# Loading in variables of interest ----------------------------------------


dlhs3 <- fread("dlhs3_numeric.csv", select = c(1:5, 7, 11:16, 20, 24:27, 28:30,
                                               38, 46, 51, 54, 64:67, 81:83, 92:97,
                                               116:145, 150:152, 297:299, 330, 338, 724,
                                               726, 728, 730, 732, 734, 736, 738, 
                                               740, 742, 744:747, 888, 1161:1162, 1164:1165))


#now keeping till merge with dlhs4

# Renaming ----------------------------------------------------------------

dlhs3 <- rename(dlhs3, rural_urban = htype, religion = hv115, caste = hv116a,
                caste_group = hv116b, source_water = hv117, water_treat = hv118, 
                toilet = hv120, type_house = hv124, bpl_card = hv134,
                health_ins = hv135, date = vdate, month = vmonth, year = vyear,
                age = v104, years_school = v113, age_at_marriage = v118,
                age_first_birth = v121, tot_still_births = v128, tot_spont_abort 
                = v129a, tot_induc_abort = v129b, tot_live_births = v134, 
                tot_preg = v135, multiples = v136a, received_anc = v206,
                month_received_anc = v207, num_anc_visits = v208, type_delivery 
                = v229, place_delivery = v230, conducted_delivery = v231, jsy = v239, pp_checkup = v241,
                fem_ster = v401b01, vasectomy = v401b02, iud = v401b03, bcp 
                = v401b04, emerg_contracept = v401b05, inject = v401b06, 
                condom_nirodh = v401b07, female_condom = v401b08, rhythm 
                = v401b09, withdrawal = v401b10, other_method = v401b11, 
                pregnant = v404, using_method = v405, type_method =v406,
                fp_future = v445, dewwt = emwt, dhhwt = hhwt)




# Making numbers ----------------------------------------------------------

dlhs3$state <- as.numeric(dlhs3$state)
#dlhs3$district <- as.numeric(dlhs3$district)
dlhs3$psu <- as.numeric(dlhs3$psu)
dlhs3$hhno <- as.numeric(dlhs3$hhno)
dlhs3$lineno <- as.numeric(dlhs3$lineno)

# fixing DLHS-3 district variable -----------------------------------------

#DLHS-3 combined state and district in variable, with state first number and district last two. Keeping just last two.

dlhs3$district <- str_sub(dlhs3$dist,start= -2)

#renaming dist as state_dist to make clear it is the state/district combo
dlhs3$state_dist <- dlhs3$dist

#dropping old district variable
#dlhs3 <- dlhs3 %>% select(-c(dist))

#adding leading 0s to psu to remove duplicates from accidental same numbers (i.e. psu 31 andd hh 195 getting same id as psu 3 and hh 1195)
dlhs3$psu <- str_pad(dlhs3$psu, 2, "left", "0")



# Creating ID variable ----------------------------------------------------

dlhs3$hh_id <- paste(dlhs3$state,dlhs3$dist,dlhs3$psu,dlhs3$hhno, sep = "")

dlhs3$id_person <- paste(dlhs3$state, dlhs3$dist,dlhs3$psu, dlhs3$hhno, dlhs3$lineno, dlhs3$vsremq, sep = "")



#checking hh and personal IDs are unique
#dlhs3 <- dlhs3 %>% group_by(hh_id)
#dlhs3 <- dlhs3 %>% add_count(hh_id)

#dlhs3 <- dlhs3 %>% group_by(id_person)
#dlhs3 <- dlhs3 %>% add_count(id_person)

#table(dlhs3$n)

#dlhs3 <- dlhs3 %>% select(-c(n))

#no duplicates using ever married woman's serial number in id_person

#dupes <- subset(dlhs3, nn >1)

#comparing to hh variable
df3hh <- fread("DLHS3-HH.csv", select = c(1:8, 676:740))

#fixing district variable
df3hh$district <- str_sub(df3hh$dist,start= -2)

#renaming dist as state_dist to make clear it is the state/district combo
df3hh$state_dist <- df3hh$dist

#dropping old district variable
#df3hh <- df3hh %>% select(-c(dist))
#adding leading 0 to psu
df3hh$psu <- str_pad(df3hh$psu, 2, "left", "0")

df3hh$hh_id <- paste(df3hh$state,df3hh$dist,df3hh$psu,df3hh$hhno, sep = "")
#df3hh <- df3hh %>% group_by(hh_id) %>% add_count(hh_id)

#keeping only wealth index + health insurance variables + linking variables
dlhs3hh <- df3hh %>% select(c(state, dist, psu, hhno, hh_id, hv119a, hv119b, hv119c, hv119d,
                              hv119e, hv119f, hv119g, hv119h, hv119i, hv121, hv123,
                              hv129i, hv129j, hv129k, hv129m, hv129n, hv129o, hv129p,
                              hv129s, hv129t, hv129u, hv129v, hv130,
                              hv136a, hv136b, hv136c, hv136d, hv136e, hv136f, hv136g))

#left join matching on hhid

dlhs3 <- left_join(
  dlhs3,
  dlhs3hh,
  by = c("hh_id", "state", "dist", "psu", "hhno")
  
)

dlhs3 <- rename(dlhs3, boil_water = hv119a, alum_water = hv119b, bleach_water = hv119c, strain_water = hv119d, 
                filter_water = hv119e, electr_purif_water = hv119f, settle_water = hv119g, other_water = hv119h, dk_water = hv119i,
                toilet_shared = hv121, cook_fuel = hv123, radio = hv129i, tv_bw = hv129j, tv_color = hv129k,
                mobile_phone = hv129m, other_phone = hv129n, computer = hv129o, fridge = hv129p, bike = hv129s, motorcycle = hv129t,
                animal_cart = hv129u, car = hv129v, own_agric_land = hv130)


# July 20th 2022 redoing fertility to match NFHS --------------------------

# logic to match NFHS is as follows: most recent termination (miscarriage/stillbirth/abortion) is coded 
#as such. Any termination that happened before that is left as generic T).
#In DLHS coding anything > 1 is a termination

dlhs3$ever_terminated <- ifelse(dlhs3$tot_still_births > 0 | dlhs3$tot_induc_abort > 0 |
                                  dlhs3$tot_spont_abort > 0, 1, 0)

#doing piecemeal coding to get most recent termination. Taking info from most recent pregnancy if it ended in a non-live birth 
dlhs3$miscarriage_abortion_stillbirth <- ifelse(dlhs3$v139_1 > 1, dlhs3$v139_1, 0)

#now looking at people who did not report a termination from their last pregnancy, to see if their second to last pregnancy 
# is their most recent termination

dlhs3$miscarriage_abortion_stillbirth <- ifelse(dlhs3$miscarriage_abortion_stillbirth < 1 & dlhs3$v139_2 > 1,
                                                  dlhs3$v139_2, dlhs3$miscarriage_abortion_stillbirth)

#now looking at third to last pregnancy
dlhs3$miscarriage_abortion_stillbirth <- ifelse(dlhs3$miscarriage_abortion_stillbirth < 1 & dlhs3$v139_3 > 1,
                                                dlhs3$v139_3, dlhs3$miscarriage_abortion_stillbirth)

#now looking at fourth to last pregnancy
dlhs3$miscarriage_abortion_stillbirth <- ifelse(dlhs3$miscarriage_abortion_stillbirth < 1 & dlhs3$v139_4 > 1,
                                                dlhs3$v139_4, dlhs3$miscarriage_abortion_stillbirth)


#now looking at fifth to last pregnancy
dlhs3$miscarriage_abortion_stillbirth <- ifelse(dlhs3$miscarriage_abortion_stillbirth < 1 & dlhs3$v139_5 > 1,
                                                dlhs3$v139_5, dlhs3$miscarriage_abortion_stillbirth)

#now looking at sixth to last pregnancy
dlhs3$miscarriage_abortion_stillbirth <- ifelse(dlhs3$miscarriage_abortion_stillbirth < 1 & dlhs3$v139_6 > 1,
                                                dlhs3$v139_6, dlhs3$miscarriage_abortion_stillbirth)

#months of pregnancy of  last termination occurred is only recorded for induced abortion 
#looking just at those who reported induced abortion for last termination -- induced abortion == 3
dlhs3$month_last_termination_occurred <- ifelse(dlhs3$miscarriage_abortion_stillbirth == 3, dlhs3$v130, NA)

#now looking at month and year of most recent termination. Coding similarly to type of termination.

# logic -- if the most recent pregnancy was a non-live birth and we have a month for the non-live birth that becomes 
# our most recent month of termination
dlhs3$month_last_terminated <- ifelse(dlhs3$v139_1 > 1 & dlhs3$v143a_1 > 0, dlhs3$v143a_1, NA)

#logic -- if we did not fill in most recent termination month from most recent pregnancy month of abortion/birth (i.e. if it 
# is still NA), check if the second most recent pregnancy is a non-live birth, and if so fill in that month of outcome. Otherwise leave as is.
dlhs3$month_last_terminated <- ifelse(is.na(dlhs3$month_last_terminated) & dlhs3$v139_2 > 1, dlhs3$v143a_2, dlhs3$month_last_terminated)

#logic -- same as above, but now moving on to third most recent pregnancy if still unfilled
dlhs3$month_last_terminated<- ifelse(is.na(dlhs3$month_last_terminated) & dlhs3$v139_3 > 1, dlhs3$v143a_3, dlhs3$month_last_terminated)

#logic -- same as above, but now moving on to fourth most recent pregnancy if still unfilled
dlhs3$month_last_terminated <- ifelse(is.na(dlhs3$month_last_terminated) & dlhs3$v139_4 > 1, dlhs3$v143a_4, dlhs3$month_last_terminated)

#logic -- same as above, but now moving on to fifth most recent pregnancy if still unfilled
dlhs3$month_last_terminated <- ifelse(is.na(dlhs3$month_last_terminated) & dlhs3$v139_5 > 1, dlhs3$v143a_5, dlhs3$month_last_terminated)

#logic -- same as above, but now moving on to sixth most recent pregnancy if still unfilled
dlhs3$month_last_terminated <- ifelse(is.na(dlhs3$month_last_terminated) & dlhs3$v139_6 > 1, dlhs3$v143a_6, dlhs3$month_last_terminated)

#now doing the same for last year of termination
# logic -- if the most recent pregnancy was a non-live birth and we have a year for the non-live birth that becomes 
# our most recent year of termination
dlhs3$year_last_terminated <- ifelse(dlhs3$v139_1 > 1 & dlhs3$v143b_1 > 0, dlhs3$v143b_1, NA)

#logic -- if we did not fill in most recent termination year from most recent pregnancy year of abortion/birth (i.e. if it 
# is still NA), check if the second most recent pregnancy is a non-live birth, and if so fill in that year of outcome. Otherwise leave as is.
dlhs3$year_last_terminated <- ifelse(is.na(dlhs3$year_last_terminated) & dlhs3$v139_2 > 1, dlhs3$v143b_2, dlhs3$year_last_terminated)

#logic -- same as above, but now moving on to third most recent pregnancy if still unfilled
dlhs3$year_last_terminated<- ifelse(is.na(dlhs3$year_last_terminated) & dlhs3$v139_3 > 1, dlhs3$v143b_3, dlhs3$year_last_terminated)

#logic -- same as above, but now moving on to fourth most recent pregnancy if still unfilled
dlhs3$year_last_terminated <- ifelse(is.na(dlhs3$year_last_terminated) & dlhs3$v139_4 > 1, dlhs3$v143b_4, dlhs3$year_last_terminated)

#logic -- same as above, but now moving on to fifth most recent pregnancy if still unfilled
dlhs3$year_last_terminated <- ifelse(is.na(dlhs3$year_last_terminated) & dlhs3$v139_5 > 1, dlhs3$v143b_5, dlhs3$year_last_terminated)

#logic -- same as above, but now moving on to sixth most recent pregnancy if still unfilled
dlhs3$year_last_terminated <- ifelse(is.na(dlhs3$year_last_terminated) & dlhs3$v139_6 > 1, dlhs3$v143b_6, dlhs3$year_last_terminated)

#making other such terminations variable using termination cases from v139 pregnancy variables
dlhs3$nonlive1 <- ifelse(dlhs3$v139_1 > 1, 1, NA)
dlhs3$nonlive2 <- ifelse(dlhs3$v139_2 > 1, 1, NA)
dlhs3$nonlive3 <- ifelse(dlhs3$v139_3 > 1, 1, NA)
dlhs3$nonlive4 <- ifelse(dlhs3$v139_4 > 1, 1, NA)
dlhs3$nonlive5 <- ifelse(dlhs3$v139_5 > 1, 1, NA)
dlhs3$nonlive6 <- ifelse(dlhs3$v139_6 > 1, 1, NA)

#making variable summing the number of times a pregnancy was a non-live event
dlhs3 <- dlhs3 %>%
  mutate(terminations = rowSums(across(starts_with("nonlive")) == 1, na.rm = TRUE))

#now making binary variable yes/no (1/0) if women had other terminations during the time frame 
dlhs3$other_terminations <- ifelse(dlhs3$terminations > 1, 1, 0)

#making most recent live birth index child month and year of birth 
#checking if most recent child was a live birth, if yes filling in month of birth. If non-live birth leaving as NA.
dlhs3$index_mob <- ifelse(dlhs3$v139_1 == 1, dlhs3$v143a_1, NA)

#if most recent birth was a nonlive birth we look to the next birth to see if it was live, if so that becomes index child dob
dlhs3$index_mob <- ifelse(is.na(dlhs3$index_mob) & dlhs3$v139_2 == 1, dlhs3$v143a_2, dlhs3$index_mob)

#continuing if first two births were nonlive
dlhs3$index_mob <- ifelse(is.na(dlhs3$index_mob) & dlhs3$v139_3 == 1, dlhs3$v143a_3, dlhs3$index_mob)

#continuing if first three births were nonlive
dlhs3$index_mob <- ifelse(is.na(dlhs3$index_mob) & dlhs3$v139_4 == 1, dlhs3$v143a_4, dlhs3$index_mob)

#continuing if first four births were nonlive
dlhs3$index_mob <- ifelse(is.na(dlhs3$index_mob) & dlhs3$v139_5 == 1, dlhs3$v143a_5, dlhs3$index_mob)

#continuing if first five births were nonlive
dlhs3$index_mob <- ifelse(is.na(dlhs3$index_mob) & dlhs3$v139_6 == 1, dlhs3$v143a_6, dlhs3$index_mob)

#now doing the same for year
#checking if most recent child was a live birth, if yes filling in year of birth. If non-live birth leaving as NA.
dlhs3$index_yob <- ifelse(dlhs3$v139_1 == 1, dlhs3$v143b_1, NA)

#if most recent birth was a nonlive birth we look to the next birth to see if it was live, if so that becomes index child dob
dlhs3$index_yob <- ifelse(is.na(dlhs3$index_yob) & dlhs3$v139_2 == 1, dlhs3$v143b_2, dlhs3$index_yob)

#continuing if first two births were nonlive
dlhs3$index_yob <- ifelse(is.na(dlhs3$index_yob) & dlhs3$v139_3 == 1, dlhs3$v143b_3, dlhs3$index_yob)

#continuing if first three births were nonlive
dlhs3$index_yob <- ifelse(is.na(dlhs3$index_yob) & dlhs3$v139_4 == 1, dlhs3$v143b_4, dlhs3$index_yob)

#continuing if first four births were nonlive
dlhs3$index_yob <- ifelse(is.na(dlhs3$index_yob) & dlhs3$v139_5 == 1, dlhs3$v143b_5, dlhs3$index_yob)

#continuing if first five births were nonlive
dlhs3$index_yob <- ifelse(is.na(dlhs3$index_yob) & dlhs3$v139_6 == 1, dlhs3$v143b_6, dlhs3$index_yob)

# creating a variable to show if index child is alive 
#checking if most recent child was a live birth, if yes filling in if alive. If non-live birth leaving as NA.
dlhs3$index_alive <- ifelse(dlhs3$v139_1 == 1, dlhs3$v145_1, NA)
dlhs3$index_alive <- ifelse(is.na(dlhs3$index_alive) & dlhs3$v139_2 == 1, dlhs3$v145_2, dlhs3$index_alive)
dlhs3$index_alive <- ifelse(is.na(dlhs3$index_alive) & dlhs3$v139_3 == 1, dlhs3$v145_3, dlhs3$index_alive)
dlhs3$index_alive <- ifelse(is.na(dlhs3$index_alive) & dlhs3$v139_4 == 1, dlhs3$v145_4, dlhs3$index_alive)
dlhs3$index_alive <- ifelse(is.na(dlhs3$index_alive) & dlhs3$v139_5 == 1, dlhs3$v145_5, dlhs3$index_alive)
dlhs3$index_alive <- ifelse(is.na(dlhs3$index_alive) & dlhs3$v139_6 == 1, dlhs3$v145_6, dlhs3$index_alive)

#SEE NOTE BELOW IN DLHS4 ABOUT AGE AT DEATH
# creating a variable to show index child age of death
#checking if most recent child was a live birth, if yes filling in if alive. If non-live birth leaving as NA.
#dlhs3$index_agedeath <- ifelse(dlhs3$v139_1 == 1, dlhs3$v146_1, NA)
#dlhs3$index_agedeath <- ifelse(is.na(dlhs3$index_agedeath) & dlhs3$v139_2 == 1, dlhs3$v146_2, dlhs3$index_agedeath)
#dlhs3$index_agedeath <- ifelse(is.na(dlhs3$index_agedeath) & dlhs3$v139_3 == 1, dlhs3$v146_3, dlhs3$index_agedeath)
#dlhs3$index_agedeath <- ifelse(is.na(dlhs3$index_agedeath) & dlhs3$v139_4 == 1, dlhs3$v146_4, dlhs3$index_agedeath)
#dlhs3$index_agedeath <- ifelse(is.na(dlhs3$index_agedeath) & dlhs3$v139_5 == 1, dlhs3$v146_5, dlhs3$index_agedeath)
#dlhs3$index_agedeath <- ifelse(is.na(dlhs3$index_agedeath) & dlhs3$v139_6 == 1, dlhs3$v146_6, dlhs3$index_agedeath)


#dropping v139s and v143s and nonlive variables
dlhs3 <- dlhs3 %>% select(-c(v139_1, v139_2, v139_3, v139_4, v139_5, v139_6, v143a_1, v143a_2, v143a_3, v143a_4, v143a_5, v143a_6,
                             v143b_1, v143b_2, v143b_3, v143b_4, v143b_5, v143b_6, v144_1, v144_2, v144_3, v144_4, v144_5, v144_6,
                             v145_1, v145_2, v145_3, v145_4, v145_5, v145_6, v146_1, v146_2, v146_3, v146_4, v146_5, v146_6,
                             nonlive1, nonlive2, nonlive3, nonlive4, 
                             nonlive5, nonlive6))

#matching on harmonized district ids (from district match file)
#using dlhs3_district_names file created elsewhere

#dropping NAs from districts not in dlhs3

dlhs3_district_names <- dlhs3_district_names %>% filter(!is.na(dlhs3_district_names$DLHS3_id))

dlhs3 <- left_join(dlhs3, dlhs3_district_names, 
                       by = c("state_dist" = "DLHS3_id"))

#this introduced multiple matches to district. Doing distinct to remove the multiples.
dlhs3 <- distinct(dlhs3)

#did a check. Works.
#dlhs3_try_check <- dlhs3_distinct %>% select(c(state, district, state_dist, dist_id))

#DLHS 3 now matches NFHS termination variables

# DLHS-4 Load -------------------------------------------------------------

dlhs4 <- fread("DLHS-4 Women.csv", select = c(1:6, 9:10, 19, 32, 44:102, 
                                              121, 123,149, 159:164, 
                                              180:181, 185,192:197, 216:290, 303, 305, 307, 
                                              483:487, 530, 544, 958:968, 
                                              971, 974:976, 1115, 1275:1278))



# Renaming ----------------------------------------------------------------

dlhs4 <- rename(dlhs4, district = dist, date = qsinterviewdate, month = qsinterviewmonth,
                year = qsinterviewyear, rural_urban = htype, years_school = hv13, religion = hv30,
                caste = hv31a, caste_group = hv31b, source_water = hv32, 
                water_treat = hv33, boil_water = hv34a, alum_water = hv34b, 
                bleach_water = hv34c, strain_water = hv34d, filter_water = hv34e, 
                electr_purif_water = hv34f, settle_water = hv34g, other_water = hv34h, dk_water = hv34i,
                toilet= hv35, toilet_shared = hv36, cook_fuel = hv39,
                type_house = hv40, radio = hv46a, tv = hv46b, computer_no_wifi = hv46c, 
                computer_wifi = hv46d, other_phone = hv46e, mobile_phone = hv46f, 
                fridge = hv46h, bike = hv46k, motorcycle = hv46l, car = hv46m,
                animal_cart = hv46p, machine_cart = hv46q, own_agric_land = hv47,
                bpl_card = hv48,
                health_ins = hv49, age = q105, age_at_marriage = q107, 
                age_first_birth = q121, tot_still_births = q128_sb, 
                tot_induc_abort = q129_indu, tot_spont_abort = q129_spot,
                tot_live_births = q136, tot_preg = q137, multiples = q139b,
                received_anc = q207, month_received_anc =q208_m, 
                num_anc_visits = q209_n, type_delivery = q230, 
                place_delivery = q231, conducted_delivery = q232, jsy = q240a, pp_checkup = q242, 
                fem_ster = q401b1, vasectomy = q401b2, 
                iud = q401b3, bcp_daily = q401b4, bcp_weekly = q401b5,
                emerg_contracept = q401b6, inject = q401b7, 
                condom_nirodh = q401b8,female_condom = q401b9, rhythm = q401b10, 
                withdrawal = q401b11, other_method = q401b14, pregnant = q404, 
                using_method = q405, type_method = q406, fp_future = q445)

#renaming prim_key and primekey_new to match hhi_id and id_person
dlhs4 <- rename(dlhs4, hh_id = prim_key, id_person = primekey_new)

#keeping only variables needed 
dlhs4 <- dlhs4 %>% select(-c(hv37, hv38, hv41, hv42, hv43, hv44, hv45, hv46g, hv46i, hv46j, hv46n, 
                             hv46o, hv46r, hv46s, hv46t, hv47a, hv47b, hv47c))

#dlhs4 <- dlhs4 %>% group_by(id_person)
#dlhs4 <- dlhs4 %>% add_count(id_person)
#table(dlhs4$n)
#dlhs4 <- dlhs4 %>% select(-c(n))
#all unique


# July 20th 2022 redoing terminations calculations ------------------------

# logic to match NFHS is as follows: most recent termination (miscarriage/stillbirth/abortion) is coded 
#as such. Any termination that happened before that is left as generic T).
#In DLHS coding anything > 1 is a termination
dlhs4 <- dlhs4 %>% mutate_if(is.integer,as.numeric)
#dlhs4 <- dlhs4 %>% mutate_if(is.character,as.numeric)
dlhs4$pregnant <- as.numeric(dlhs4$pregnant)
dlhs4$using_method <- as.numeric(dlhs4$using_method)
dlhs4$fp_future <- as.numeric(dlhs4$fp_future)
#making integer64 columns into characters
dlhs4$hh_id <- as.character(dlhs4$hh_id)
dlhs4$id_person <- as.character(dlhs4$id_person)


dlhs4 <- dlhs4 %>% mutate_at(c(1:3, 6:61),as.numeric)

dlhs4$ever_terminated <- ifelse(dlhs4$tot_still_births > 0 | dlhs4$tot_induc_abort > 0 |
                                  dlhs4$tot_spont_abort > 0, 1, 0)

#doing piecemeal coding to get most recent termination. Taking info from most recent pregnancy if it ended in a non-live birth 
dlhs4$miscarriage_abortion_stillbirth <- ifelse(dlhs4$q141_1 > 1, dlhs4$q141_1, 0)

#now looking at people who did not report a termination from their last pregnancy, to see if their second to last pregnancy 
# is their most recent termination

dlhs4$miscarriage_abortion_stillbirth <- ifelse(dlhs4$miscarriage_abortion_stillbirth < 1 & dlhs4$q141_2 > 1,
                                                dlhs4$q141_2, dlhs4$miscarriage_abortion_stillbirth)

#now looking at third to last pregnancy
dlhs4$miscarriage_abortion_stillbirth <- ifelse(dlhs4$miscarriage_abortion_stillbirth < 1 & dlhs4$q141_3 > 1,
                                                dlhs4$q141_3, dlhs4$miscarriage_abortion_stillbirth)

#now looking at fourth to last pregnancy
dlhs4$miscarriage_abortion_stillbirth <- ifelse(dlhs4$miscarriage_abortion_stillbirth < 1 & dlhs4$q141_4 > 1,
                                                dlhs4$q141_4, dlhs4$miscarriage_abortion_stillbirth)


#now looking at fifth to last pregnancy
dlhs4$miscarriage_abortion_stillbirth <- ifelse(dlhs4$miscarriage_abortion_stillbirth < 1 & dlhs4$q141_5 > 1,
                                                dlhs4$q141_5, dlhs4$miscarriage_abortion_stillbirth)

#now looking at sixth to last pregnancy
dlhs4$miscarriage_abortion_stillbirth <- ifelse(dlhs4$miscarriage_abortion_stillbirth < 1 & dlhs4$q141_6 > 1,
                                                dlhs4$q141_6, dlhs4$miscarriage_abortion_stillbirth)

#months of pregnancy of  last termination occurred is only recorded for induced abortion 
#looking just at those who reported induced abortion for last termination -- induced abortion == 3
dlhs4$month_last_termination_occurred <- ifelse(dlhs4$miscarriage_abortion_stillbirth == 3, dlhs4$q130, NA)

#now looking at month and year of most recent termination. Coding similarly to type of termination.

# logic -- if the most recent pregnancy was a non-live birth and we have a month for the non-live birth that becomes 
# our most recent month of termination
dlhs4$month_last_terminated <- ifelse(dlhs4$q141_1 > 1 & dlhs4$q145m_1 > 0, dlhs4$q145m_1, NA)

#logic -- if we did not fill in most recent termination month from most recent pregnancy month of abortion/birth (i.e. if it 
# is still NA), check if the second most recent pregnancy is a non-live birth, and if so fill in that month of outcome. Otherwise leave as is.
dlhs4$month_last_terminated <- ifelse(is.na(dlhs4$month_last_terminated) & dlhs4$q141_2 > 1, dlhs4$q145m_2, dlhs4$month_last_terminated)

#logic -- same as above, but now moving on to third most recent pregnancy if still unfilled
dlhs4$month_last_terminated<- ifelse(is.na(dlhs4$month_last_terminated) & dlhs4$q141_3 > 1, dlhs4$q145m_3, dlhs4$month_last_terminated)

#logic -- same as above, but now moving on to fourth most recent pregnancy if still unfilled
dlhs4$month_last_terminated <- ifelse(is.na(dlhs4$month_last_terminated) & dlhs4$q141_4 > 1, dlhs4$q145m_4, dlhs4$month_last_terminated)

#logic -- same as above, but now moving on to fifth most recent pregnancy if still unfilled
dlhs4$month_last_terminated <- ifelse(is.na(dlhs4$month_last_terminated) & dlhs4$q141_5 > 1, dlhs4$q145m_5, dlhs4$month_last_terminated)

#logic -- same as above, but now moving on to sixth most recent pregnancy if still unfilled
dlhs4$month_last_terminated <- ifelse(is.na(dlhs4$month_last_terminated) & dlhs4$q141_6 > 1, dlhs4$q145m_6, dlhs4$month_last_terminated)

#now doing the same for last year of termination
# logic -- if the most recent pregnancy was a non-live birth and we have a year for the non-live birth that becomes 
# our most recent year of termination
dlhs4$year_last_terminated <- ifelse(dlhs4$q141_1 > 1 & dlhs4$q145y_1 > 0, dlhs4$q145y_1, NA)

#logic -- if we did not fill in most recent termination year from most recent pregnancy year of abortion/birth (i.e. if it 
# is still NA), check if the second most recent pregnancy is a non-live birth, and if so fill in that year of outcome. Otherwise leave as is.
dlhs4$year_last_terminated <- ifelse(is.na(dlhs4$year_last_terminated) & dlhs4$q141_2 > 1, dlhs4$q145y_2, dlhs4$year_last_terminated)

#logic -- same as above, but now moving on to third most recent pregnancy if still unfilled
dlhs4$year_last_terminated<- ifelse(is.na(dlhs4$year_last_terminated) & dlhs4$q141_3 > 1, dlhs4$q145y_3, dlhs4$year_last_terminated)

#logic -- same as above, but now moving on to fourth most recent pregnancy if still unfilled
dlhs4$year_last_terminated <- ifelse(is.na(dlhs4$year_last_terminated) & dlhs4$q141_4 > 1, dlhs4$q145y_4, dlhs4$year_last_terminated)

#logic -- same as above, but now moving on to fifth most recent pregnancy if still unfilled
dlhs4$year_last_terminated <- ifelse(is.na(dlhs4$year_last_terminated) & dlhs4$q141_5 > 1, dlhs4$q145y_5, dlhs4$year_last_terminated)

#logic -- same as above, but now moving on to sixth most recent pregnancy if still unfilled
dlhs4$year_last_terminated <- ifelse(is.na(dlhs4$year_last_terminated) & dlhs4$q141_6 > 1, dlhs4$q145y_6, dlhs4$year_last_terminated)

#making most recent live birth index child month and year of birth 
#checking if most recent child was a live birth, if yes filling in month of birth. If non-live birth leaving as NA.
dlhs4$index_mob <- ifelse(dlhs4$q141_1 == 1, dlhs4$q145m_1, NA)

#if most recent birth was a nonlive birth we look to the next birth to see if it was live, if so that becomes index child dob
dlhs4$index_mob <- ifelse(is.na(dlhs4$index_mob) & dlhs4$q141_2 == 1, dlhs4$q145m_2, dlhs4$index_mob)

#continuing if first two births were nonlive
dlhs4$index_mob <- ifelse(is.na(dlhs4$index_mob) & dlhs4$q141_3 == 1, dlhs4$q145m_3, dlhs4$index_mob)

#continuing if first three births were nonlive
dlhs4$index_mob <- ifelse(is.na(dlhs4$index_mob) & dlhs4$q141_4 == 1, dlhs4$q145m_4, dlhs4$index_mob)

#continuing if first four births were nonlive
dlhs4$index_mob <- ifelse(is.na(dlhs4$index_mob) & dlhs4$q141_5 == 1, dlhs4$q145m_5, dlhs4$index_mob)

#continuing if first five births were nonlive
dlhs4$index_mob <- ifelse(is.na(dlhs4$index_mob) & dlhs4$q141_6 == 1, dlhs4$q145m_6, dlhs4$index_mob)

#now doing the same for year
#checking if most recent child was a live birth, if yes filling in year of birth. If non-live birth leaving as NA.
dlhs4$index_yob <- ifelse(dlhs4$q141_1 == 1, dlhs4$q145y_1, NA)

#if most recent birth was a nonlive birth we look to the next birth to see if it was live, if so that becomes index child dob
dlhs4$index_yob <- ifelse(is.na(dlhs4$index_yob) & dlhs4$q141_2 == 1, dlhs4$q145y_2, dlhs4$index_yob)

#continuing if first two births were nonlive
dlhs4$index_yob <- ifelse(is.na(dlhs4$index_yob) & dlhs4$q141_3 == 1, dlhs4$q145y_3, dlhs4$index_yob)

#continuing if first three births were nonlive
dlhs4$index_yob <- ifelse(is.na(dlhs4$index_yob) & dlhs4$q141_4 == 1, dlhs4$q145y_4, dlhs4$index_yob)

#continuing if first four births were nonlive
dlhs4$index_yob <- ifelse(is.na(dlhs4$index_yob) & dlhs4$q141_5 == 1, dlhs4$q145y_5, dlhs4$index_yob)

#continuing if first five births were nonlive
dlhs4$index_yob <- ifelse(is.na(dlhs4$index_yob) & dlhs4$q141_6 == 1, dlhs4$q145y_6, dlhs4$index_yob)

# creating a variable to show if index child is alive 
#checking if most recent child was a live birth, if yes filling in if alive. If non-live birth leaving as NA.
dlhs4$index_alive <- ifelse(dlhs4$q141_1 == 1, dlhs4$q152_1, NA)
dlhs4$index_alive <- ifelse(is.na(dlhs4$index_alive) & dlhs4$q141_2 == 1, dlhs4$q152_2, dlhs4$index_alive)
dlhs4$index_alive <- ifelse(is.na(dlhs4$index_alive) & dlhs4$q141_3 == 1, dlhs4$q152_3, dlhs4$index_alive)
dlhs4$index_alive <- ifelse(is.na(dlhs4$index_alive) & dlhs4$q141_4 == 1, dlhs4$q152_4, dlhs4$index_alive)
dlhs4$index_alive <- ifelse(is.na(dlhs4$index_alive) & dlhs4$q141_5 == 1, dlhs4$q152_5, dlhs4$index_alive)
dlhs4$index_alive <- ifelse(is.na(dlhs4$index_alive) & dlhs4$q141_6 == 1, dlhs4$q152_6, dlhs4$index_alive)


# creating a variable to show index child age of death

#August 4th. Not using age of death for now as these are separate from still births so not necessary.

#putting into format to match dlhs3 and nfhs where it is three digits, those starting with 1 signify days, those starting with 2 signify months
#dlhs4$agedeath1 <- ifelse(dlhs4$q153_1 == 1, (str_pad(dlhs4$q153_days_1, 2, "right", "0")), NA)

#checking if most recent child was a live birth, if yes filling in if alive. If non-live birth leaving as NA.
#need to fill in separately age of death in months or days depending on what was reported
#dlhs4$index_agedeath <- ifelse(dlhs4$q141_1 == 1, dlhs4$v146_1, NA)
#dlhs4$index_agedeath <- ifelse(is.na(dlhs4$index_agedeath) & dlhs4$q141_2 == 1, dlhs4$v146_2, dlhs4$index_agedeath)
#dlhs4$index_agedeath <- ifelse(is.na(dlhs4$index_agedeath) & dlhs4$q141_3 == 1, dlhs4$v146_3, dlhs4$index_agedeath)
#dlhs4$index_agedeath <- ifelse(is.na(dlhs4$index_agedeath) & dlhs4$q141_4 == 1, dlhs4$v146_4, dlhs4$index_agedeath)
#dlhs4$index_agedeath <- ifelse(is.na(dlhs4$index_agedeath) & dlhs4$q141_5 == 1, dlhs4$v146_5, dlhs4$index_agedeath)
#dlhs4$index_agedeath <- ifelse(is.na(dlhs4$index_agedeath) & dlhs4$q141_6 == 1, dlhs4$v146_6, dlhs4$index_agedeath)



#making other such terminations variable using termination cases from q141 pregnancy variables
dlhs4$nonlive1 <- ifelse(dlhs4$q141_1 > 1, 1, NA)
dlhs4$nonlive2 <- ifelse(dlhs4$q141_2 > 1, 1, NA)
dlhs4$nonlive3 <- ifelse(dlhs4$q141_3 > 1, 1, NA)
dlhs4$nonlive4 <- ifelse(dlhs4$q141_4 > 1, 1, NA)
dlhs4$nonlive5 <- ifelse(dlhs4$q141_5 > 1, 1, NA)
dlhs4$nonlive6 <- ifelse(dlhs4$q141_6 > 1, 1, NA)

#making variable summing the number of times a pregnancy was a non-live event
dlhs4 <- dlhs4 %>%
  mutate(terminations = rowSums(across(starts_with("nonlive")) == 1, na.rm = TRUE))

#now making binary variable yes/no (1/0) if women had other terminations during the time frame 
dlhs4$other_terminations <- ifelse(dlhs4$terminations > 1, 1, 0)

#dropping q141s and v143s and nonlive variables
dlhs4 <- dlhs4 %>% select(-c(q141_1, q141_2, q141_3, q141_4, q141_5, q141_6, q145m_1, q145m_2, q145m_3, q145m_4, q145m_5, q145m_6,
                             q145y_1, q145y_2, q145y_3, q145y_4, q145y_5, q145y_6, q146m_1, q146m_2, q146m_3, q146m_4, q146m_5, q146m_6,
                             q146y_1, q146y_2, q146y_3, q146y_4, q146y_5, q146y_6, q147_1, q147_2, q147_3, q147_4, q147_5, q147_6,
                             q148_1, q148_2, q148_3, q148_4, q148_5, q148_6, q149_1, q149_2, q149_3, q149_4, q149_5, q149_6,
                             q150_1, q150_2, q150_3, q150_4, q150_5, q150_6, q151_1, q151_2, q151_3, q151_4, q151_5, q151_6,
                             q152_1, q152_2, q152_3, q152_4, q152_5, q152_6, q153_1, q153_2, q153_3, q153_4, q153_5, 
                             q153_days_1, q153_days_2, q153_days_3, q153_days_4, q153_days_5, q153_months_1, q153_months_2, 
                             q153_months_3, q153_months_4, q153_months_5, 
                             nonlive1, nonlive2, nonlive3, nonlive4, 
                             nonlive5, nonlive6))

#adding in harmonized district ids. From district harmonization file. Using dlhs4_district_names that was created there.

########### Using DLHS3 state_district variable for harmonizing all districts. Keeping district and making state_dist variable
#need to make separate state_district variable in dlhs4 to match dlh3 district
dlhs4$district <- str_pad(dlhs4$district, 2, "left", "0") 

dlhs4$state_dist <- paste(dlhs4$state, dlhs4$district, sep ="")
dlhs4$state_dist <- as.numeric(dlhs4$state_dist)


dlhs4_district_names <- dlhs4_district_names %>% filter(!is.na(dlhs4_district_names$DLHS4_id))

dlhs4 <- left_join(dlhs4, dlhs4_district_names, 
                   by = c("state_dist" = "DLHS4_id"))


#this introduced multiple matches to district. Doing distinct to remove the multiples.
dlhs4 <- distinct(dlhs4)

#check works
#dlhs4 <- dlhs4_try %>% select(c(state, district, state_dist, dist_id))

# matching dlhs 3 and 4 variables -----------------------------------------

dlhs3 <- select(dlhs3, -c("hhno", "lineno"))

names(dlhs3)
names(dlhs4)

dlhs3 <- select(dlhs3, -c(dist))

# examining variables -----------------------------------------------------
#moving dlhs3 and dlhs4 variables to match 
dlhs3 <- dlhs3 %>% relocate(hh_id, .after = psu)
dlhs3 <- dlhs3 %>% relocate(id_person, .after = hh_id)
dlhs3 <- dlhs3 %>% relocate(date, .after = id_person)
dlhs3 <- dlhs3 %>% relocate(month, .after = date)
dlhs3 <- dlhs3 %>% relocate(year, .after = month)
dlhs3 <- dlhs3 %>% relocate(date, .after = id_person)

dlhs4 <- dlhs4 %>% relocate(years_school, .after = age)
dlhs4 <- dlhs4 %>% relocate(tot_spont_abort, .before = tot_induc_abort)

#combining dlhs4 weekly and daily bcp into one variable -- 1 is yes, 2 is no
table(dlhs4$bcp_daily)
table(dlhs4$bcp_weekly)

dlhs4$bcp <- ifelse(dlhs4$bcp_daily == 1 | dlhs4$bcp_weekly == 1, 1, 0)

#dropping weekly and daily bcp variables
dlhs4 <- select(dlhs4, -c("bcp_daily", "bcp_weekly"))

#back to moving variables
dlhs4 <- dlhs4 %>% relocate(bcp, .after = iud)

names(dlhs3)
names(dlhs4)
#moving dlhs3 health insurance variables
dlhs3 <- dlhs3 %>% relocate(c(56:62), .after = health_ins)

#removing serial number variables in dlhs3
dlhs3 <- select(dlhs3, -c("vsremq", "vsrshq"))


#renaming DLHS3 health insurance variables

dlhs3 <- rename(dlhs3, ESIS = hv136a, CGHS = hv136b, reimburse = hv136c, CHIP = hv136d,
                mediclaim = hv136e, other_priv = hv136f, other = hv136g)

#renaming DLHS4 health insurance variables
dlhs4 <- rename(dlhs4, ESIS = hv50a, RSBY = hv50b, CGHS = hv50c, reimburse = hv50d, CHIP = hv50e,
                mediclaim = hv50f, other_priv = hv50g, other = hv50h)

#renaming month and year of birth/abortion variables
#dlhs3 <- dlhs3 %>% rename(month1 = v143a_1, month2 = v143a_2, month3 = v143a_3, month4 = v143a_4, month5 = v143a_5, month6 = v143a_6,
#                          year1 = v143b_1, year2 = v143b_2, year3 = v143b_3, year4 = v143b_4, year5 = v143b_5, year6 = v143b_6)

#dlhs4 <- dlhs4 %>% rename(month1 = q145m_1, month2 = q145m_2, month3 = q145m_3, month4 = q145m_4, month5 = q145m_5, month6 = q145m_6,
#                          year1 = q145y_1, year2 = q145y_2, year3 = q145y_3, year4 = q145y_4, year5 = q145y_5, year6 = q145y_6)

#adding in survey variable
dlhs3$survey <- c("DLHS3")
#dlhs4$survey <- as.character(dlhs4$survey)
dlhs4$survey <- c("DLHS4")


#making numeric
dlhs3 <- dlhs3 %>% mutate_if(is.integer,as.numeric)
#dlhs4 <- dlhs4 %>% mutate_if(is.integer, as.numeric)
#dlhs4 <- dlhs4 %>% mutate_if(is.character, as.numeric)


#making DLHS3 RSBY column 
dlhs3$RSBY <- c(0)
dlhs3 <- dlhs3 %>% relocate(RSBY, .after = ESIS)

#looking at variables that were asked differently in DLHS3 and 4
table(dlhs3$state)
table(dlhs4$state)

#Telangana added in 2014, it is number 36 with all other states the same

table(dlhs3$district)
#Moving dlhs3 district to after state
dlhs3 <- dlhs3 %>% relocate(district, .after = state)



#removing old district variable and replacing with new one
#dlhs4 <- select(dlhs4, -c("district"))
#dlhs4 <- rename(dlhs4, district = state_dist)

dlhs4 <- dlhs4 %>% relocate(state_dist, .after = district)
dlhs3 <- dlhs3 %>% relocate(state_dist, .after = district)
###################################################################



table(dlhs4$psu)
table(dlhs3$psu)
#dropping PSU from both datasets
#dlhs3 <- select(dlhs3, -c("psu"))
#dlhs4 <- select(dlhs4, -c("psu"))

#September 8 keeping PSU

table(dlhs3$date) ##one day labeled as 55, turning to NA
dlhs3$date <- ifelse(dlhs3$date > 31, NA, dlhs3$date)

table(dlhs4$date)

table(dlhs3$month)
table(dlhs4$month)

table(dlhs3$year) #one entry listed as 8, making NA
dlhs3$year <- ifelse(dlhs3$year < 2007, NA, dlhs3$year)

table(dlhs4$year)

#will remove NAs once combined
table(dlhs3$religion)
table(dlhs4$religion)

table(dlhs3$caste)
table(dlhs4$caste)

table(dlhs3$caste_group)
table(dlhs4$caste_group)

#making NA variables match for caste group
dlhs3$caste_group <- ifelse(dlhs3$caste_group == 4, 6, dlhs3$caste_group)

#these response options are different, combining dlhs 3 responses 1 and 2 into new 1 to match.
table(dlhs3$source_water)
table(dlhs4$source_water)

dlhs3$source_water <- ifelse(dlhs3$source_water == 2, 1, dlhs3$source_water)
dlhs3$source_water <- ifelse(dlhs3$source_water == 3, 2, dlhs3$source_water)
dlhs3$source_water <- ifelse(dlhs3$source_water == 4, 3, dlhs3$source_water)
dlhs3$source_water <- ifelse(dlhs3$source_water == 5, 4, dlhs3$source_water)
dlhs3$source_water <- ifelse(dlhs3$source_water == 6, 5, dlhs3$source_water)
dlhs3$source_water <- ifelse(dlhs3$source_water == 7, 6, dlhs3$source_water)
dlhs3$source_water <- ifelse(dlhs3$source_water == 8, 7, dlhs3$source_water)
dlhs3$source_water <- ifelse(dlhs3$source_water == 9, 8, dlhs3$source_water)
dlhs3$source_water <- ifelse(dlhs3$source_water == 10, 9, dlhs3$source_water)
dlhs3$source_water <- ifelse(dlhs3$source_water == 11, 10, dlhs3$source_water)
dlhs3$source_water <- ifelse(dlhs3$source_water == 12, 11, dlhs3$source_water)
dlhs3$source_water <- ifelse(dlhs3$source_water == 13, 12, dlhs3$source_water)
dlhs3$source_water <- ifelse(dlhs3$source_water == 14, 13, dlhs3$source_water)

#there are 3 99s in dlhs3. Removing
dlhs3$source_water <- ifelse(dlhs3$source_water == 99, NA, dlhs3$source_water)

table(dlhs3$water_treat)#removing the 3, that must be error
dlhs3$water_treat <- ifelse(dlhs3$water_treat == 3, NA, dlhs3$water_treat)

table(dlhs4$water_treat)

table(dlhs3$toilet)
table(dlhs4$toilet)

#note items do not match here if looking at this variable
table(dlhs3$type_house)
table(dlhs4$type_house)

table(dlhs3$bpl_card)
table(dlhs4$bpl_card)

table(dlhs3$health_ins)
table(dlhs4$health_ins)

table(dlhs3$ESIS)
table(dlhs4$ESIS)

table(dlhs3$CGHS)
table(dlhs4$CGHS)
table(dlhs3$reimburse)
table(dlhs4$reimburse)
table(dlhs3$CHIP)
table(dlhs4$CHIP)
table(dlhs3$RSBY)
table(dlhs4$RSBY)
table(dlhs3$mediclaim)
table(dlhs4$mediclaim)
table(dlhs3$other_priv)
table(dlhs4$other_priv)
table(dlhs3$other)
table(dlhs4$other)

table(dlhs3$age)
table(dlhs4$age)

#hard to parse, we might need a better metric but the education questions don't match
#will come back to this

table(dlhs3$years_school)
table(dlhs4$years_school)

table(dlhs3$age_at_marriage)
table(dlhs4$age_at_marriage)

table(dlhs3$age_first_birth)
table(dlhs4$age_first_birth)

table(dlhs3$num_still_births)
table(dlhs4$num_still_births)

#in DLHS 4 the 0s are coded as NA for the following variables
table(dlhs3$num_spont_abort)
table(dlhs4$num_spont_abort)

table(dlhs3$num_induc_abort)
table(dlhs4$num_induc_abor)

#end NA / 0 coding difference

table(dlhs3$tot_live_births)
table(dlhs4$tot_live_births)

table(dlhs3$tot_preg)
table(dlhs4$tot_preg)

table(dlhs3$multiples)
table(dlhs4$multiples)

table(dlhs3$received_anc)
table(dlhs4$received_anc)

table(dlhs3$num_anc_visits) #98 and 99 are NAs
table(dlhs4$num_anc_visits)

table(dlhs3$type_delivery)
table(dlhs4$type_delivery)

#these are coded differently between surveys
table(dlhs3$place_delivery)
table(dlhs4$place_delivery)

table(dlhs3$jsy)
table(dlhs4$jsy)

table(dlhs3$pp_checkup)
table(dlhs4$pp_checkup)

table(dlhs3$fem_ster)
table(dlhs4$fem_ster)
table(dlhs3$vasectomy)
table(dlhs4$vasectomy)
table(dlhs3$iud)
table(dlhs4$iud)

table(dlhs3$bcp) #changing coding to match dlhs4
dlhs3$bcp <- ifelse(dlhs3$bcp == 1, 1, 0)

table(dlhs4$bcp)

table(dlhs3$emerg_contracept)
table(dlhs4$emerg_contracept)
table(dlhs3$inject)
table(dlhs4$inject)
table(dlhs3$condom_nirodh)
table(dlhs4$condom_nirodh)
table(dlhs3$female_condom)
table(dlhs4$female_condom)
table(dlhs3$rhythm)
table(dlhs4$rhythm)
table(dlhs3$withdrawal)
table(dlhs4$withdrawal)
table(dlhs3$other_method)
table(dlhs4$other_method)

table(dlhs3$pregnant)
table(dlhs4$pregnant)

table(dlhs3$using_method)
table(dlhs4$using_method)

table(dlhs3$type_method)
table(dlhs4$type_method)
table(dlhs3$fp_future)
table(dlhs4$fp_future)

#moving weight variables so in same place in both datasets
#dlhs3 <- dlhs3 %>% relocate(dhhwt, .after = fp_future)
dlhs3 <- dlhs3 %>% relocate(dewwt, .after = dhhwt)
dlhs3 <- dlhs3 %>% relocate(shhwt, .after = dewwt)
dlhs3 <- dlhs3 %>% relocate(sewwt, .after = shhwt)

#dropping dlhs4 q128 and q129 as they're now both renamed as other variables
dlhs4 <- dlhs4 %>% select(-c(q128, q129))

#moving insurance to correct place
dlhs3 <- dlhs3 %>% relocate(mediclaim, .after = CHIP)
dlhs3 <- dlhs3 %>% relocate(other_priv, .after = mediclaim)
dlhs3 <- dlhs3 %>% relocate(other, .after = other_priv)

#moving weight variables so in same place in both datasets
dlhs4 <- dlhs4 %>% relocate(dhhwt, .before = dewwt)
dlhs4 <- dlhs4 %>% relocate(shhwt, .before = sewwt)

#dropping dlhs3 and 4 v130/q140 variable as it now has a different name
dlhs3 <- dlhs3 %>% select(-c(v130))
dlhs4 <- dlhs4 %>% select(-c(q130))

#dropping dlhs4 q231a and qq231 variables
dlhs4 <- dlhs4 %>% select(-c(q231a, qq231))

#moving index_mob and index_yob after tot_live_births
dlhs3 <- dlhs3 %>% relocate(index_mob, .after = tot_live_births)
dlhs3 <- dlhs3 %>% relocate(index_yob, .after = index_mob)

dlhs4 <- dlhs4 %>% relocate(index_mob, .after = tot_live_births)
dlhs4 <- dlhs4 %>% relocate(index_yob, .after = index_mob)

#moving dlhs3 weight variables which got mixed up 
dlhs3 <- dlhs3 %>% relocate(pregnant:sewwt, .after = other_method)

#harmonizing wealth index variables
names(dlhs3)
names(dlhs4)

#making dlhs3 both types of tv variables (b&w and color) into one
table(dlhs3$tv_bw)
table(dlhs3$tv_color)

length(which(dlhs3$tv_bw == 1 & dlhs3$tv_color == 1))

dlhs3 <- dlhs3 %>% mutate(tv = case_when(tv_bw == 1 ~ 1,
                                         tv_color == 1 ~ 1,
                                         TRUE ~ 0))

#dropping old variables
dlhs3 <- dlhs3 %>% select(-c(tv_bw, tv_color))

#now combining dlhs4 computers w/ and w/o internet into one variable
dlhs4 <- dlhs4 %>% mutate(computer = case_when(computer_wifi == 1 ~ 1,
                                               computer_no_wifi == 1 ~ 1,
                                               TRUE ~ 0))
#dropping old variables
dlhs4 <- dlhs4 %>% select(-c(computer_no_wifi, computer_wifi))

#dropping dlhs4 machine cart variable. Only animal cart is shared across surveys.
dlhs4 <- dlhs4 %>% select(-c(machine_cart))

library(janitor)
compare_df_cols(dlhs3, dlhs4)

#making numeric to match
dlhs4$received_anc <- as.numeric(dlhs4$received_anc)
dlhs4$month_received_anc <- as.numeric(dlhs4$month_received_anc)
dlhs4$num_anc_visits <- as.numeric(dlhs4$num_anc_visits)
dlhs4$type_delivery <- as.numeric(dlhs4$type_delivery)
dlhs4$place_delivery <- as.numeric(dlhs4$place_delivery)
dlhs4$conducted_delivery <- as.numeric(dlhs4$conducted_delivery)
dlhs4$jsy <- as.numeric(dlhs4$jsy)
dlhs4$pp_checkup <- as.numeric(dlhs4$pp_checkup)
dlhs4$index_alive <- as.numeric(dlhs4$index_alive)
dlhs4$index_mob <- as.numeric(dlhs4$index_mob)
dlhs4$index_yob <- as.numeric(dlhs4$index_yob)
dlhs4$multiples <- as.numeric(dlhs4$multiples)
dlhs4$miscarriage_abortion_stillbirth <- as.numeric(dlhs4$miscarriage_abortion_stillbirth)
dlhs4$month_last_terminated <- as.numeric(dlhs4$month_last_terminated)
dlhs4$year_last_terminated <- as.numeric(dlhs4$year_last_terminated)

#dlhs3$district <- as.numeric(dlhs3$district)
#dlhs3$district <- str_pad(dlhs3$district, 2, "left", "0")
dlhs3$psu <- as.numeric(dlhs3$psu)

#merging into one dataframe
dlhs <- bind_rows(dlhs3, dlhs4)

#removing x variable introduced as signifier
#dlhs <- dlhs %>% select(-c("X"))

#moving id_person to first variable
dlhs <- dlhs %>% relocate(id_person, .before = state)



write.csv(dlhs, "dlhs.csv")

#next steps adding in NFHS/AHS/Policy variables. Add in district enrollment into RSBY last 

dlhs <- read.csv("dlhs.csv")

#preliminary matching with NFHS
dlhs <- dlhs %>% select(-c("hh_id"))
dlhs <- rename(dlhs, caseid = id_person)
dlhs <- dlhs %>% relocate(caseid, .before = state)
dlhs <- dlhs %>% relocate(age, .after = year)
dlhs <- dlhs %>% relocate(years_school, .after = caste_group)
#dlhs <- dlhs %>% select(-c("water_treat"))
dlhs <- dlhs %>% select(-c("type_house"))
dlhs <- dplyr::rename(dlhs, bpl = bpl_card)
dlhs <- dlhs %>% relocate(sewwt, .after = shhwt)
dlhs <- dlhs %>% select(-c("age_at_marriage")) #if we add this back in we can use s309 in nfhs4
dlhs <- dlhs %>% relocate(tot_live_births, .after = age_first_birth)
#dropping total pregnancies and multiples variable
dlhs <- dlhs %>% select(-c("tot_preg", "multiples"))


# making pregnancies dataset ----------------------------------------------
#matching across dlhs3 and dlhs4
# making dlhs3 pregnancy dataset ------------------------------------------

dlhs3_preg <- fread("dlhs3_numeric.csv", select = c(1:5, 26, 86:145))

# making dlhs4 pregnancies dataset ----------------------------------------
dlhs4_preg <- fread("DLHS-4 Women.csv", select = c(1:5, 186:291))


#dlhs4 has pregnancy confirmation variable that dlhs3 does not. removing.
dlhs4_preg <- dlhs4_preg %>% select(-c(starts_with("q140a")))

#dropping dlhs4 anc/ultrasound/abortion info that was not asked in dlhs3
#starts with 147, 148, 149, 150, 151
dlhs4_preg <- dlhs4_preg %>% select(-c(starts_with("q147"), starts_with("q148"), starts_with("q149"), 
                                       starts_with("q150"), starts_with("q151")))

#looking at age of death variables. dlhs4 has more than dlhs3
table(dlhs3_preg$v146_1)

#dlhs4 has indicator variable saying whether pregnancy ended in days or months (q153_1)
table(dlhs4_preg$q153_1)
table(dlhs4_preg$q153_days_1)
length(which(dlhs4_preg$q153_days_1 >= 0))

#removing q153_year variable as it's empty
dlhs4_preg <- dlhs4_preg %>% select(-c("q153_year"))

#dropping dlhs3 pregnancy index
dlhs3_preg <- dlhs3_preg %>% select(-c(starts_with("v138")))

### dropping age at death variables for now. May re-add
dlhs3_preg <- dlhs3_preg %>% select(-c(starts_with("v146")))

dlhs4_preg <- dlhs4_preg %>% select(-c(starts_with("q153")))

#dropping dlhs4 respondent month of age at time of event. dlhs3 only has years
dlhs4_preg <- dlhs4_preg %>% select(-c(starts_with("q146m")))

#renaming so they match

dlhs3_preg <- dlhs3_preg %>% rename(outcome = starts_with("v139"),
                                    singlemult = starts_with("v141_"), 
                                    birthorder = starts_with("v141a"),
                                    sex = starts_with("v142"),
                                    month_event = starts_with("v143a"),
                                    year_event = starts_with("v143b"),
                                    mother_age_event = starts_with("v144"),
                                    alive = starts_with("v145"))


dlhs4_preg <- dlhs4_preg %>% rename(outcome = starts_with("q141"),
                                    singlemult = starts_with("q143_"), 
                                    birthorder = starts_with("q143a"),
                                    sex = starts_with("q144"),
                                    month_event = starts_with("q145m"),
                                    year_event = starts_with("q145y"),
                                    mother_age_event = starts_with("q146"),
                                    alive = starts_with("q152"))

#adding leading 0 to psu
dlhs3_preg$psu <- str_pad(dlhs3_preg$psu, 2, "left", "0")

#making id variable
dlhs3_preg$id_person <- paste(dlhs3_preg$state, dlhs3_preg$dist,dlhs3_preg$psu, dlhs3_preg$hhno, dlhs3_preg$lineno, dlhs3_preg$vsremq,sep = "")
#dropping variables used to make id
#dlhs3_preg <- dlhs3_preg %>% select(-c(state, dist, psu, hhno, lineno))
dlhs3_preg <- dlhs3_preg %>% select(-c(hhno, lineno))
dlhs3_preg$dist <- str_sub(dlhs3_preg$dist,start= -2)
dlhs3_preg$dist <- as.numeric(dlhs3_preg$dist)

#same for dlhs4, but variable already exists 
dlhs4_preg <- rename(dlhs4_preg, id_person = primekey_new)

#dropping variables not needed anymore
#dlhs4_preg <- dlhs4_preg %>% select(-c(state, dist, psu, prim_key))
dlhs4_preg <- dlhs4_preg %>% select(-c(prim_key))


#moving dlhs3 id_person to first spot
dlhs3_preg <- dlhs3_preg %>% relocate(id_person, .before = outcome1)

dlhs4_preg$id_person <- as.character(dlhs4_preg$id_person)

#making numeric
dlhs3_preg <- dlhs3_preg %>% mutate_if(is.integer,as.numeric)
dlhs4_preg <- dlhs4_preg %>% mutate_if(is.integer, as.numeric)
dlhs4_preg <- dlhs4_preg %>% mutate_if(is.character, as.numeric)

dlhs3_preg <- dlhs3_preg %>% select(-c(vsremq))
dlhs3_preg$psu <- as.numeric(dlhs3_preg$psu)

dlhs3_preg$survey <- c("dlhs3")
dlhs4_preg$survey <- c("dlhs4")



#merging
dlhs_preg <- rbind(dlhs3_preg, dlhs4_preg)

#saving
write.csv(dlhs_preg, "dlhs_preg.csv")

dlhs_preg <- read.csv("dlhs_preg.csv")

#will only keep most recent pregnancy to match nfhs, but keeping all for now. In wide will need to switch to long.

#making variable in dlhs4 to match dlhs3 age at death
# in dlhs3 a leading 1 signifies age of death in days, a 2 gives it in month
#in dlhs4 this is in a separate variable -- q153. Adding 0s so it matches

dlhs4_preg$age_death_1 <- str_pad(dlhs4_preg$q153_days_1, 2, "left", "0")
dlhs4_preg$age_death_2 <- str_pad(dlhs4_preg$q153_days_2, 2, "left", "0")
dlhs4_preg$age_death_3 <- str_pad(dlhs4_preg$q153_days_3, 2, "left", "0")
dlhs4_preg$age_death_4 <- str_pad(dlhs4_preg$q153_days_4, 2, "left", "0")
dlhs4_preg$age_death_5 <- str_pad(dlhs4_preg$q153_days_5, 2, "left", "0")
dlhs4_preg$age_death_6 <- str_pad(dlhs4_preg$q153_days_6, 2, "left", "0")

dlhs4_preg$agedied1 <- ifelse(dlhs4_preg$q153_1 == 1, paste(dlhs4_preg$q153_1, dlhs4_preg$age_death_1, sep =""), NA)  
dlhs4$state_dist <- as.numeric(dlhs4$state_dist)


# Archive -----------------------------------------------------------------

# Looking at birth outcomes -----------------------------------------------
#NOT USING AS OF JULY 20

#this question asked about all births from Jan 1 2004, which is about 3-4 years for each person depending on when they were interviewed

# 1 is live birth, 2 is still birth, 3 is induced abortion, 4 is spontaneous abortion

table(dlhs3$v139_1)
table(dlhs3$v139_2)
table(dlhs3$v139_3)
table(dlhs3$v139_4)
table(dlhs3$v139_5)
table(dlhs3$v139_6)


dlhs3fert <- dlhs3 %>% select(c(id_person, v139_1, v139_2, v139_3, v139_4, v139_5, v139_6))

#save as csv for cluster

#write.csv(dlhs3fert, "dlhs3fert.csv")

#below is run on cluster

# recoding for counting and summing. Not most efficient but works.
dlhs3$kid1 <- ifelse(dlhs3$v139_1 == "1", "B", NA)
dlhs3$kid1 <- ifelse(dlhs3$v139_1 == "2", "S", dlhs3$kid1)
dlhs3$kid1 <- ifelse(dlhs3$v139_1 == "3", "A", dlhs3$kid1)
dlhs3$kid1 <- ifelse(dlhs3$v139_1 == "4", "M", dlhs3$kid1)

dlhs3$kid2 <- ifelse(dlhs3$v139_2 == "1", "B", NA)
dlhs3$kid2 <- ifelse(dlhs3$v139_2 == "2", "S", dlhs3$kid2)
dlhs3$kid2 <- ifelse(dlhs3$v139_2 == "3", "A", dlhs3$kid2)
dlhs3$kid2 <- ifelse(dlhs3$v139_2 == "4", "M", dlhs3$kid2)

dlhs3$kid3 <- ifelse(dlhs3$v139_3 == "1", "B", NA)
dlhs3$kid3 <- ifelse(dlhs3$v139_3 == "2", "S", dlhs3$kid3)
dlhs3$kid3 <- ifelse(dlhs3$v139_3 == "3", "A", dlhs3$kid3)
dlhs3$kid3 <- ifelse(dlhs3$v139_3 == "4", "M", dlhs3$kid3)

dlhs3$kid4 <- ifelse(dlhs3$v139_4 == "1", "B", NA)
dlhs3$kid4 <- ifelse(dlhs3$v139_4 == "2", "S", dlhs3$kid4)
dlhs3$kid4 <- ifelse(dlhs3$v139_4 == "3", "A", dlhs3$kid4)
dlhs3$kid4 <- ifelse(dlhs3$v139_4 == "4", "M", dlhs3$kid4)

dlhs3$kid5 <- ifelse(dlhs3$v139_5 == "1", "B", NA)
dlhs3$kid5 <- ifelse(dlhs3$v139_5 == "2", "S", dlhs3$kid5)
dlhs3$kid5 <- ifelse(dlhs3$v139_5 == "3", "A", dlhs3$kid5)
dlhs3$kid5 <- ifelse(dlhs3$v139_5 == "4", "M", dlhs3$kid5)

dlhs3$kid6 <- ifelse(dlhs3$v139_6 == "1", "B", NA)
dlhs3$kid6 <- ifelse(dlhs3$v139_6 == "2", "S", dlhs3$kid6)
dlhs3$kid6 <- ifelse(dlhs3$v139_6 == "3", "A", dlhs3$kid6)
dlhs3$kid6 <- ifelse(dlhs3$v139_6 == "4", "M", dlhs3$kid6)

#making sum variables across the 139 columns
dlhs3 <- dlhs3 %>%
  mutate(abortion = rowSums(across(starts_with("kid")) == "A", na.rm = TRUE)) %>%
  mutate(miscarriage = rowSums(across(starts_with("kid")) == "M", na.rm = TRUE)) %>%
  mutate(stillbirth = rowSums(across(starts_with("kid")) == "S", na.rm = TRUE)) %>%
  mutate(birth = rowSums(across(starts_with("kid")) == "B", na.rm = TRUE))

#ran above on cluster

#reading in fertility only data
dlhs3fert <- read.csv("dlhs3fertility.csv")

#keeping only abortions/stillbirths/miscarriages/births variables
dlhs3fert <- dlhs3fert %>% select(c("id_person", "abortion", "miscarriage", "stillbirth", "birth"))

#making a character to match dlhs3 for merge
dlhs3fert$id_person <- as.character(dlhs3fert$id_person)

#adding in fertility variables
dlhs3 <- left_join(
  dlhs3,
  dlhs3fert,
  by = c("id_person")
)

# removing the 139 columns now that it's been calculated
dlhs3 <- dlhs3 %>% select(-c("v139_1", "v139_2", "v139_3", "v139_4", "v139_5", "v139_6"))

# calculating fertility outcomes since 2008 -------------------------------
#July 20th no longer using this fertility outcome code. See below.

#making sum variables across the 141 (preg outcome) columns
dlhs4fert <- dlhs4 %>% select(c(id_person, q141_1, q141_2, q141_3, q141_4, q141_5, q141_6))

#save as csv for cluster

#write.csv(dlhs4fert, "dlhs4fert.csv")

#below is run on cluster

# recoding for counting and summing. Not most efficient but works.
dlhs4$kid1 <- ifelse(dlhs4$v139_1 == "1", "B", NA)
dlhs4$kid1 <- ifelse(dlhs4$v139_1 == "2", "S", dlhs4$kid1)
dlhs4$kid1 <- ifelse(dlhs4$v139_1 == "3", "A", dlhs4$kid1)
dlhs4$kid1 <- ifelse(dlhs4$v139_1 == "4", "M", dlhs4$kid1)

dlhs4$kid2 <- ifelse(dlhs4$v139_2 == "1", "B", NA)
dlhs4$kid2 <- ifelse(dlhs4$v139_2 == "2", "S", dlhs4$kid2)
dlhs4$kid2 <- ifelse(dlhs4$v139_2 == "3", "A", dlhs4$kid2)
dlhs4$kid2 <- ifelse(dlhs4$v139_2 == "4", "M", dlhs4$kid2)

dlhs4$kid3 <- ifelse(dlhs4$v139_3 == "1", "B", NA)
dlhs4$kid3 <- ifelse(dlhs4$v139_3 == "2", "S", dlhs4$kid3)
dlhs4$kid3 <- ifelse(dlhs4$v139_3 == "3", "A", dlhs4$kid3)
dlhs4$kid3 <- ifelse(dlhs4$v139_3 == "4", "M", dlhs4$kid3)

dlhs4$kid4 <- ifelse(dlhs4$v139_4 == "1", "B", NA)
dlhs4$kid4 <- ifelse(dlhs4$v139_4 == "2", "S", dlhs4$kid4)
dlhs4$kid4 <- ifelse(dlhs4$v139_4 == "3", "A", dlhs4$kid4)
dlhs4$kid4 <- ifelse(dlhs4$v139_4 == "4", "M", dlhs4$kid4)

dlhs4$kid5 <- ifelse(dlhs4$v139_5 == "1", "B", NA)
dlhs4$kid5 <- ifelse(dlhs4$v139_5 == "2", "S", dlhs4$kid5)
dlhs4$kid5 <- ifelse(dlhs4$v139_5 == "3", "A", dlhs4$kid5)
dlhs4$kid5 <- ifelse(dlhs4$v139_5 == "4", "M", dlhs4$kid5)

dlhs4$kid6 <- ifelse(dlhs4$v139_6 == "1", "B", NA)
dlhs4$kid6 <- ifelse(dlhs4$v139_6 == "2", "S", dlhs4$kid6)
dlhs4$kid6 <- ifelse(dlhs4$v139_6 == "3", "A", dlhs4$kid6)
dlhs4$kid6 <- ifelse(dlhs4$v139_6 == "4", "M", dlhs4$kid6)

#making sum variables across the 139 columns
dlhs4 <- dlhs4 %>%
  mutate(abortion = rowSums(across(starts_with("kid")) == "A", na.rm = TRUE)) %>%
  mutate(miscarriage = rowSums(across(starts_with("kid")) == "M", na.rm = TRUE)) %>%
  mutate(stillbirth = rowSums(across(starts_with("kid")) == "S", na.rm = TRUE)) %>%
  mutate(birth = rowSums(across(starts_with("kid")) == "B", na.rm = TRUE))

#ran above on cluster

#reading in fertility only data
dlhs4fert <- read.csv("dlhs4fertility.csv")

#keeping only aboritons/stillbirths/miscarriages/births variables
dlhs4fert <- dlhs4fert %>% select(c("id_person", "abortion", "miscarriage", "stillbirth", "birth"))

#making a character to match dlhs4 for merge
dlhs4fert$id_person <- as.character(dlhs4fert$id_person)


#adding in fertility variables
dlhs4 <- left_join(
  dlhs4,
  dlhs4fert,
  by = c("id_person")
)


# removing the 141 columns now that it's been calculated
dlhs4 <- dlhs4 %>% select(-c("q141_1", "q141_2", "q141_3", "q141_4", "q141_5", "q141_6"))



