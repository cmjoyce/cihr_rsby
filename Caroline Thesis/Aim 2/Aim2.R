library(tidyverse)
library(cowplot)
library(did)
library(survey)
library(etwfe)

setwd("./Caroline Thesis/Aim 2")

df <- read.csv("df_updated_primary_w_socioeconomic.csv")


#RSBY enrollment for aim 2

#plotting wealth quintiles by state

#plotting rsby coverage by wealth quintile and state

df %>% ggplot(mapping = aes(x = as.factor(state))) + geom_bar(aes(fill = as.factor(rsby)), position = "fill") + facet_grid(. ~ wi_quintile)

#keeping only rsby == yes for plot

df$state_labeled <- factor(df$state, 
               levels = c(2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 26, 27, 28, 
                          29, 31, 32, 33, 34),
               labels = c("Himachal Pradesh", "Punjab", "Chandigarh", "Uttarakhand", "Haryana", "Rajasthan", "Uttar Pradesh", "Bihar", "Sikkim",
                          "Arunachal Pradesh", "Manipur", "Mizoram", "Tripura", "Meghalaya", "Assam", "West Bengal", "Jharkhand", "Odisha",
                          "Chhattisgarh", "Madhya Pradesh", "Maharashtra", "Andhra Pradesh", "Karnataka", "Goa", "Kerala", "Tamil Nadu", "Puducherry",
                          "Andaman & Nicobar Islands"))

df$wiquint_labeled <- factor(df$wi_quintile,
                             levels = c(1, 2, 3, 4, 5),
                             labels = c("1 (Poorest)", "2", "3", "4", "5 (Richest)"))


ggplot(data=df, aes(x=state_labeled, y=rsby)) + 
  stat_summary(fun = mean, geom = "bar") +
  #stat_summary(aes(label=round(..y..,2)), fun.y=mean, geom="text", vjust = -0.5)+
  #ylim(0.0, 0.3)+
  ylab("Percent enrolled in RSBY") +
  xlab("State") +
  theme_cowplot(11)+
  #panel_border() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

#splitting by wealth quintile
ggplot(data=df, aes(x=state_labeled, y=rsby)) + 
  stat_summary(fun = mean, geom = "bar") +
  facet_wrap(. ~wiquint_labeled) + #Split by another variable
  ylab("Percent enrolled in RSBY") +
  xlab("State") +
  theme_cowplot(11)+
  panel_border() +
  theme(axis.text.x=element_text(angle=60, hjust=1))


# Assigning treatment -----------------------------------------------------

#districts <- read.csv("District_Codebook.csv")
#glimpse(districts)

#districts <- unique(districts)
#write.csv(districts,"districts_codebook_unique.csv")

districts <- read.csv("districts_codebook_unique.csv")

#add in states from df

states_distid <- df %>% select(c(state, dist_id))
states_distid <- unique(states_distid)

dist <- left_join(districts, states_distid, by = "dist_id")

#fixing district duplicates that were missed in initial harmonization

#Bijapur district in Chhattisgarh was created in 2007. There is also a Bijapur district in Karnataka. Reverting to original district to match across surveys.
dist$dist_id <- ifelse(dist$dist_id == 82 & dist$state == 22, 129, dist$dist_id)

dist$namefix <- ifelse(dist$dist_id == 129 & dist$namefix == "Bijapur", "Dantewada", dist$namefix)

#Error in initial district harmonization. State Chhattisgarh original ID 2211 is labeled in district names as "Garhwa". This is an error and it is correctly
#labeled as Raipur in original dataset. Dist_ID should be 466. Relabeling 

dist$dist_id <- ifelse(dist$dist_id == 190 & dist$state == 22, 466, dist$dist_id)

dist$namefix <- ifelse(dist$dist_id == 466 & dist$namefix == "Garhwa", "Raipur", dist$namefix)

##now load in long district enrollment

enrollment <- read.csv("LongDistrictEnrollment.csv")

# create state variable to match

enrollment <- enrollment %>% mutate(state_match =  case_when(State == "Andhra Pradesh" ~ 27,
                                                            State == "Arunachal Pradesh" ~ 12,
                                                            State == "Assam" ~ 18,
                                                            State == "Bihar" ~ 10,
                                                            State == "Chandigarh" ~ 4,
                                                            State == "Chhattisgarh" ~ 22,
                                                            State == "Delhi" ~ 7,
                                                            State == "Goa" ~ 29,
                                                            State == "Gujarat" ~ 24,
                                                            State == "Haryana" ~ 6,
                                                            State == "Himachal Pradesh" ~ 2,
                                                            State == "Jammu and Kashmir" ~ 1,
                                                            State == "Jharkand" ~ 20,
                                                            State == "Karnataka" ~ 28,
                                                            State == "Kerala" ~ 31,
                                                            State == "Madhya Pradesh" ~ 23,
                                                            State == "Maharashtra" ~ 26,
                                                            State == "Manipur" ~ 14,
                                                            State == "Meghalaya" ~ 17,
                                                            State == "Mizoram" ~ 15,
                                                            State == "Nagaland" ~ 13,
                                                            State == "Orrisa" ~ 21,
                                                            State == "Pondicherry" ~ 33,
                                                            State == "Punjab" ~ 3,
                                                            State == "Rajasthan" ~ 8,
                                                            State == "Tamil Nadu" ~ 32,
                                                            State == "Tripura" ~ 16,
                                                            State == "Uttar Pradesh" ~ 9,
                                                            State == "Uttrakhand" ~ 5,
                                                            State == "West Bengal" ~ 19))
 

#creating binary access variable 
enrollment <- enrollment %>% mutate(access_2010 = case_when(!is.na(Enrolled_2010) ~ 1,
                                                            TRUE ~ 0),
                                    access_2011 = case_when(!is.na(Enrolled_2011) ~ 1,
                                                            TRUE ~ 0),
                                    access_2012 = case_when(!is.na(Enrolled_2012) ~ 1,
                                                            TRUE ~ 0),
                                    access_2013 = case_when(!is.na(Enrolled_2013) ~ 1,
                                                            TRUE ~ 0),
                                    access_2014 = case_when(!is.na(Enrolled_2014) ~ 1,
                                                            TRUE ~ 0),
                                    access_2015 = case_when(!is.na(Enrolled_2015) ~ 1,
                                                            TRUE ~ 0),
                                    access_2016 = case_when(!is.na(Enrolled_2016) ~ 1,
                                                            TRUE ~ 0))

# fuzzy match on name of district
library(fuzzyjoin)

#dropping districts without states (districts not in pregnancy dataset)
dist <- dist %>% filter(!is.na(state))

#matching on state and district. Allow for up to two letters different for fuzzy match.
check <- stringdist_left_join(dist, enrollment, by = c("namefix" = "District", "state" = "state_match"), max_dist = 2,
                              distance_col = "distance")

#dropping states that do not match
check <- check %>% filter(state.distance == 0)

#checking districts that didn't find a match with anti-join
nomatch <- stringdist_anti_join(dist, enrollment, by = c("namefix" = "District", "state" = "state_match"), max_dist = 2)

#manually renaming enrollment file to match
enroll_match <- read.csv("LongDistrictEnrollment_MATCH_names.csv")

# create state variable to match

enroll_match <- enroll_match %>% mutate(state_match =  case_when(State == "Andhra Pradesh" ~ 27,
                                                             State == "Arunachal Pradesh" ~ 12,
                                                             State == "Assam" ~ 18,
                                                             State == "Bihar" ~ 10,
                                                             State == "Chandigarh" ~ 4,
                                                             State == "Chhattisgarh" ~ 22,
                                                             State == "Delhi" ~ 7,
                                                             State == "Goa" ~ 29,
                                                             State == "Gujarat" ~ 24,
                                                             State == "Haryana" ~ 6,
                                                             State == "Himachal Pradesh" ~ 2,
                                                             State == "Jammu and Kashmir" ~ 1,
                                                             State == "Jharkand" ~ 20,
                                                             State == "Karnataka" ~ 28,
                                                             State == "Kerala" ~ 31,
                                                             State == "Madhya Pradesh" ~ 23,
                                                             State == "Maharashtra" ~ 26,
                                                             State == "Manipur" ~ 14,
                                                             State == "Meghalaya" ~ 17,
                                                             State == "Mizoram" ~ 15,
                                                             State == "Nagaland" ~ 13,
                                                             State == "Orrisa" ~ 21,
                                                             State == "Pondicherry" ~ 33,
                                                             State == "Punjab" ~ 3,
                                                             State == "Rajasthan" ~ 8,
                                                             State == "Tamil Nadu" ~ 32,
                                                             State == "Tripura" ~ 16,
                                                             State == "Uttar Pradesh" ~ 9,
                                                             State == "Uttrakhand" ~ 5,
                                                             State == "West Bengal" ~ 19))


#creating binary access variable 
enroll_match <- enroll_match %>% mutate(access_2010 = case_when(!is.na(Enrolled_2010) ~ 1,
                                                            TRUE ~ 0),
                                    access_2011 = case_when(!is.na(Enrolled_2011) ~ 1,
                                                            TRUE ~ 0),
                                    access_2012 = case_when(!is.na(Enrolled_2012) ~ 1,
                                                            TRUE ~ 0),
                                    access_2013 = case_when(!is.na(Enrolled_2013) ~ 1,
                                                            TRUE ~ 0),
                                    access_2014 = case_when(!is.na(Enrolled_2014) ~ 1,
                                                            TRUE ~ 0),
                                    access_2015 = case_when(!is.na(Enrolled_2015) ~ 1,
                                                            TRUE ~ 0),
                                    access_2016 = case_when(!is.na(Enrolled_2016) ~ 1,
                                                            TRUE ~ 0))


match <- stringdist_left_join(dist, enroll_match, by = c("namefix" = "District", "state" = "state_match"), max_dist = 2,
                              distance_col = "distance")

match_check <- match %>% select(c(dist_id, namefix, state, District, State, namefix.distance, state.distance,
                                  distance))

#filtering out states that don't match
match_check <- match_check %>% filter(state.distance == 0)

#checking districts that didn't find a match with anti-join
nomatch <- stringdist_anti_join(dist, enroll_match, by = c("namefix" = "District", "state" = "state_match"), max_dist = 2)

#filtering out states sikkim (11) and tamil nadu (32) from no match
nomatch <- nomatch %>% filter(state != 11 & state !=32)

#renaming Kalaburagi as Gulbarga in dist
dist$namefix <- ifelse(dist$namefix == "Kalaburagi","Gulbarga", dist$namefix)

#re-running match_check above

#filtering out non-matches
match_check <- match_check %>% filter(namefix.distance < 2)

match_check <- match_check %>% select(-c(namefix.distance, state.distance, distance))

#now match with df for enrollment. Starting at beginning and doing it clean.
states_distid <- df %>% select(c(state, dist_id))
states_distid <- unique(states_distid)

dist <- left_join(districts, states_distid, by = "dist_id")
#dropping districts without states (districts not in pregnancy dataset)
dist <- dist %>% filter(!is.na(state))

#fixing district duplicates that were missed in initial harmonization

#Bijapur district in Chhattisgarh was created in 2007. There is also a Bijapur district in Karnataka. Reverting to original district to match across surveys.
dist$dist_id <- ifelse(dist$dist_id == 82 & dist$state == 22, 129, dist$dist_id)

dist$namefix <- ifelse(dist$dist_id == 129 & dist$namefix == "Bijapur", "Dantewada", dist$namefix)

#Error in initial district harmonization. State Chhattisgarh original ID 2211 is labeled in district names as "Garhwa". This is an error and it is correctly
#labeled as Raipur in original dataset. Dist_ID should be 466. Relabeling 

dist$dist_id <- ifelse(dist$dist_id == 190 & dist$state == 22, 466, dist$dist_id)

dist$namefix <- ifelse(dist$dist_id == 466 & dist$namefix == "Garhwa", "Raipur", dist$namefix)


#renaming Kalaburagi as Gulbarga in dist
dist$namefix <- ifelse(dist$namefix == "Kalaburagi","Gulbarga", dist$namefix)

#found error in matching in district harmonization. DLHS3 state_dist == 2211 should be Raipur (466) not Garhwa (190). Fixing
#df$dist_id <- ifelse(df$survey == "DLHS3" & df$state_dist == 2211, 466, df$dist_id)

#manually renaming enrollment file to match
enroll_match <- read.csv("LongDistrictEnrollment_MATCH_names.csv")

# create state variable to match

enroll_match <- enroll_match %>% mutate(state_match =  case_when(State == "Andhra Pradesh" ~ 27,
                                                                 State == "Arunachal Pradesh" ~ 12,
                                                                 State == "Assam" ~ 18,
                                                                 State == "Bihar" ~ 10,
                                                                 State == "Chandigarh" ~ 4,
                                                                 State == "Chhattisgarh" ~ 22,
                                                                 State == "Delhi" ~ 7,
                                                                 State == "Goa" ~ 29,
                                                                 State == "Gujarat" ~ 24,
                                                                 State == "Haryana" ~ 6,
                                                                 State == "Himachal Pradesh" ~ 2,
                                                                 State == "Jammu and Kashmir" ~ 1,
                                                                 State == "Jharkand" ~ 20,
                                                                 State == "Karnataka" ~ 28,
                                                                 State == "Kerala" ~ 31,
                                                                 State == "Madhya Pradesh" ~ 23,
                                                                 State == "Maharashtra" ~ 26,
                                                                 State == "Manipur" ~ 14,
                                                                 State == "Meghalaya" ~ 17,
                                                                 State == "Mizoram" ~ 15,
                                                                 State == "Nagaland" ~ 13,
                                                                 State == "Orrisa" ~ 21,
                                                                 State == "Pondicherry" ~ 33,
                                                                 State == "Punjab" ~ 3,
                                                                 State == "Rajasthan" ~ 8,
                                                                 State == "Tamil Nadu" ~ 32,
                                                                 State == "Tripura" ~ 16,
                                                                 State == "Uttar Pradesh" ~ 9,
                                                                 State == "Uttrakhand" ~ 5,
                                                                 State == "West Bengal" ~ 19))


#creating binary access variable 
enroll_match <- enroll_match %>% mutate(access_2010 = case_when(!is.na(Enrolled_2010) ~ 1,
                                                                TRUE ~ 0),
                                        access_2011 = case_when(!is.na(Enrolled_2011) ~ 1,
                                                                TRUE ~ 0),
                                        access_2012 = case_when(!is.na(Enrolled_2012) ~ 1,
                                                                TRUE ~ 0),
                                        access_2013 = case_when(!is.na(Enrolled_2013) ~ 1,
                                                                TRUE ~ 0),
                                        access_2014 = case_when(!is.na(Enrolled_2014) ~ 1,
                                                                TRUE ~ 0),
                                        access_2015 = case_when(!is.na(Enrolled_2015) ~ 1,
                                                                TRUE ~ 0),
                                        access_2016 = case_when(!is.na(Enrolled_2016) ~ 1,
                                                                TRUE ~ 0))


match <- stringdist_left_join(dist, enroll_match, by = c("namefix" = "District", "state" = "state_match"), max_dist = 2,
                              distance_col = "distance")
match <- match %>% select(-c(X))

match <- match %>% filter(state.distance == 0)

#filtering out non-matches
match <- match %>% filter(namefix.distance < 2)

match <- match %>% select(-c(namefix.distance, state.distance, distance))

range(match$dist_id)
range(df$dist_id)

#found duplicate in df districts. Kheri (300) Lakhimpur Kheri (332) are all the same districts. Renaming and fixing to just be 332 and correct name.
df$dist_id <- ifelse(df$dist_id == 300, 332, df$dist_id )



#trying match with df on small columns
matchdf <- df %>% select(caseid, state, dist_id, state_dist, year_of_intr, survey, district)

#removing match state_match columns, state is now in it and can be used
match <- match %>% select(-c(state_match))

#removing A&N islands 
matchdf <- matchdf %>% filter(state != 34)

#removing sikkim
matchdf <- matchdf %>% filter(state != 11)

#removing pondicherry
matchdf <- matchdf %>% filter(state != 33)
match <- match %>% filter(state != 33)

#removing tamil nadu since only 2 districts are represented in enrollment
matchdf <- matchdf %>% filter(state != 32)
match <- match %>% filter(state != 32)

check_match <- left_join(matchdf, match, by = c("dist_id", "state"))
length(which(is.na(check_match$namefix)))

noname <- check_match %>% filter(is.na(namefix))
table(noname$state)

#filtering out observations we know do not match
noname <- noname %>% filter(!dist_id %in% nomatch$dist_id)

check_match <- check_match %>% filter(!is.na(namefix))

#gives 1.96 million pregnancies in states in which we have enrollment data  

#count number of years enrolled for each observation
check_match <- check_match %>% 
  rowwise() %>% 
  mutate(accessyears = sum(c(access_2010, access_2011, access_2012, access_2013, access_2014, access_2015, access_2016)))

#now do for entire dataset

#removing A&N islands 
df_aim2 <- df %>% filter(state != 34)

#removing sikkim
df_aim2 <- df_aim2 %>% filter(state != 11)

#removing pondicherry
df_aim2 <- df_aim2 %>% filter(state != 33)

#removing tamil nadu since only 2 districts are represented in enrollment
df_aim2 <- df_aim2 %>% filter(state != 32)

df_treatment <- left_join(df_aim2, match, by = c("dist_id", "state"))
length(which(is.na(df_treatment$namefix)))

df_treatment <- df_treatment %>% filter(!is.na(namefix))

#gives 1.95 million pregnancies in states in which we have enrollment data  

#count number of years enrolled for each observation
df_treatment <- df_treatment %>% 
  rowwise() %>% 
  mutate(accessyears = sum(c(access_2010, access_2011, access_2012, access_2013, access_2014, access_2015, access_2016)))

#writing csv to assign treatment and access years
#write.csv(df_treatment, "df_treatment_assigned.csv")

#df <- df_treatment
df <- read.csv("df_treatment_assigned.csv")



#creating treatment variable where assignment is first year RSBY was available. If never available treatment is 0

df <- df %>% mutate(treatment = case_when(access_2010 == 1 ~ 2010,
                                          access_2010 == 0 & access_2011 == 1 ~ 2011,
                                          access_2010 == 0 & access_2011 == 0 & access_2012 == 1 ~ 2012,
                                          access_2010 == 0 & access_2011 == 0 & access_2012 == 0 & access_2013 == 1 ~ 2013,
                                          access_2010 == 0 & access_2011 == 0 & access_2012 == 0 & access_2013 == 0 &
                                            access_2014 == 1 ~ 2014,
                                          access_2010 == 0 & access_2011 == 0 & access_2012 == 0 & access_2013 == 0 &
                                            access_2014 == 0 & access_2015 == 1 ~ 2015,
                                          access_2010 == 0 & access_2011 == 0 & access_2012 == 0 & access_2013 == 0 &
                                            access_2014 == 0 & access_2015 == 0 & access_2016 == 1 ~ 2016,
                                          TRUE ~ 0))

table(df$treatment)
names(df)
df <- df %>% select(-c(X.1, primary_fix.y))
#creating early / mid / late adopter change. Using up to 2010 / 2012 / 2014 as cutoffs

df <-df %>% mutate(treat = case_when(df$treatment == 0 ~ 0,
                                     df$treatment == 2010 ~ 1,
                                     df$treatment == 2011 ~ 2,
                                     df$treatment == 2012 ~ 2,
                                     df$treatment == 2013 ~ 3,
                                     df$treatment == 2014 ~ 3,
                                     TRUE ~ NA_real_))

#looking at districts enrolled for each outcome year, writing csv
dist_years_sampled <- table(df$dist_id, df$outcome_year)
#write.csv(dist_years_sampled, "dist_years_sampled.csv")
dist_years_sampled <- read.csv("dist_years_sampled.csv")
dist_years_sampled <- dist_years_sampled %>% rename(districs = X)
dist_years_sampled <- dist_years_sampled %>% rename("2004" = X2004, "2005" = X2005, "2006" = X2006, "2007" = X2007,
                                                    "2008" = X2008, "2009" = X2009, "2010" = X2010, "2011" = X2011, 
                                                    "2012" = X2012, "2013" = X2013, "2014" = X2014, "2015" = X2015,
                                                    "2016" = X2016, "2017" = X2017, "2018" = X2018, "2019" = X2019)

#creating count
dist_years_sampled <- dist_years_sampled %>% 
  rowwise() %>% 
  mutate(years_sampled = sum(2:17))

dist_years_sampled$years_sampled <-  rowSums(dist_years_sampled[2:17] > 0, na.rm = TRUE)
#write.csv(dist_years_sampled, "dist_years_sampled.csv")

dist_years_sampled <- read.csv("dist_years_sampled.csv")

df_distgrouped <- df %>% group_by(dist_id)

# Looking at rates across years -------------------------------------------

library(did)
library(survey)
library(cowplot)

#visualizing rates of stillbirth, abortion, and miscarriage across years.

#setting design for survey glm
#making strata variable from rural/urban
df$strat_rurb <- ifelse(df$rural_urban == 0, 2, 1)
table(df$strat_rurb)

df$treated <- ifelse(df$treatment > 0, 1, 0)

design <- svydesign(data = df, ids = ~psu2, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)

sb_rate <- svyby(~sb, ~outcome_year*treated, design, svymean, vartype=c("se","ci"), na.rm.all = TRUE)

sb_rate$sb_per1000 <- sb_rate$sb*1000
sb_rate$ci_l_per1000 <- sb_rate$ci_l*1000
sb_rate$ci_u_per1000 <- sb_rate$ci_u*1000
sb_rate$se_per1000 <- sb_rate$se*1000

mycolors_wi <- c("#024b7a", "#44b7c2", "#5ddf6c", "#458e48", "#e67e00")
mycolors_prim <- c("#e67e00", "#024b7a")
mycolors_caste <- c("#e67e00", "#44b7c2", "#024b7a")

stillbirth_wi <- ggplot(data = sb_rate, mapping = aes(x= outcome_year, y = sb_per1000, color = as.factor(treated))) + geom_point() + 
  #geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+
  geom_line() + 
  scale_y_continuous(limits = c(0, 50), breaks = c(0, 25, 50, 75, 100))+
  geom_vline(xintercept = 2010, linetype = "dotdash")+
  #scale_color_manual(values = mycolors_wi,name = "Wealth Quintile", breaks =c("1", "2", "3", "4", "5"), 
  #                   labels = c("0.2 (Poorest)", "0.4", "0.6", "0.8", "1.0 (Richest)"))+
  labs(y = "Rate of stillbirths per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot(11)


#creating leads and lags
#need to get first year 

#looking at enrollment by district
dists <- df %>% select(c(state, dist_id, access_2010, access_2011, access_2012, access_2013, access_2014, access_2015, access_2016, accessyears,
                         treatment, treated)) 

dists <- unique(dists)

dists$state_labeled <- factor(dists$state, 
                           levels = c(2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 26, 27, 28, 
                                      29, 31),
                           labels = c("Himachal Pradesh", "Punjab", "Chandigarh", "Uttarakhand", "Haryana", "Rajasthan", "Uttar Pradesh", "Bihar", "Sikkim",
                                      "Arunachal Pradesh", "Manipur", "Mizoram", "Tripura", "Meghalaya", "Assam", "West Bengal", "Jharkhand", "Odisha",
                                      "Chhattisgarh", "Madhya Pradesh", "Maharashtra", "Andhra Pradesh", "Karnataka", "Goa", "Kerala"))

library(gtsummary)

dists %>%
  tbl_cross(
    row = state_labeled,
    col = treatment, label = list(treatment ~ "Enrollment", state_labeled ~ "State")
  ) %>% bold_labels() %>% as_gt() %>%
  gt::gtsave( # save table as image
    filename = "my_table_image.png")


#dropping districts not represented in all study years.
#using dist_years_sampled file from above to filter out.

#districts with < 16 years of data

dist_not_full_years <- dist_years_sampled %>% filter(years_sampled < 16)
dist_not_full_years$districs

list_dist_not_full <- dist_not_full_years$districs

df_full <- df %>% filter(!dist_id %in% c(list_dist_not_full))

nrow(df) - nrow(df_full)

#removed 77831 observations

#checking to see if we lost states

table(df$state)

table(df_full$state)

#lost obs from states 10, 18, 20, 21, 22, 28

#write.csv(df_full, "df_dist_all_years_w_treatment.csv")

df <- read.csv("df_dist_all_years_w_treatment.csv")

#df <- df_full

# Tables and Figures ------------------------------------------------------
library(sf)
map_dists <- read_sf("India Shapefile/polbnda_ind.shp")


#descriptive map of enrollment by district
ggplot() + geom_sf(data = map_dists) + theme_void() #empty map of districts

#reading in district enrollment data for eventual merge
enrollment <- read.csv("LongDistrictEnrollment.csv")

table(map_dists$nam)

map_dists <- map_dists %>% rename(State = nam)
map_dists <- map_dists %>% rename(District = laa)

# count enrollment descriptive using g variable below

#group by district

df_dists <- df %>% group_by(dist_id) %>% summarise(treated = mean(g))

ggplot(data = df_dists, mapping = aes(x = as.factor(treated))) + geom_bar() +
  ylim(0, 350)+
  xlab("Districts Enrolled")+
  ylab("Enrolled by Year")+
  theme_cowplot()


# DiD ---------------------------------------------------------------------

#first making caseid numeric
library(stringr)
#removing spaces
#df$caseid_num <- str_replace_all(df$caseid, " ", "") 

#for 3 AHS without stratum code removing NA in center of ID
#df$caseid_num <- gsub("[^0-9.-]", "", df$caseid_num)

#making numeric
#df$caseid_num <- as.numeric(df$caseid_num)

## above does not work. Using X variable as id
df <- df %>% rename(id = X)
str(df$state)
df$state <- as.factor(df$state)
str(df$rural_urban) 
df$rural_urban <- as.factor(df$rural_urban)

df <- df %>% mutate(g = case_when(treat == 0 ~ 0,
                                      treat == 1 ~ 2010,
                                      treat == 2 ~ 2012, 
                                      treat == 3 ~ 2014,
                                      TRUE ~ NA_real_))

#xformula <- ~ age + rural_urban + wi_perc_rank:outcome_year


#df$treat_wealth <- df$g * df$wi_perc_rank
did_check_att_sb <- att_gt(yname = "sb",
                        tname = "outcome_year",
                        gname = "g",
                        idname = "id",
                        #xformla = EAG + (1 + mom_age),
                        data = df,
                        panel = FALSE,
                        #allow_unbalanced_panel = FALSE,
                        weightsname = "weight_adj",
                        clustervars = c("id","dist_id"),
                        control_group = "notyettreated",
                        bstrap=FALSE, cband=FALSE
)

did_check_att_abort <- att_gt(yname = "abort",
                           tname = "outcome_year",
                           gname = "g",
                           idname = "id",
                           #xformla = xformula,
                           data = df,
                           panel = FALSE,
                           #allow_unbalanced_panel = FALSE,
                           weightsname = "weight_adj",
                           clustervars = c("dist_id")
)

did_check_att_ms <- att_gt(yname = "miscarriage",
                           tname = "outcome_year",
                           gname = "g",
                           idname = "id",
                           #xformla = xformula,
                           data = df,
                           panel = FALSE,
                           #allow_unbalanced_panel = FALSE,
                           weightsname = "weight_adj",
                           clustervars = c("dist_id"),
                           #bstrap=FALSE, cband=FALSE
)


summary(did_check_att)
ggdid(did_check_att_sb)
ggdid(did_check_att_abort)
ggdid(did_check_att_ms)

#make into one plot
did.es <- aggte(did_check_att_sb, type="dynamic")
ggdid(did.es)


# Stratified DiD ----------------------------------------------------------


#subsetting into quintiles 

quint1 <- df %>% filter(wi_quintile == 1)
quint2 <- df %>% filter(wi_quintile == 2)
quint3 <- df %>% filter(wi_quintile == 3)
quint4 <- df %>% filter(wi_quintile == 4)
quint5 <- df %>% filter(wi_quintile == 5)

quint1_sb_att <- att_gt(yname = "sb",
                         tname = "outcome_year",
                         gname = "g",
                         idname = "id",
                         #xformla = ~age,
                         data = quint1,
                         panel = FALSE,
                         allow_unbalanced_panel = FALSE,
                         weightsname = "weight_adj",
                         control_group = "notyettreated",
                         clustervars = c("dist_id"),
                         print_details = TRUE,
                         #est_method = "reg",
                         bstrap=TRUE, cband=FALSE
)



quint1_att_abort <- att_gt(yname = "abort",
                           tname = "outcome_year",
                           gname = "g",
                           idname = "id",
                           #xformla = ~age,
                           data = quint1,
                           panel = FALSE,
                           allow_unbalanced_panel = FALSE,
                           weightsname = "weight_adj",
                           control_group = "notyettreated",
                           print_details = TRUE,
                           #est_method = "reg",
                           bstrap=TRUE, cband=FALSE
)

quint1_att_miscarriage <- att_gt(yname = "miscarriage",
                                 tname = "outcome_year",
                                 gname = "g",
                                 idname = "id",
                                 #xformla = ~age,
                                 data = quint1,
                                 panel = FALSE,
                                 allow_unbalanced_panel = FALSE,
                                 weightsname = "weight_adj",
                                 control_group = "notyettreated",
                                 print_details = TRUE,
                                 #est_method = "reg",
                                 bstrap=TRUE, cband=FALSE
)

quint2_sb_att <- att_gt(yname = "sb",
                        tname = "outcome_year",
                        gname = "g",
                        idname = "id",
                        #xformla = ~age,
                        data = quint2,
                        panel = FALSE,
                        allow_unbalanced_panel = FALSE,
                        weightsname = "weight_adj",
                        control_group = "notyettreated",
                        clustervars = c("dist_id"),
                        print_details = TRUE,
                        #est_method = "reg",
                        bstrap=TRUE, cband=FALSE
)



quint2_att_abort <- att_gt(yname = "abort",
                           tname = "outcome_year",
                           gname = "g",
                           idname = "id",
                           #xformla = ~age,
                           data = quint2,
                           panel = FALSE,
                           allow_unbalanced_panel = FALSE,
                           weightsname = "weight_adj",
                           control_group = "notyettreated",
                           print_details = TRUE,
                           #est_method = "reg",
                           bstrap=TRUE, cband=FALSE
)

quint2_att_miscarriage <- att_gt(yname = "miscarriage",
                                 tname = "outcome_year",
                                 gname = "g",
                                 idname = "id",
                                 #xformla = ~age,
                                 data = quint2,
                                 panel = FALSE,
                                 allow_unbalanced_panel = FALSE,
                                 weightsname = "weight_adj",
                                 control_group = "notyettreated",
                                 print_details = TRUE,
                                 #est_method = "reg",
                                 bstrap=TRUE, cband=FALSE
)

quint3_sb_att <- att_gt(yname = "sb",
                        tname = "outcome_year",
                        gname = "g",
                        idname = "id",
                        #xformla = ~age,
                        data = quint3,
                        panel = FALSE,
                        allow_unbalanced_panel = FALSE,
                        weightsname = "weight_adj",
                        control_group = "notyettreated",
                        clustervars = c("dist_id"),
                        print_details = TRUE,
                        #est_method = "reg",
                        bstrap=TRUE, cband=FALSE
)



quint3_att_abort <- att_gt(yname = "abort",
                           tname = "outcome_year",
                           gname = "g",
                           idname = "id",
                           #xformla = ~age,
                           data = quint3,
                           panel = FALSE,
                           allow_unbalanced_panel = FALSE,
                           weightsname = "weight_adj",
                           control_group = "notyettreated",
                           print_details = TRUE,
                           #est_method = "reg",
                           bstrap=TRUE, cband=FALSE
)

quint3_att_miscarriage <- att_gt(yname = "miscarriage",
                                 tname = "outcome_year",
                                 gname = "g",
                                 idname = "id",
                                 #xformla = ~age,
                                 data = quint3,
                                 panel = FALSE,
                                 allow_unbalanced_panel = FALSE,
                                 weightsname = "weight_adj",
                                 control_group = "notyettreated",
                                 print_details = TRUE,
                                 #est_method = "reg",
                                 bstrap=TRUE, cband=FALSE
)

quint4_sb_att <- att_gt(yname = "sb",
                        tname = "outcome_year",
                        gname = "g",
                        idname = "id",
                        #xformla = ~age,
                        data = quint4,
                        panel = FALSE,
                        allow_unbalanced_panel = FALSE,
                        weightsname = "weight_adj",
                        control_group = "notyettreated",
                        clustervars = c("dist_id"),
                        print_details = TRUE,
                        #est_method = "reg",
                        bstrap=TRUE, cband=FALSE
)



quint4_att_abort <- att_gt(yname = "abort",
                           tname = "outcome_year",
                           gname = "g",
                           idname = "id",
                           #xformla = ~age,
                           data = quint4,
                           panel = FALSE,
                           allow_unbalanced_panel = FALSE,
                           weightsname = "weight_adj",
                           control_group = "notyettreated",
                           print_details = TRUE,
                           #est_method = "reg",
                           bstrap=TRUE, cband=FALSE
)

quint4_att_miscarriage <- att_gt(yname = "miscarriage",
                                 tname = "outcome_year",
                                 gname = "g",
                                 idname = "id",
                                 #xformla = ~age,
                                 data = quint4,
                                 panel = FALSE,
                                 allow_unbalanced_panel = FALSE,
                                 weightsname = "weight_adj",
                                 control_group = "notyettreated",
                                 print_details = TRUE,
                                 #est_method = "reg",
                                 bstrap=TRUE, cband=FALSE
)

quint5_sb_att <- att_gt(yname = "sb",
                        tname = "outcome_year",
                        gname = "g",
                        idname = "id",
                        #xformla = ~age,
                        data = quint5,
                        panel = FALSE,
                        allow_unbalanced_panel = FALSE,
                        weightsname = "weight_adj",
                        control_group = "notyettreated",
                        clustervars = c("dist_id"),
                        print_details = TRUE,
                        #est_method = "reg",
                        bstrap=TRUE, cband=FALSE
)



quint5_att_abort <- att_gt(yname = "abort",
                           tname = "outcome_year",
                           gname = "g",
                           idname = "id",
                           #xformla = ~age,
                           data = quint5,
                           panel = FALSE,
                           allow_unbalanced_panel = FALSE,
                           weightsname = "weight_adj",
                           control_group = "notyettreated",
                           print_details = TRUE,
                           #est_method = "reg",
                           bstrap=TRUE, cband=FALSE
)

quint5_att_miscarriage <- att_gt(yname = "miscarriage",
                                 tname = "outcome_year",
                                 gname = "g",
                                 idname = "id",
                                 #xformla = ~age,
                                 data = quint5,
                                 panel = FALSE,
                                 allow_unbalanced_panel = FALSE,
                                 weightsname = "weight_adj",
                                 control_group = "notyettreated",
                                 print_details = TRUE,
                                 #est_method = "reg",
                                 bstrap=TRUE, cband=FALSE
)

#multiplying estimates by 1000 and making event and calendar time models and plots
quint1_sb_att$attper1000 <- quint1_sb_att$att * 1000
quint1_sb_att.group <- aggte(quint1_sb_att, type = "group", na.rm = TRUE)

quint1_sb_att.est <- round(quint1_sb_att.group[["overall.att"]]*1000)
quint1_sb_att.cilow <- round((quint1_sb_att.group[["overall.att"]]-1.96*quint1_sb_att.group[["overall.se"]])*1000)
quint1_sb_att.ciup <- round((quint1_sb_att.group[["overall.att"]]+1.96*quint1_sb_att.group[["overall.se"]])*1000)

quint1_sb_att.group.plot <- ggdid(quint1_sb_att.group)

quint1_sb_att.dynamic <- aggte(quint1_sb_att, type = "dynamic")
quint1_sb_att.dynamic.plot <- ggdid(quint1_sb_att.dynamic)

quint1_sb_att.calendar <- aggte(quint1_sb_att, type = "calendar")
quint1_sb_att.calendar.plot <- ggdid(quint1_sb_att)

quint1_att_abort$attper1000 <- quint1_att_abort$att * 1000
quint1_att_abort.group <- aggte(quint1_att_abort, type = "group", na.rm = TRUE)

quint1_att_abort.est <- round(quint1_att_abort.group[["overall.att"]]*1000)
quint1_att_abort.cilow <- round((quint1_att_abort.group[["overall.att"]]-1.96*quint1_att_abort.group[["overall.se"]])*1000)
quint1_att_abort.ciup <- round((quint1_att_abort.group[["overall.att"]]+1.96*quint1_att_abort.group[["overall.se"]])*1000)

quint1_att_abort.group.plot <- ggdid(quint1_att_abort.group)

quint1_att_abort.dynamic <- aggte(quint1_att_abort, type = "dynamic")
quint1_att_abort.dynamic.plot <- ggdid(quint1_att_abort.dynamic)

quint1_att_abort.calendar <- aggte(quint1_att_abort, type = "calendar")
quint1_att_abort.calendar.plot <- ggdid(quint1_att_abort)

quint1_att_miscarriage$attper1000 <- quint1_att_miscarriage$att * 1000
quint1_att_miscarriage.group <- aggte(quint1_att_miscarriage, type = "group", na.rm = TRUE)

quint1_att_miscarriage.est <- round(quint1_att_miscarriage.group[["overall.att"]]*1000)
quint1_att_miscarriage.cilow <- round((quint1_att_miscarriage.group[["overall.att"]]-1.96*quint1_att_miscarriage.group[["overall.se"]])*1000)
quint1_att_miscarriage.ciup <- round((quint1_att_miscarriage.group[["overall.att"]]+1.96*quint1_att_miscarriage.group[["overall.se"]])*1000)

quint1_att_miscarriage.group.plot <- ggdid(quint1_att_miscarriage.group)

quint1_att_miscarriage.dynamic <- aggte(quint1_att_miscarriage, type = "dynamic")
quint1_att_miscarriage.dynamic.plot <- ggdid(quint1_att_miscarriage.dynamic)

quint1_att_miscarriage.calendar <- aggte(quint1_att_miscarriage, type = "calendar")
quint1_att_miscarriage.calendar.plot <- ggdid(quint1_att_miscarriage)

### Quint 2
quint2_sb_att$attper1000 <- quint2_sb_att$att * 1000
quint2_sb_att.group <- aggte(quint2_sb_att, type = "group", na.rm = TRUE)

quint2_sb_att.est <- round(quint2_sb_att.group[["overall.att"]]*1000)
quint2_sb_att.cilow <- round((quint2_sb_att.group[["overall.att"]]-1.96*quint2_sb_att.group[["overall.se"]])*1000)
quint2_sb_att.ciup <- round((quint2_sb_att.group[["overall.att"]]+1.96*quint2_sb_att.group[["overall.se"]])*1000)

quint2_sb_att.group.plot <- ggdid(quint2_sb_att.group)

quint2_sb_att.dynamic <- aggte(quint2_sb_att, type = "dynamic")
quint2_sb_att.dynamic.plot <- ggdid(quint2_sb_att.dynamic)

quint2_sb_att.calendar <- aggte(quint2_sb_att, type = "calendar")
quint2_sb_att.calendar.plot <- ggdid(quint2_sb_att)

quint2_att_abort$attper1000 <- quint2_att_abort$att * 1000
quint2_att_abort.group <- aggte(quint2_att_abort, type = "group", na.rm = TRUE)

quint2_att_abort.est <- round(quint2_att_abort.group[["overall.att"]]*1000)
quint2_att_abort.cilow <- round((quint2_att_abort.group[["overall.att"]]-1.96*quint2_att_abort.group[["overall.se"]])*1000)
quint2_att_abort.ciup <- round((quint2_att_abort.group[["overall.att"]]+1.96*quint2_att_abort.group[["overall.se"]])*1000)

quint2_att_abort.group.plot <- ggdid(quint2_att_abort.group)

quint2_att_abort.dynamic <- aggte(quint2_att_abort, type = "dynamic")
quint2_att_abort.dynamic.plot <- ggdid(quint2_att_abort.dynamic)

quint2_att_abort.calendar <- aggte(quint2_att_abort, type = "calendar")
quint2_att_abort.calendar.plot <- ggdid(quint2_att_abort)

quint2_att_miscarriage$attper1000 <- quint2_att_miscarriage$att * 1000
quint2_att_miscarriage.group <- aggte(quint2_att_miscarriage, type = "group", na.rm = TRUE)

quint2_att_miscarriage.est <- round(quint2_att_miscarriage.group[["overall.att"]]*1000)
quint2_att_miscarriage.cilow <- round((quint2_att_miscarriage.group[["overall.att"]]-1.96*quint2_att_miscarriage.group[["overall.se"]])*1000)
quint2_att_miscarriage.ciup <- round((quint2_att_miscarriage.group[["overall.att"]]+1.96*quint2_att_miscarriage.group[["overall.se"]])*1000)

quint2_att_miscarriage.group.plot <- ggdid(quint2_att_miscarriage.group)

quint2_att_miscarriage.dynamic <- aggte(quint2_att_miscarriage, type = "dynamic")
quint2_att_miscarriage.dynamic.plot <- ggdid(quint2_att_miscarriage.dynamic)

quint2_att_miscarriage.calendar <- aggte(quint2_att_miscarriage, type = "calendar")
quint2_att_miscarriage.calendar.plot <- ggdid(quint2_att_miscarriage)

## Quint 3 
quint3_sb_att$attper1000 <- quint3_sb_att$att * 1000
quint3_sb_att.group <- aggte(quint3_sb_att, type = "group", na.rm = TRUE)

quint3_sb_att.est <- round(quint3_sb_att.group[["overall.att"]]*1000)
quint3_sb_att.cilow <- round((quint3_sb_att.group[["overall.att"]]-1.96*quint3_sb_att.group[["overall.se"]])*1000)
quint3_sb_att.ciup <- round((quint3_sb_att.group[["overall.att"]]+1.96*quint3_sb_att.group[["overall.se"]])*1000)

quint3_sb_att.group.plot <- ggdid(quint3_sb_att.group)

quint3_sb_att.dynamic <- aggte(quint3_sb_att, type = "dynamic")
quint3_sb_att.dynamic.plot <- ggdid(quint3_sb_att.dynamic)

quint3_sb_att.calendar <- aggte(quint3_sb_att, type = "calendar")
quint3_sb_att.calendar.plot <- ggdid(quint3_sb_att)

quint3_att_abort$attper1000 <- quint3_att_abort$att * 1000
quint3_att_abort.group <- aggte(quint3_att_abort, type = "group", na.rm = TRUE)

quint3_att_abort.est <- round(quint3_att_abort.group[["overall.att"]]*1000)
quint3_att_abort.cilow <- round((quint3_att_abort.group[["overall.att"]]-1.96*quint3_att_abort.group[["overall.se"]])*1000)
quint3_att_abort.ciup <- round((quint3_att_abort.group[["overall.att"]]+1.96*quint3_att_abort.group[["overall.se"]])*1000)

quint3_att_abort.group.plot <- ggdid(quint3_att_abort.group)

quint3_att_abort.dynamic <- aggte(quint3_att_abort, type = "dynamic")
quint3_att_abort.dynamic.plot <- ggdid(quint3_att_abort.dynamic)

quint3_att_abort.calendar <- aggte(quint3_att_abort, type = "calendar")
quint3_att_abort.calendar.plot <- ggdid(quint3_att_abort)

quint3_att_miscarriage$attper1000 <- quint3_att_miscarriage$att * 1000
quint3_att_miscarriage.group <- aggte(quint3_att_miscarriage, type = "group", na.rm = TRUE)

quint3_att_miscarriage.est <- round(quint3_att_miscarriage.group[["overall.att"]]*1000)
quint3_att_miscarriage.cilow <- round((quint3_att_miscarriage.group[["overall.att"]]-1.96*quint3_att_miscarriage.group[["overall.se"]])*1000)
quint3_att_miscarriage.ciup <- round((quint3_att_miscarriage.group[["overall.att"]]+1.96*quint3_att_miscarriage.group[["overall.se"]])*1000)

quint3_att_miscarriage.group.plot <- ggdid(quint3_att_miscarriage.group)

quint3_att_miscarriage.dynamic <- aggte(quint3_att_miscarriage, type = "dynamic")
quint3_att_miscarriage.dynamic.plot <- ggdid(quint3_att_miscarriage.dynamic)

quint3_att_miscarriage.calendar <- aggte(quint3_att_miscarriage, type = "calendar")
quint3_att_miscarriage.calendar.plot <- ggdid(quint3_att_miscarriage)


# Quint 4
quint4_sb_att$attper1000 <- quint4_sb_att$att * 1000
quint4_sb_att.group <- aggte(quint4_sb_att, type = "group", na.rm = TRUE)

quint4_sb_att.est <- round(quint4_sb_att.group[["overall.att"]]*1000)
quint4_sb_att.cilow <- round((quint4_sb_att.group[["overall.att"]]-1.96*quint4_sb_att.group[["overall.se"]])*1000)
quint4_sb_att.ciup <- round((quint4_sb_att.group[["overall.att"]]+1.96*quint4_sb_att.group[["overall.se"]])*1000)

quint4_sb_att.group.plot <- ggdid(quint4_sb_att.group)

quint4_sb_att.dynamic <- aggte(quint4_sb_att, type = "dynamic")
quint4_sb_att.dynamic.plot <- ggdid(quint4_sb_att.dynamic)

quint4_sb_att.calendar <- aggte(quint4_sb_att, type = "calendar")
quint4_sb_att.calendar.plot <- ggdid(quint4_sb_att)

quint4_att_abort$attper1000 <- quint4_att_abort$att * 1000
quint4_att_abort.group <- aggte(quint4_att_abort, type = "group", na.rm = TRUE)

quint4_att_abort.est <- round(quint4_att_abort.group[["overall.att"]]*1000)
quint4_att_abort.cilow <- round((quint4_att_abort.group[["overall.att"]]-1.96*quint4_att_abort.group[["overall.se"]])*1000)
quint4_att_abort.ciup <- round((quint4_att_abort.group[["overall.att"]]+1.96*quint4_att_abort.group[["overall.se"]])*1000)

quint4_att_abort.group.plot <- ggdid(quint4_att_abort.group)

quint4_att_abort.dynamic <- aggte(quint4_att_abort, type = "dynamic")
quint4_att_abort.dynamic.plot <- ggdid(quint4_att_abort.dynamic)

quint4_att_abort.calendar <- aggte(quint4_att_abort, type = "calendar")
quint4_att_abort.calendar.plot <- ggdid(quint4_att_abort)

quint4_att_miscarriage$attper1000 <- quint4_att_miscarriage$att * 1000
quint4_att_miscarriage.group <- aggte(quint4_att_miscarriage, type = "group", na.rm = TRUE)

quint4_att_miscarriage.est <- round(quint4_att_miscarriage.group[["overall.att"]]*1000)
quint4_att_miscarriage.cilow <- round((quint4_att_miscarriage.group[["overall.att"]]-1.96*quint4_att_miscarriage.group[["overall.se"]])*1000)
quint4_att_miscarriage.ciup <- round((quint4_att_miscarriage.group[["overall.att"]]+1.96*quint4_att_miscarriage.group[["overall.se"]])*1000)

quint4_att_miscarriage.group.plot <- ggdid(quint4_att_miscarriage.group)

quint4_att_miscarriage.dynamic <- aggte(quint4_att_miscarriage, type = "dynamic")
quint4_att_miscarriage.dynamic.plot <- ggdid(quint4_att_miscarriage.dynamic)

quint4_att_miscarriage.calendar <- aggte(quint4_att_miscarriage, type = "calendar")
quint4_att_miscarriage.calendar.plot <- ggdid(quint4_att_miscarriage)

# Quint 5

quint5_sb_att$attper1000 <- quint5_sb_att$att * 1000
quint5_sb_att.group <- aggte(quint5_sb_att, type = "group", na.rm = TRUE)

quint5_sb_att.est <- round(quint5_sb_att.group[["overall.att"]]*1000)
quint5_sb_att.cilow <- round((quint5_sb_att.group[["overall.att"]]-1.96*quint5_sb_att.group[["overall.se"]])*1000)
quint5_sb_att.ciup <- round((quint5_sb_att.group[["overall.att"]]+1.96*quint5_sb_att.group[["overall.se"]])*1000)

quint5_sb_att.group.plot <- ggdid(quint5_sb_att.group)

quint5_sb_att.dynamic <- aggte(quint5_sb_att, type = "dynamic")
quint5_sb_att.dynamic.plot <- ggdid(quint5_sb_att.dynamic)

quint5_sb_att.calendar <- aggte(quint5_sb_att, type = "calendar")
quint5_sb_att.calendar.plot <- ggdid(quint5_sb_att)

quint5_att_abort$attper1000 <- quint5_att_abort$att * 1000
quint5_att_abort.group <- aggte(quint5_att_abort, type = "group", na.rm = TRUE)

quint5_att_abort.est <- round(quint5_att_abort.group[["overall.att"]]*1000)
quint5_att_abort.cilow <- round((quint5_att_abort.group[["overall.att"]]-1.96*quint5_att_abort.group[["overall.se"]])*1000)
quint5_att_abort.ciup <- round((quint5_att_abort.group[["overall.att"]]+1.96*quint5_att_abort.group[["overall.se"]])*1000)

quint5_att_abort.group.plot <- ggdid(quint5_att_abort.group)

quint5_att_abort.dynamic <- aggte(quint5_att_abort, type = "dynamic")
quint5_att_abort.dynamic.plot <- ggdid(quint5_att_abort.dynamic)

quint5_att_abort.calendar <- aggte(quint5_att_abort, type = "calendar")
quint5_att_abort.calendar.plot <- ggdid(quint5_att_abort)

quint5_att_miscarriage$attper1000 <- quint5_att_miscarriage$att * 1000
quint5_att_miscarriage.group <- aggte(quint5_att_miscarriage, type = "group", na.rm = TRUE)

quint5_att_miscarriage.est <- round(quint5_att_miscarriage.group[["overall.att"]]*1000)
quint5_att_miscarriage.cilow <- round((quint5_att_miscarriage.group[["overall.att"]]-1.96*quint5_att_miscarriage.group[["overall.se"]])*1000)
quint5_att_miscarriage.ciup <- round((quint5_att_miscarriage.group[["overall.att"]]+1.96*quint5_att_miscarriage.group[["overall.se"]])*1000)

quint5_att_miscarriage.group.plot <- ggdid(quint5_att_miscarriage.group)

quint5_att_miscarriage.dynamic <- aggte(quint5_att_miscarriage, type = "dynamic")
quint5_att_miscarriage.dynamic.plot <- ggdid(quint5_att_miscarriage.dynamic)

quint5_att_miscarriage.calendar <- aggte(quint5_att_miscarriage, type = "calendar")
quint5_att_miscarriage.calendar.plot <- ggdid(quint5_att_miscarriage)


# Rural / urban stratification
table(df$rural_urban)

rural <- df %>% filter(rural_urban == 0)
urban <- df %>% filter(rural_urban == 1)

rural_sb_att <- att_gt(yname = "sb",
                        tname = "outcome_year",
                        gname = "g",
                        idname = "id",
                        #xformla = ~age,
                        data = rural,
                        panel = FALSE,
                        allow_unbalanced_panel = FALSE,
                        weightsname = "weight_adj",
                        control_group = "notyettreated",
                        clustervars = c("dist_id"),
                        print_details = TRUE,
                        #est_method = "reg",
                        bstrap=TRUE, cband=FALSE
)



rural_att_abort <- att_gt(yname = "abort",
                           tname = "outcome_year",
                           gname = "g",
                           idname = "id",
                           #xformla = ~age,
                           data = rural,
                           panel = FALSE,
                           allow_unbalanced_panel = FALSE,
                           weightsname = "weight_adj",
                           control_group = "notyettreated",
                           print_details = TRUE,
                           #est_method = "reg",
                           bstrap=TRUE, cband=FALSE
)

rural_att_miscarriage <- att_gt(yname = "miscarriage",
                                 tname = "outcome_year",
                                 gname = "g",
                                 idname = "id",
                                 #xformla = ~age,
                                 data = rural,
                                 panel = FALSE,
                                 allow_unbalanced_panel = FALSE,
                                 weightsname = "weight_adj",
                                 control_group = "notyettreated",
                                 print_details = TRUE,
                                 #est_method = "reg",
                                 bstrap=TRUE, cband=FALSE
)

urban_sb_att <- att_gt(yname = "sb",
                        tname = "outcome_year",
                        gname = "g",
                        idname = "id",
                        #xformla = ~age,
                        data = urban,
                        panel = FALSE,
                        allow_unbalanced_panel = FALSE,
                        weightsname = "weight_adj",
                        control_group = "notyettreated",
                        clustervars = c("dist_id"),
                        print_details = TRUE,
                        #est_method = "reg",
                        bstrap=TRUE, cband=FALSE
)



urban_att_abort <- att_gt(yname = "abort",
                           tname = "outcome_year",
                           gname = "g",
                           idname = "id",
                           #xformla = ~age,
                           data = urban,
                           panel = FALSE,
                           allow_unbalanced_panel = FALSE,
                           weightsname = "weight_adj",
                           control_group = "notyettreated",
                           print_details = TRUE,
                           #est_method = "reg",
                           bstrap=TRUE, cband=FALSE
)

urban_att_miscarriage <- att_gt(yname = "miscarriage",
                                 tname = "outcome_year",
                                 gname = "g",
                                 idname = "id",
                                 #xformla = ~age,
                                 data = urban,
                                 panel = FALSE,
                                 allow_unbalanced_panel = FALSE,
                                 weightsname = "weight_adj",
                                 control_group = "notyettreated",
                                 print_details = TRUE,
                                 #est_method = "reg",
                                 bstrap=TRUE, cband=FALSE
)


aggte(rural_sb_att, type = "dynamic")
ggdid(rural_sb_att)

 #### RURAL ABORTION DYNAMIC EFFECTS AND PLOTS

rural_atta_abort_dynamic <- aggte(rural_att_abort, type = "dynamic")

rural_abot_dynamic_plot <- ggdid(rural_atta_abort_dynamic)

rural_atta_abort_calendar <- aggte(rural_att_abort, type = "calendar")

rural_abot_calendar_plot <- ggdid(rural_atta_abort_calendar)

rural_abort_group <- aggte(rural_att_abort, type = "group")
ggdid(rural_abort_group)

rural_atta_abort_dynamic.est <- round(rural_atta_abort_dynamic[["overall.att"]]*1000)
rural_atta_abort_dynamic.cilow <- round((rural_atta_abort_dynamic[["overall.att"]]-1.96*rural_atta_abort_dynamic[["overall.se"]])*1000)
rural_atta_abort_dynamic.ciup <- round((rural_atta_abort_dynamic[["overall.att"]]+1.96*rural_atta_abort_dynamic[["overall.se"]])*1000)

rural_atta_abort_dynamic.outcome <- c("Abortion")

rural_atta_abort_dynamic.dyn <- data.frame(time=rural_atta_abort_dynamic[["egt"]],
                                   att=rural_atta_abort_dynamic[["att.egt"]],
                                   se=rural_atta_abort_dynamic[["se.egt"]])

rural_atta_abort_dynamic.dyn  <- rural_atta_abort_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))



ggplot(rural_atta_abort_dynamic.dyn)+
  geom_vline(xintercept=-0,lty=2,color="grey59")+
  geom_hline(yintercept=0,lty=2,color="grey59")+
  
  geom_rect( aes(xmin=0,
                 xmax=9,
                 ymin=(rural_atta_abort_dynamic[["overall.att"]]-1.96*rural_atta_abort_dynamic[["overall.se"]])*1000,
                 ymax=(rural_atta_abort_dynamic[["overall.att"]]+1.96*rural_atta_abort_dynamic[["overall.se"]])*1000, alpha="95% CI"), fill="lightblue",
             color=NA)+
  geom_segment( 
    aes(x = 0, y = rural_atta_abort_dynamic[["overall.att"]]*1000, xend = 9, yend = rural_atta_abort_dynamic[["overall.att"]]*1000, lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att*1000,2),ymin = round((att-1.96*se)*1000,2), ymax = round((att+1.96*se)*1000,2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of abortions per 1,000 pregnancies -- RURAL")+
  xlab("Time since Intervention implemented")+
  #scale_x_continuous(breaks=c(-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6),labels = c("-6","-5","-4","-3","-2","-1","+1","+2","+3","+4","+5","+6","+7"))+
  
  scale_color_manual(values=c("red","blue2"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  #facet_wrap(~set)+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  
  theme_bw()+
  #main_theme+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  ggtitle(str_wrap("Event study analysis of dynamic effects after the intervention",
                   width=80))



######## RURAL MISCARRIAGE EVENT STUDY PLOTS 

rural_att_miscarriage_dynamic <- aggte(rural_att_miscarriage, type = "dynamic")

rural_miscarriage_dynamic_plot <- ggdid(rural_att_miscarriage_dynamic)

rural_att_miscarriage_calendar <- aggte(rural_att_miscarriage, type = "calendar")

rural_miscarriage_calendar_plot <- ggdid(rural_att_miscarriage_calendar)

rural_att_miscarriage_dynamic.est <- round(rural_att_miscarriage_dynamic[["overall.att"]]*1000)
rural_att_miscarriage_dynamic.cilow <- round((rural_att_miscarriage_dynamic[["overall.att"]]-1.96*rural_att_miscarriage_dynamic[["overall.se"]])*1000)
rural_att_miscarriage_dynamic.ciup <- round((rural_att_miscarriage_dynamic[["overall.att"]]+1.96*rural_att_miscarriage_dynamic[["overall.se"]])*1000)

rural_att_miscarriage_dynamic.outcome <- c("Miscarriage")

rural_att_miscarriage_dynamic.dyn <- data.frame(time=rural_att_miscarriage_dynamic[["egt"]],
                                           att=rural_att_miscarriage_dynamic[["att.egt"]],
                                           se=rural_att_miscarriage_dynamic[["se.egt"]])

rural_att_miscarriage_dynamic.dyn  <- rural_att_miscarriage_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))



ggplot(rural_att_miscarriage_dynamic.dyn)+
  geom_vline(xintercept=-0,lty=2,color="grey59")+
  geom_hline(yintercept=0,lty=2,color="grey59")+
  
  geom_rect( aes(xmin=0,
                 xmax=9,
                 ymin=(rural_att_miscarriage_dynamic[["overall.att"]]-1.96*rural_att_miscarriage_dynamic[["overall.se"]])*1000,
                 ymax=(rural_att_miscarriage_dynamic[["overall.att"]]+1.96*rural_att_miscarriage_dynamic[["overall.se"]])*1000, alpha="95% CI"), fill="lightblue",
             color=NA)+
  geom_segment( 
    aes(x = 0, y = rural_att_miscarriage_dynamic[["overall.att"]]*1000, xend = 9, yend = rural_att_miscarriage_dynamic[["overall.att"]]*1000, lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att*1000,2),ymin = round((att-1.96*se)*1000,2), ymax = round((att+1.96*se)*1000,2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of miscarriages per 1,000 pregnancies -- RURAL")+
  xlab("Time since Intervention implemented")+
  #scale_x_continuous(breaks=c(-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6),labels = c("-6","-5","-4","-3","-2","-1","+1","+2","+3","+4","+5","+6","+7"))+
  
  scale_color_manual(values=c("red","blue2"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  #facet_wrap(~set)+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  
  theme_bw()+
  #main_theme+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  ggtitle(str_wrap("Event study analysis of dynamic effects after the intervention",
                   width=80))


###### combining rural group estimates into one plot #####
rural_abort_group <- aggte(rural_att_abort, type = "group")
ggdid(rural_abort_group)

rural_abort_group_tidy <- tidy(rural_abort_group)
rural_abort_group_tidy$outcome <- c("Abortion")
rural_abort_group_tidy$est_per1000 <- rural_abort_group_tidy$estimate*1000
rural_abort_group_tidy$stderr_per1000 <- rural_abort_group_tidy$std.error*1000
rural_abort_group_tidy$conflow_per1000 <- rural_abort_group_tidy$conf.low*1000
rural_abort_group_tidy$confhigh_per1000 <- rural_abort_group_tidy$conf.high*1000



rural_sb_group <- aggte(rural_sb_att, type = "group")

rural_sb_group_tidy <- tidy(rural_sb_group)
rural_sb_group_tidy$outcome <- c("Stillbirth")
rural_sb_group_tidy$est_per1000 <- rural_sb_group_tidy$estimate*1000
rural_sb_group_tidy$stderr_per1000 <- rural_sb_group_tidy$std.error*1000
rural_sb_group_tidy$conflow_per1000 <- rural_sb_group_tidy$conf.low*1000
rural_sb_group_tidy$confhigh_per1000 <- rural_sb_group_tidy$conf.high*1000



rural_miscarriage_group <- aggte(rural_att_miscarriage, type = "group")

rural_miscarriage_group_tidy <- tidy(rural_miscarriage_group)
rural_miscarriage_group_tidy$outcome <- c("Miscarriage")
rural_miscarriage_group_tidy$est_per1000 <- rural_miscarriage_group_tidy$estimate*1000
rural_miscarriage_group_tidy$stderr_per1000 <- rural_miscarriage_group_tidy$std.error*1000
rural_miscarriage_group_tidy$conflow_per1000 <- rural_miscarriage_group_tidy$conf.low*1000
rural_miscarriage_group_tidy$confhigh_per1000 <- rural_miscarriage_group_tidy$conf.high*1000



mycolors_outcomes <- c("#024b7a", "#e67e00", "#44b7c2")
rural_group_atts <- rbind(rural_sb_group_tidy, rural_abort_group_tidy, rural_miscarriage_group_tidy)

library(cowplot)
rural_atts_group_plot <- ggplot(data = rural_group_atts, mapping = aes(x = group, y = (estimate*100), color = outcome)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin = (conf.low*100), ymax = (conf.high*100)), width = 0.3, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, color = I("black"), linetype = 2)+
  ylim(-30, 30)+
  #scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8), labels = c("2004-2005", "2006-2007", "2008-2009", "2010-2011",
  #                                                         "2012-2013", "2014-2015", "2016-2017", "2018-2019"))+
  scale_color_manual(values = mycolors_outcomes, name = "Outcome", breaks = c("Stillbirth", "Abortion", "Miscarriage"))+
  ylab("ATT Overall and by Group for Rural Residents")+
  xlab("Treatment Year") +
  theme_cowplot()
  


rural_att_miscarriage_dynamic.dyn <- data.frame(time=rural_att_miscarriage_dynamic[["egt"]],
                                                att=rural_att_miscarriage_dynamic[["att.egt"]],
                                                se=rural_att_miscarriage_dynamic[["se.egt"]])

rural_att_miscarriage_dynamic.dyn  <- rural_att_miscarriage_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))



ggplot(rural_att_miscarriage_dynamic.dyn)+
  geom_vline(xintercept=-0,lty=2,color="grey59")+
  geom_hline(yintercept=0,lty=2,color="grey59")+
  
  geom_rect( aes(xmin=0,
                 xmax=9,
                 ymin=(rural_att_miscarriage_dynamic[["overall.att"]]-1.96*rural_att_miscarriage_dynamic[["overall.se"]])*1000,
                 ymax=(rural_att_miscarriage_dynamic[["overall.att"]]+1.96*rural_att_miscarriage_dynamic[["overall.se"]])*1000, alpha="95% CI"), fill="lightblue",
             color=NA)+
  geom_segment( 
    aes(x = 0, y = rural_att_miscarriage_dynamic[["overall.att"]]*1000, xend = 9, yend = rural_att_miscarriage_dynamic[["overall.att"]]*1000, lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att*1000,2),ymin = round((att-1.96*se)*1000,2), ymax = round((att+1.96*se)*1000,2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of miscarriages per 1,000 pregnancies -- RURAL")+
  xlab("Time since Intervention implemented")+
  #scale_x_continuous(breaks=c(-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6),labels = c("-6","-5","-4","-3","-2","-1","+1","+2","+3","+4","+5","+6","+7"))+
  
  scale_color_manual(values=c("red","blue2"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  #facet_wrap(~set)+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  
  theme_bw()+
  #main_theme+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  ggtitle(str_wrap("Event study analysis of dynamic effects after the intervention",
                   width=80))


#### urban ABORTION DYNAMIC EFFECTS AND PLOTS #########

urban_att_abort_dynamic <- aggte(urban_att_abort, type = "dynamic")

urban_abot_dynamic_plot <- ggdid(urban_att_abort_dynamic)

urban_atta_abort_calendar <- aggte(urban_att_abort, type = "calendar")

urban_abot_calendar_plot <- ggdid(urban_atta_abort_calendar)

urban_att_abort_dynamic.est <- round(urban_att_abort_dynamic[["overall.att"]]*1000)
urban_att_abort_dynamic.cilow <- round((urban_att_abort_dynamic[["overall.att"]]-1.96*urban_att_abort_dynamic[["overall.se"]])*1000)
urban_att_abort_dynamic.ciup <- round((urban_att_abort_dynamic[["overall.att"]]+1.96*urban_att_abort_dynamic[["overall.se"]])*1000)

urban_att_abort_dynamic.outcome <- c("Abortion")

urban_att_abort_dynamic.dyn <- data.frame(time=urban_att_abort_dynamic[["egt"]],
                                           att=urban_att_abort_dynamic[["att.egt"]],
                                           se=urban_att_abort_dynamic[["se.egt"]])

urban_att_abort_dynamic.dyn  <- urban_att_abort_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))



ggplot(urban_att_abort_dynamic.dyn)+
  geom_vline(xintercept=-0,lty=2,color="grey59")+
  geom_hline(yintercept=0,lty=2,color="grey59")+
  
  geom_rect( aes(xmin=0,
                 xmax=9,
                 ymin=(urban_att_abort_dynamic[["overall.att"]]-1.96*urban_att_abort_dynamic[["overall.se"]])*1000,
                 ymax=(urban_att_abort_dynamic[["overall.att"]]+1.96*urban_att_abort_dynamic[["overall.se"]])*1000, alpha="95% CI"), fill="lightblue",
             color=NA)+
  geom_segment( 
    aes(x = 0, y = urban_att_abort_dynamic[["overall.att"]]*1000, xend = 9, yend = urban_att_abort_dynamic[["overall.att"]]*1000, lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att*1000,2),ymin = round((att-1.96*se)*1000,2), ymax = round((att+1.96*se)*1000,2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of abortions per 1,000 pregnancies -- urban")+
  xlab("Time since Intervention implemented")+
  #scale_x_continuous(breaks=c(-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6),labels = c("-6","-5","-4","-3","-2","-1","+1","+2","+3","+4","+5","+6","+7"))+
  
  scale_color_manual(values=c("red","blue2"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  #facet_wrap(~set)+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  
  theme_bw()+
  #main_theme+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  ggtitle(str_wrap("Event study analysis of dynamic effects after the intervention",
                   width=80))



######## urban MISCARRIAGE EVENT STUDY PLOTS ###############

urban_att_miscarriage_dynamic <- aggte(urban_att_miscarriage, type = "dynamic")

urban_miscarriage_dynamic_plot <- ggdid(urban_att_miscarriage_dynamic)

urban_att_miscarriage_calendar <- aggte(urban_att_miscarriage, type = "calendar")

urban_miscarriage_calendar_plot <- ggdid(urban_att_miscarriage_calendar)

urban_att_miscarriage_dynamic.est <- round(urban_att_miscarriage_dynamic[["overall.att"]]*1000)
urban_att_miscarriage_dynamic.cilow <- round((urban_att_miscarriage_dynamic[["overall.att"]]-1.96*urban_att_miscarriage_dynamic[["overall.se"]])*1000)
urban_att_miscarriage_dynamic.ciup <- round((urban_att_miscarriage_dynamic[["overall.att"]]+1.96*urban_att_miscarriage_dynamic[["overall.se"]])*1000)

urban_att_miscarriage_dynamic.outcome <- c("Miscarriage")

urban_att_miscarriage_dynamic.dyn <- data.frame(time=urban_att_miscarriage_dynamic[["egt"]],
                                                att=urban_att_miscarriage_dynamic[["att.egt"]],
                                                se=urban_att_miscarriage_dynamic[["se.egt"]])

urban_att_miscarriage_dynamic.dyn  <- urban_att_miscarriage_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))



ggplot(urban_att_miscarriage_dynamic.dyn)+
  geom_vline(xintercept=-0,lty=2,color="grey59")+
  geom_hline(yintercept=0,lty=2,color="grey59")+
  
  geom_rect( aes(xmin=0,
                 xmax=9,
                 ymin=(urban_att_miscarriage_dynamic[["overall.att"]]-1.96*urban_att_miscarriage_dynamic[["overall.se"]])*1000,
                 ymax=(urban_att_miscarriage_dynamic[["overall.att"]]+1.96*urban_att_miscarriage_dynamic[["overall.se"]])*1000, alpha="95% CI"), fill="lightblue",
             color=NA)+
  geom_segment( 
    aes(x = 0, y = urban_att_miscarriage_dynamic[["overall.att"]]*1000, xend = 9, yend = urban_att_miscarriage_dynamic[["overall.att"]]*1000, lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att*1000,2),ymin = round((att-1.96*se)*1000,2), ymax = round((att+1.96*se)*1000,2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of miscarriages per 1,000 pregnancies -- urban")+
  xlab("Time since Intervention implemented")+
  #scale_x_continuous(breaks=c(-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6),labels = c("-6","-5","-4","-3","-2","-1","+1","+2","+3","+4","+5","+6","+7"))+
  
  scale_color_manual(values=c("red","blue2"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  #facet_wrap(~set)+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  
  theme_bw()+
  #main_theme+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  ggtitle(str_wrap("Event study analysis of dynamic effects after the intervention",
                   width=80))



quint4_sb_att.group.plot <- ggdid(quint4_sb_att.group)

quint4_sb_att.dynamic <- aggte(quint4_sb_att, type = "dynamic")
quint4_sb_att.dynamic.plot <- ggdid(quint4_sb_att.dynamic)

quint4_sb_att.calendar <- aggte(quint4_sb_att, type = "calendar")
quint4_sb_att.calendar.plot <- ggdid(quint4_sb_att)


# Education (yes vs. no primary) stratification
table(df$primary_school)

primary <- df %>% filter(primary_school == 1)
nonprimary <- df %>% filter(primary_school == 0)

primary_sb_att <- att_gt(yname = "sb",
                       tname = "outcome_year",
                       gname = "g",
                       idname = "id",
                       #xformla = ~age,
                       data = primary,
                       panel = FALSE,
                       allow_unbalanced_panel = FALSE,
                       weightsname = "weight_adj",
                       control_group = "notyettreated",
                       clustervars = c("dist_id"),
                       print_details = TRUE,
                       #est_method = "reg",
                       bstrap=TRUE, cband=FALSE
)



primary_att_abort <- att_gt(yname = "abort",
                          tname = "outcome_year",
                          gname = "g",
                          idname = "id",
                          #xformla = ~age,
                          data = primary,
                          panel = FALSE,
                          allow_unbalanced_panel = FALSE,
                          weightsname = "weight_adj",
                          control_group = "notyettreated",
                          print_details = TRUE,
                          #est_method = "reg",
                          bstrap=TRUE, cband=FALSE
)

primary_att_miscarriage <- att_gt(yname = "miscarriage",
                                tname = "outcome_year",
                                gname = "g",
                                idname = "id",
                                #xformla = ~age,
                                data = primary,
                                panel = FALSE,
                                allow_unbalanced_panel = FALSE,
                                weightsname = "weight_adj",
                                control_group = "notyettreated",
                                print_details = TRUE,
                                #est_method = "reg",
                                bstrap=TRUE, cband=FALSE
)


nonprimary_sb_att <- att_gt(yname = "sb",
                       tname = "outcome_year",
                       gname = "g",
                       idname = "id",
                       #xformla = ~age,
                       data = nonprimary,
                       panel = FALSE,
                       allow_unbalanced_panel = FALSE,
                       weightsname = "weight_adj",
                       control_group = "notyettreated",
                       clustervars = c("dist_id"),
                       print_details = TRUE,
                       #est_method = "reg",
                       bstrap=TRUE, cband=FALSE
)



nonprimary_att_abort <- att_gt(yname = "abort",
                          tname = "outcome_year",
                          gname = "g",
                          idname = "id",
                          #xformla = ~age,
                          data = nonprimary,
                          panel = FALSE,
                          allow_unbalanced_panel = FALSE,
                          weightsname = "weight_adj",
                          control_group = "notyettreated",
                          print_details = TRUE,
                          #est_method = "reg",
                          bstrap=TRUE, cband=FALSE
)

nonprimary_att_miscarriage <- att_gt(yname = "miscarriage",
                                tname = "outcome_year",
                                gname = "g",
                                idname = "id",
                                #xformla = ~age,
                                data = nonprimary,
                                panel = FALSE,
                                allow_unbalanced_panel = FALSE,
                                weightsname = "weight_adj",
                                control_group = "notyettreated",
                                print_details = TRUE,
                                #est_method = "reg",
                                bstrap=TRUE, cband=FALSE
)

primary_att_abort_dynamic <- aggte(primary_att_abort, type = "dynamic")

primary_abort_dynamic_plot <- ggdid(primary_att_abort_dynamic)

primary_att_abort_calendar <- aggte(primary_att_abort, type = "calendar")

primary_abort_calendar_plot <- ggdid(primary_att_abort_calendar)

primary_att_group_abort <- aggte(primary_att_abort, type = "group")
primary_abort_group_plot <- ggdid(primary_att_group_abprt)

primary_att_sb_dynamic <- aggte(primary_sb_att, type = "dynamic")

primary_sb_dynamic_plot <- ggdid(primary_att_sb_dynamic)

primary_att_sb_calendar <- aggte(primary_sb_att, type = "calendar")

primary_sb_calendar_plot <- ggdid(primary_att_sb_calendar)

primary_att_group_sb <- aggte(primary_sb_att, type = "group")
primary_sb_group_plot <- ggdid(primary_att_group_sb)

primary_att_miscarriage_dynamic <- aggte(primary_att_miscarriage, type = "dynamic")

primary_miscarriage_dynamic_plot <- ggdid(primary_att_miscarriage_dynamic)

primary_att_miscarriage_calendar <- aggte(primary_att_miscarriage, type = "calendar")

primary_miscarriage_calendar_plot <- ggdid(primary_att_miscarriage_calendar)

primary_att_group_miscarriage <- aggte(primary_att_miscarriage, type = "group")
primary_miscarriage_group_plot <- ggdid(primary_att_group_miscarriage)

nonprimary_att_abort_dynamic <- aggte(nonprimary_att_abort, type = "dynamic")

nonprimary_abort_dynamic_plot <- ggdid(nonprimary_att_abort_dynamic)

nonprimary_att_abort_calendar <- aggte(nonprimary_att_abort, type = "calendar")

nonprimary_abort_calendar_plot <- ggdid(nonprimary_att_abort_calendar)

nonprimary_att_group_abort <- aggte(nonprimary_att_abort, type = "group")
nonprimary_abort_group_plot <- ggdid(nonprimary_att_group_abort)

nonprimary_att_sb_dynamic <- aggte(nonprimary_sb_att, type = "dynamic")

nonprimary_sb_dynamic_plot <- ggdid(nonprimary_att_sb_dynamic)

nonprimary_att_sb_calendar <- aggte(nonprimary_sb_att, type = "calendar")

nonprimary_sb_calendar_plot <- ggdid(nonprimary_att_sb_calendar)

nonprimary_att_group_sb <- aggte(nonprimary_sb_att, type = "group")
nonprimary_sb_group_plot <- ggdid(nonprimary_att_group_sb)

nonprimary_att_miscarriage_dynamic <- aggte(nonprimary_att_miscarriage, type = "dynamic")

nonprimary_miscarriage_dynamic_plot <- ggdid(nonprimary_att_miscarriage_dynamic)

nonprimary_att_miscarriage_calendar <- aggte(nonprimary_att_miscarriage, type = "calendar")

nonprimary_miscarriage_calendar_plot <- ggdid(nonprimary_att_miscarriage_calendar)

nonprimary_att_group_miscarriage <- aggte(nonprimary_att_miscarriage, type = "group")
nonprimary_miscarriage_group_plot <- ggdid(nonprimary_att_group_miscarriage)


ggdid(quint1_sb_att)
#make into one plot

quint1.sb.es <- aggte(quint1_sb_att, type="dynamic")
ggdid(quint1.sb.es)



# Wooldridge approach -----------------------------------------------------

library(etwfe)

#filtering out 21 obs without age
df_mod <- df %>% filter(!is.na(age))
df_mod$poorest <- ifelse(df_mod$wi_quintile == 1, 1, 0)

mod_sb <-
  etwfe(
    fml  = sb ~ 1,
      #strat_rurb + age, 
    tvar = outcome_year,        
    gvar = g, 
    ivar = dist_id,
    data = df_mod,       # dataset
    xvar = wi_quintile,
    cgroup = "notyet",
    vcov = ~dist_id  # vcov adjustment (here: clustered)
  )

mod_abort <-
  etwfe(
    fml  = abort ~ strat_rurb + age, 
    tvar = outcome_year,        
    gvar = g, 
    ivar = dist_id,
    data = df_mod,       # dataset
    xvar = wi_quintile,
    cgroup = "notyet",
    vcov = ~dist_id  # vcov adjustment (here: clustered)
  )

mod_ms <-
  etwfe(
    fml  = miscarriage ~ strat_rurb + age, 
    tvar = outcome_year,        
    gvar = g, 
    ivar = dist_id,
    data = df_mod,       # dataset
    xvar = wi_quintile,
    cgroup = "notyet",
    vcov = ~dist_id  # vcov adjustment (here: clustered)
  )


emfx(mod, type = "calendar")

emfx(mod_sb, 
  newdata = datagrid(wi_quintile = 1:5),
  variables = "outcome_year")

mod_es_sb = emfx(mod_sb, type = "event", post_only = FALSE)
mod_es_abort = emfx(mod_abort, type = "event", post_only = FALSE)
mod_es_ms = emfx(mod_ms, type = "event", post_only = FALSE)

mod_cal_sb <- emfx(mod_sb, type = "calendar")
mod_cal_abort <- emfx(mod_abort, type = "calendar")
mod_cal_ms <- emfx(mod_ms, type = "calendar")

ggplot(mod_es, aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high))

emfx(mod, hypothesis = "b1 = b2")

avg_mod <- avg_comparisons(mod, 
                             variables = list(wi_quintile = c(1,5)), 
                             by = "outcome_year")


library(emmeans)

emmeans(mod_sb, "wi_quintile")

contrast(emmeans(mod_sb, "x2", at = list(x1 = 90, x2 = c(2, 5))), "revpairwise", infer = c(TRUE, TRUE))


marginaleffects::slopes(mod_sb)


# Archive -----------------------------------------------------------------


# DiD in NFHS only and lowest wealth quintile only

nfhs <- df %>% filter(survey == "NFHS4" | survey == "NFHS5")


#trying something
nfhs <- nfhs %>% mutate(g = case_when(treat == 0 ~ 0,
                                      treat == 1 ~ 2010,
                                      treat == 2 ~ 2012, 
                                      treat == 3 ~ 2014,
                                      TRUE ~ NA_real_))


nfhs$wealth_year <- nfhs$outcome_year * nfhs$wi_perc_rank

xformula <- ~ rural_urban + age

nfhs_check_att <- att_gt(yname = "sb",
                         tname = "outcome_year",
                         gname = "g",
                         idname = "id",
                         #xformla = xformula,
                         data = nfhs,
                         panel = FALSE,
                         weightsname = "weight_adj",
                         control_group = "notyettreated",
                         print_details = TRUE,
                         est_method = "reg",
                         bstrap=FALSE, cband=FALSE
)
#ATT difference
sb_att_diff <- cbind(quint1_sb_att$group, quint1_sb_att$t, (quint_high_sb_att$att - quint1_sb_att$att))

#calculating CIs using mboot. First need to combine the influence functions

# formula to combine influence functions 
# IF_combined <- w1 * did_est1$influence.function + w2 * did_est2$influence.function

# first need to calculate each wif -- weighted extra term influence function
# load parameters

w1 <- 0.5
w2 <- 0.5
IF_combined_sb <- (w1 * quint_high_sb_att$inffunc) + (w2 * quint1_sb_att$inffunc)

quint1_sb_wif <- wif()

att_diff_func <- function(df) {
  att_treatment_boot <- att(data, treatment_var, outcome_var, group1 = treatment_group)
  att_control_boot <- att(data, treatment_var, outcome_var, group1 = control_group)
  return(att_treatment_boot$ATE - att_control_boot$ATE)
}

conditional_did_pretest(yname = "sb",
                        tname = "outcome_year",
                        gname = "g",
                        idname = "id",
                        data = nfhs,
                        panel = FALSE,
                        weightsname = "weight_adj",
                        control_group = "notyettreated")

summary(nfhs_check_att)

ggdid(nfhs_check_att)

# Grouped district analyses -----------------------------------------------

#setting design
#making strata variable from rural/urban
df$strat_rurb <- ifelse(df$rural_urban == 0, 2, 1)
table(df$strat_rurb)
df$year <- as.factor(df$outcome_year)
df$dists_fact <- as.factor(df$dist_id)

design <- svydesign(data = df, ids = ~psu2, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)

svy_unweighted <- lm(sb ~ dists_fact * year, data = df)

svy_unweight_nfhs <- lm(sb ~ dist_id * year, data = nfhs)

svy_dist_rate <- svyglm(~sb, ~outcome_year*~dist_id*wi_quintile, design, svymean, vartype=c("se","ci"), na.rm.all = TRUE)

poor <- df %>% filter(wi_quintile ==1)
design_poor <- svydesign(data = poor, ids = ~psu2, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)
svy_dist_poor <- svyby(~sb, ~outcome_year*~dists_fact, design_poor, svymean, vartype=c("se","ci"), na.rm.all = TRUE)

#want for each district rate by year 
rates <- df %>% group_by(dist_id, outcome_year, wi_quintile) %>% summarise(weighted_sb_rate = weighted.mean(sb, weight_adj),
                                                                           weighted_abort_rate = weighted.mean(abort, weight_adj),
                                                                           weighted_ms_rate = weighted.mean(miscarriage, weight_adj))

rates_wide <- pivot_wider(rates, names_from = wi_quintile, values_from = c(weighted_sb_rate, weighted_abort_rate, weighted_ms_rate)) 

#only care about difference between richest and poorest


#re-formatting data to try 

# first assigning EAG status by state
# EAG states are Bihar (10), Chhattisgarh (22), Jharkhand (20), 
# Madhya Pradesh (23), Orissa (21), Rajasthan (8), Uttaranchal (5), and Uttar Pradesh (9)

df <- df %>% mutate(EAG = case_when(state == 10 ~ 1,
                                    state == 22 ~ 1,
                                    state == 20 ~ 1,
                                    state == 23 ~ 1,
                                    state == 21 ~ 1,
                                    state == 8 ~ 1,
                                    state == 5 ~ 1, 
                                    state == 9 ~ 1,
                                    TRUE ~ 0))

table(df$state, df$EAG)

#making factor rural / urban as numeric. 1 is urban, 0 is rural
df$urban <- as.numeric(as.character(df$rural_urban))

#Group by district and year, have overall rates / 100,000 of S / A / M and then differences in rates / 100,000 between bottom and
#top of wealth distribution (1 vs. 5?)

# want % of people in lowest wealth quintile -- making variable with # district in lowest wealth quintile

df_dist <- df %>% 
  group_by(dist_id, outcome_year) %>%
  summarise(count_sb = sum(sb), count_abort = sum(abort), count_ms = sum(miscarriage), count_pregs = n(), g = mean(g),
            prop_first_wi_quint = round(100*sum(wi_quintile==1)/n(),2), eag = sum(EAG), mom_age = mean(age, na.rm = T),
            eag_min = min(EAG), eag_max = max(EAG))




df_dist_poor <- poor %>% 
  group_by(dist_id, outcome_year) %>%
  summarise(count_sb = sum(sb), count_abort = sum(abort), count_ms = sum(miscarriage), count_pregs = n(), g = mean(g),
            prop_first_wi_quint = round(100*sum(wi_quintile==1)/n(),2), eag = sum(EAG), mom_age = mean(age, na.rm = T),
            eag_min = min(EAG), eag_max = max(EAG))

#making rate per 100000

df_dist$sb_rate <- (df_dist$count_sb / df_dist$count_pregs)*1000
df_dist$abort_rate <- (df_dist$count_abort / df_dist$count_pregs)*1000
df_dist$ms_rate <- (df_dist$count_ms / df_dist$count_pregs)*1000

df_dist_poor$sb_rate <- (df_dist_poor$count_sb / df_dist_poor$count_pregs)*1000
df_dist_poor$abort_rate <- (df_dist_poor$count_abort / df_dist_poor$count_pregs)*1000
df_dist_poor$ms_rate <- (df_dist_poor$count_ms / df_dist_poor$count_pregs)*1000


#making EAG 1 / 0
df_dist$EAG <- ifelse(df_dist$eag > 0, 1, 0)

df_dist_poor$EAG <- ifelse(df_dist_poor$eag > 0, 1, 0)

# making urban 1 / 0



dist_att_sb <- att_gt(yname = "sb_rate",
                      tname = "outcome_year",
                      gname = "g",
                      idname = "dist_id",
                      xformla = ~ prop_first_wi_quint + EAG + (1 + mom_age),
                      data = df_dist,
                      panel = FALSE,
                      #allow_unbalanced_panel = FALSE,
                      #weightsname = "weight_adj",
                      clustervars = c("dist_id"),
                      control_group = "notyettreated",
                      bstrap=TRUE #, cband=FALSE
)


dist_att_sb_poor <- att_gt(yname = "sb_rate",
                           tname = "outcome_year",
                           gname = "g",
                           idname = "dist_id",
                           #xformla = ~ prop_first_wi_quint + EAG + (1 + mom_age),
                           data = df_dist_poor,
                           panel = FALSE,
                           allow_unbalanced_panel = TRUE,
                           #weightsname = "weight_adj",
                           clustervars = c("dist_id"),
                           control_group = "notyettreated",
                           bstrap=TRUE #, cband=FALSE
)

dist_att_abort_poor <- att_gt(yname = "abort_rate",
                              tname = "outcome_year",
                              gname = "g",
                              idname = "dist_id",
                              #xformla = ~ prop_first_wi_quint + EAG + (1 + mom_age),
                              data = df_dist_poor,
                              panel = FALSE,
                              allow_unbalanced_panel = TRUE,
                              #weightsname = "weight_adj",
                              clustervars = c("dist_id"),
                              control_group = "notyettreated",
                              bstrap=TRUE #, cband=FALSE
)

dist_att_ms_poor <- att_gt(yname = "ms_rate",
                           tname = "outcome_year",
                           gname = "g",
                           idname = "dist_id",
                           #xformla = ~ prop_first_wi_quint + EAG + (1 + mom_age),
                           data = df_dist_poor,
                           panel = FALSE,
                           allow_unbalanced_panel = TRUE,
                           #weightsname = "weight_adj",
                           clustervars = c("dist_id"),
                           control_group = "notyettreated",
                           bstrap=TRUE #, cband=FALSE
)

ggdid(dist_att_sb_poor)
ggdid(dist_att_abort_poor)
ggdid(dist_att_ms_poor)

summary(dist_att_sb)

ggdid(dist_att_sb)


dist_sb_es <- aggte(dist_att_sb, type = "dynamic")

ggdid(dist_sb_es)

dist_sb_cal <- aggte(dist_att_sb, type = "calendar")
ggdid(dist_sb_cal)

dist_att_abort <- att_gt(yname = "abort_rate",
                         tname = "outcome_year",
                         gname = "g",
                         idname = "dist_id",
                         #xformla = xformula,
                         data = df_dist,
                         panel = FALSE,
                         #allow_unbalanced_panel = FALSE,
                         #weightsname = "weight_adj",
                         clustervars = c("dist_id"),
                         control_group = "notyettreated",
                         bstrap=FALSE, cband=FALSE
)

summary(dist_att_abort)

ggdid(dist_att_abort)


dist_abort_es <- aggte(dist_att_abort, type = "dynamic")

ggdid(dist_abort_es)

dist_abort_cal <- aggte(dist_att_abort, type = "calendar")
ggdid(dist_abort_cal)

dist_att_ms <- att_gt(yname = "ms_rate",
                      tname = "outcome_year",
                      gname = "g",
                      idname = "dist_id",
                      #xformla = xformula,
                      data = df_dist,
                      panel = TRUE,
                      #allow_unbalanced_panel = FALSE,
                      #weightsname = "weight_adj",
                      clustervars = c("dist_id"),
                      control_group = "notyettreated",
                      bstrap=FALSE, cband=FALSE
)

summary(dist_att_ms)

ggdid(dist_att_ms)


dist_ms_es <- aggte(dist_att_ms, type = "dynamic")

ggdid(dist_ms_es)

dist_ms_cal <- aggte(dist_att_ms, type = "calendar")
ggdid(dist_ms_cal)

#for SES drop everyone not in SES groups 1 and 5 and calculate differences between them?
# Grouped district analyses -----------------------------------------------

#setting design
#making strata variable from rural/urban
df$strat_rurb <- ifelse(df$rural_urban == 0, 2, 1)
table(df$strat_rurb)
df$year <- as.factor(df$outcome_year)
df$dists_fact <- as.factor(df$dist_id)

design <- svydesign(data = df, ids = ~psu2, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)

svy_unweighted <- lm(sb ~ dists_fact * year, data = df)

svy_unweight_nfhs <- lm(sb ~ dist_id * year, data = nfhs)

svy_dist_rate <- svyglm(~sb, ~outcome_year*~dist_id*wi_quintile, design, svymean, vartype=c("se","ci"), na.rm.all = TRUE)

poor <- df %>% filter(wi_quintile ==1)
design_poor <- svydesign(data = poor, ids = ~psu2, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)
svy_dist_poor <- svyby(~sb, ~outcome_year*~dists_fact, design_poor, svymean, vartype=c("se","ci"), na.rm.all = TRUE)

#want for each district rate by year 
rates <- df %>% group_by(dist_id, outcome_year, wi_quintile) %>% summarise(weighted_sb_rate = weighted.mean(sb, weight_adj),
                                                                           weighted_abort_rate = weighted.mean(abort, weight_adj),
                                                                           weighted_ms_rate = weighted.mean(miscarriage, weight_adj))

rates_wide <- pivot_wider(rates, names_from = wi_quintile, values_from = c(weighted_sb_rate, weighted_abort_rate, weighted_ms_rate)) 

#only care about difference between richest and poorest


#re-formatting data to try 

# first assigning EAG status by state
# EAG states are Bihar (10), Chhattisgarh (22), Jharkhand (20), 
# Madhya Pradesh (23), Orissa (21), Rajasthan (8), Uttaranchal (5), and Uttar Pradesh (9)

df <- df %>% mutate(EAG = case_when(state == 10 ~ 1,
                                    state == 22 ~ 1,
                                    state == 20 ~ 1,
                                    state == 23 ~ 1,
                                    state == 21 ~ 1,
                                    state == 8 ~ 1,
                                    state == 5 ~ 1, 
                                    state == 9 ~ 1,
                                    TRUE ~ 0))

table(df$state, df$EAG)

#making factor rural / urban as numeric. 1 is urban, 0 is rural
df$urban <- as.numeric(as.character(df$rural_urban))

#Group by district and year, have overall rates / 100,000 of S / A / M and then differences in rates / 100,000 between bottom and
#top of wealth distribution (1 vs. 5?)

# want % of people in lowest wealth quintile -- making variable with # district in lowest wealth quintile

df_dist <- df %>% 
  group_by(dist_id, outcome_year) %>%
  summarise(count_sb = sum(sb), count_abort = sum(abort), count_ms = sum(miscarriage), count_pregs = n(), g = mean(g),
            prop_first_wi_quint = round(100*sum(wi_quintile==1)/n(),2), eag = sum(EAG), mom_age = mean(age, na.rm = T),
            eag_min = min(EAG), eag_max = max(EAG))




df_dist_poor <- poor %>% 
  group_by(dist_id, outcome_year) %>%
  summarise(count_sb = sum(sb), count_abort = sum(abort), count_ms = sum(miscarriage), count_pregs = n(), g = mean(g),
            prop_first_wi_quint = round(100*sum(wi_quintile==1)/n(),2), eag = sum(EAG), mom_age = mean(age, na.rm = T),
            eag_min = min(EAG), eag_max = max(EAG))

#making rate per 100000

df_dist$sb_rate <- (df_dist$count_sb / df_dist$count_pregs)*1000
df_dist$abort_rate <- (df_dist$count_abort / df_dist$count_pregs)*1000
df_dist$ms_rate <- (df_dist$count_ms / df_dist$count_pregs)*1000

df_dist_poor$sb_rate <- (df_dist_poor$count_sb / df_dist_poor$count_pregs)*1000
df_dist_poor$abort_rate <- (df_dist_poor$count_abort / df_dist_poor$count_pregs)*1000
df_dist_poor$ms_rate <- (df_dist_poor$count_ms / df_dist_poor$count_pregs)*1000


#making EAG 1 / 0
df_dist$EAG <- ifelse(df_dist$eag > 0, 1, 0)

df_dist_poor$EAG <- ifelse(df_dist_poor$eag > 0, 1, 0)

# making urban 1 / 0



dist_att_sb <- att_gt(yname = "sb_rate",
                      tname = "outcome_year",
                      gname = "g",
                      idname = "dist_id",
                      xformla = ~ prop_first_wi_quint + EAG + (1 + mom_age),
                      data = df_dist,
                      panel = FALSE,
                      #allow_unbalanced_panel = FALSE,
                      #weightsname = "weight_adj",
                      clustervars = c("dist_id"),
                      control_group = "notyettreated",
                      bstrap=TRUE #, cband=FALSE
)


dist_att_sb_poor <- att_gt(yname = "sb_rate",
                           tname = "outcome_year",
                           gname = "g",
                           idname = "dist_id",
                           #xformla = ~ prop_first_wi_quint + EAG + (1 + mom_age),
                           data = df_dist_poor,
                           panel = FALSE,
                           allow_unbalanced_panel = TRUE,
                           #weightsname = "weight_adj",
                           clustervars = c("dist_id"),
                           control_group = "notyettreated",
                           bstrap=TRUE #, cband=FALSE
)

dist_att_abort_poor <- att_gt(yname = "abort_rate",
                              tname = "outcome_year",
                              gname = "g",
                              idname = "dist_id",
                              #xformla = ~ prop_first_wi_quint + EAG + (1 + mom_age),
                              data = df_dist_poor,
                              panel = FALSE,
                              allow_unbalanced_panel = TRUE,
                              #weightsname = "weight_adj",
                              clustervars = c("dist_id"),
                              control_group = "notyettreated",
                              bstrap=TRUE #, cband=FALSE
)

dist_att_ms_poor <- att_gt(yname = "ms_rate",
                           tname = "outcome_year",
                           gname = "g",
                           idname = "dist_id",
                           #xformla = ~ prop_first_wi_quint + EAG + (1 + mom_age),
                           data = df_dist_poor,
                           panel = FALSE,
                           allow_unbalanced_panel = TRUE,
                           #weightsname = "weight_adj",
                           clustervars = c("dist_id"),
                           control_group = "notyettreated",
                           bstrap=TRUE #, cband=FALSE
)

ggdid(dist_att_sb_poor)
ggdid(dist_att_abort_poor)
ggdid(dist_att_ms_poor)

summary(dist_att_sb)

ggdid(dist_att_sb)


dist_sb_es <- aggte(dist_att_sb, type = "dynamic")

ggdid(dist_sb_es)

dist_sb_cal <- aggte(dist_att_sb, type = "calendar")
ggdid(dist_sb_cal)

dist_att_abort <- att_gt(yname = "abort_rate",
                         tname = "outcome_year",
                         gname = "g",
                         idname = "dist_id",
                         #xformla = xformula,
                         data = df_dist,
                         panel = FALSE,
                         #allow_unbalanced_panel = FALSE,
                         #weightsname = "weight_adj",
                         clustervars = c("dist_id"),
                         control_group = "notyettreated",
                         bstrap=FALSE, cband=FALSE
)

summary(dist_att_abort)

ggdid(dist_att_abort)


dist_abort_es <- aggte(dist_att_abort, type = "dynamic")

ggdid(dist_abort_es)

dist_abort_cal <- aggte(dist_att_abort, type = "calendar")
ggdid(dist_abort_cal)

dist_att_ms <- att_gt(yname = "ms_rate",
                      tname = "outcome_year",
                      gname = "g",
                      idname = "dist_id",
                      #xformla = xformula,
                      data = df_dist,
                      panel = TRUE,
                      #allow_unbalanced_panel = FALSE,
                      #weightsname = "weight_adj",
                      clustervars = c("dist_id"),
                      control_group = "notyettreated",
                      bstrap=FALSE, cband=FALSE
)

summary(dist_att_ms)

ggdid(dist_att_ms)


dist_ms_es <- aggte(dist_att_ms, type = "dynamic")

ggdid(dist_ms_es)

dist_ms_cal <- aggte(dist_att_ms, type = "calendar")
ggdid(dist_ms_cal)

#for SES drop everyone not in SES groups 1 and 5 and calculate differences between them?


did::aggte(nfhs_check_att)
aggte(nfhs_check_att, type = "calendar")

#creating leads and lags
#need to get first year 



