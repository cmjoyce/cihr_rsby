
# Begin -------------------------------------------------------------------


library(tidyverse)
library(cowplot)
library(did)
library(survey)
library(etwfe)
library(ggpubr)
library(flextable)
library(janitor)

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

#May 16 making maharashtra only dataset for Ari (state 26)
df_treatment <- read.csv("df_treatment_assigned.csv")

df_maharashtra <- df_treatment %>% filter(state == 26) %>% group_by(dist_id)
df_maharashtra <- df_maharashtra %>% select(-c(X.1, X))

df_maharashtra_access <- df_maharashtra %>% group_by(dist_id) %>% select(survey, state, access_2010, Enrolled_2010, access_2011, Enrolled_2011,
                                                                         access_2012, Enrolled_2012, access_2013, Enrolled_2013, access_2014,
                                                                         Enrolled_2014, access_2015, Enrolled_2015, access_2016, Enrolled_2016,
                                                                         accessyears)

check <- df_maharashtra_access %>% group_by(dist_id) %>% summarise(district = mean(dist_id), state = mean(state), enrollment2010 = mean(Enrolled_2010),
                                                                   access2010 = mean(access_2010), enrollment2011 = mean(Enrolled_2011), access2011 =
                                                                     mean(access_2011), enrollment2012 = mean(Enrolled_2012), access2012 = mean(access_2012),
                                                                   enrollment2013 = mean(Enrolled_2013), access2013 = mean(access_2013), 
                                                                   enrollment2014 = mean(Enrolled_2014), access2014 = mean(access_2014),
                                                                   enrollment2015 = mean(Enrolled_2015), access2015 = mean(access_2015),
                                                                   enrollment2016 = mean(Enrolled_2016), access2016 = mean(access_2016))

write.csv(check, "df_maharashtra_access.csv")


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
library(gtsummary)
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


#Table 1

#rural == 0, 1s as they must be urbans.
df$strat_rurb <- ifelse(df$rural_urban == 0, 2, 1)
table(df$strat_rurb)


df$ruralurban <- factor(df$strat_rurb, 
                         levels = c(1, 2),
                         labels = c("Urban", "Rural"))

df$scheduled_c_t <- factor(df$caste_group,
                           levels = c(0,1,2),
                           labels = c("None", "Scheduled Caste", "Scheduled Tribe"))

table(df$treat)
df$treated <- ifelse(df$treat > 0, 1, 0)
table(df$treated)

df$treated <- factor(df$treated,
                     levels = c(0,1),
                     labels = c("No", "Yes"))

df$accessyear <- factor(df$g,
                        levels = c(2010, 2012, 2014),
                        labels = c("Early (2010)", "Mid (2012)", "Late (2014)"))

df <- df %>% mutate(exposure = case_when(g == 2010 ~ "Early (2010)",
                                         g == 2012 ~ "Mid (2012)",
                                         g == 2014 ~ "Late (2014)"))


t1_strat <- df %>% 
  select(treated, age, ruralurban, scheduled_c_t, primary_school, g) %>% 
  mutate(treated = case_when(treated == "No" ~ "Never Treated",
                             treated == "Yes" ~ "Ever Treated")) %>%
  tbl_strata(
    strata = treated,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(label = 
                    list(
                      age ~ "Age",
                      ruralurban ~ "Rural / Urban",
                      scheduled_c_t ~ "Member of Scheduled Caste or Scheduled Tribe",
                      primary_school ~ "Completed Primary School",
                      g ~ "Year District Received Access By"),
                  statistic = list(all_continuous() ~ "{mean} ({sd})"),
                  missing_text =  "Missing"
                  )
  ) %>% modify_header(label = "**Variable**")  %>%  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman")

library(webshot)
gt::gtsave(t1_strat, "tab_1.png", expand = 10)

t1_poster <- df %>% 
  select(treated, age, ruralurban, scheduled_c_t, primary_school, g) %>% 
  mutate(treated = case_when(treated == "No" ~ "Never Treated",
                             treated == "Yes" ~ "Ever Treated")) %>%
  tbl_strata(
    strata = treated,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(label = 
                    list(
                      age ~ "Age",
                      ruralurban ~ "Rural / Urban",
                      scheduled_c_t ~ "Member of Scheduled Caste or Scheduled Tribe",
                      primary_school ~ "Completed Primary School",
                      g ~ "Year District Received Access By"),
                  statistic = list(all_continuous() ~ "{mean} ({sd})"),
                  missing_text =  "Missing"
      )
  ) %>% modify_header(label = "**Variable**")  %>%  as_gt() %>%
  gt::tab_options(table.font.weight = "bold", table.font.names = "Times New Roman", table.font.size = 22, 
                  column_labels.border.top.color = "black", column_labels.border.bottom.color = "black", table.border.bottom.color = "black", 
                  table_body.border.bottom.color = "black", table_body.hlines.color = "black" )

library(webshot2)
gt::gtsave(t1_poster, "tab_1_poster.png", expand = 10)

## creating simulated plot for thesis chapter

N <- 100000
sim_df <- data.frame(year = runif(N, -9, 9)) # Number of pollination year
sim_df <- dplyr::mutate(sim_df, year.c = (year - mean(year))/sd(year))
alpha <- 250
beta <- 50
sim_df <- dplyr::mutate(sim_df, mu = alpha + beta*year.c)
#ggplot(data = sim_df, aes(x = year, y = mu)) + geom_smooth()
sim_df <- sim_df %>% mutate(y = mu*4)

sim_df <- sim_df %>% mutate(rate_of_y = case_when(year > 0 ~ y*2,
                                                  TRUE ~ y))

sim_df$group <- c("Early")

sim_df_mid <- data.frame(year = runif(N, -9, 9)) # Number of pollination year
sim_df_mid <- dplyr::mutate(sim_df_mid, year.c = (year - mean(year))/sd(year))
alpha <- 250
beta <- 50
sim_df_mid <- dplyr::mutate(sim_df_mid, mu = alpha + beta*year.c)
#ggplot(data = sim_df_mid, aes(x = year, y = mu)) + geom_smooth()
sim_df_mid <- sim_df %>% mutate(y = mu*3)

sim_df_mid <- sim_df_mid %>% mutate(rate_of_y = case_when(year > 3 ~ y*2,
                                                  TRUE ~ y))

sim_df_mid$group <- c("Mid")

sim_df_late <- data.frame(year = runif(N, -9, 9)) # Number of pollination year
sim_df_late <- dplyr::mutate(sim_df_late, year.c = (year - mean(year))/sd(year))
alpha <- 250
beta <- 50
sim_df_late <- dplyr::mutate(sim_df_late, mu = alpha + beta*year.c)
#ggplot(data = sim_df_late, aes(x = year, y = mu)) + geom_smooth()
sim_df_late <- sim_df_late %>% mutate(y = mu*2)
sim_df_late <- sim_df_late %>% mutate(rate_of_y = case_when(year > 6 ~ y*2,
                                                          TRUE ~ y))

sim_df_late$group <- c("Late")

sim_df_none <- data.frame(year = runif(N, -9, 9)) # Number of pollination year
sim_df_none <- dplyr::mutate(sim_df_none, year.c = (year - mean(year))/sd(year))
alpha <- 250
beta <- 50
sim_df_none <- dplyr::mutate(sim_df_none, mu = alpha + beta*year.c)
#ggplot(data = sim_df_none, aes(x = year, y = mu)) + geom_smooth()
sim_df_none <- sim_df_none %>% mutate(rate_of_y = mu)
sim_df_none <- sim_df_none %>% mutate(y = mu)

sim_df_none$group <- c("None")

#sim_df <- sim_df %>% select(-c(y))

sim_df_group <- rbind(sim_df, sim_df_mid, sim_df_late, sim_df_none)

ggplot(data = sim_df_group, aes(x = year, y = rate_of_y, color = (group))) + geom_line(linewidth = 1) + 
  scale_y_continuous(n.breaks = 6)+
  scale_x_continuous(n.breaks = 10)+
  theme_cowplot()+
  geom_vline(xintercept = 0, linetype = 2) + geom_vline(xintercept = 3, linetype = 2) + geom_vline(xintercept = 6, linetype = 2) +
  labs(x = "Year", y = "Rate of Outcome",color = "Treatment Group")



# DiD ---------------------------------------------------------------------

df <- df %>% rename(id = X)
str(df$state)
df$state <- as.factor(df$state)
str(df$rural_urban) 
df$rural_urban <- as.factor(df$rural_urban)

str(df$age)

#making age category 
df <- df %>% mutate(age_cat = case_when(age < 20 ~ 1,
                                        age > 19 & age < 25 ~ 2,
                                        age > 24 & age < 30 ~ 3,
                                        age > 29 & age < 35 ~ 4,
                                        age > 34 & age < 40 ~ 5,
                                        age > 39 & age < 45 ~ 6,
                                        age >= 45 ~ 7))

df$age_cat <- as.factor(df$age_cat)
df$age_squred <- df$age^2

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


df <- df %>% mutate(g = case_when(treat == 0 ~ 0,
                                  treat == 1 ~ 2010,
                                  treat == 2 ~ 2012, 
                                  treat == 3 ~ 2014,
                                  TRUE ~ NA_real_))




# district panel ----------------------------------------------------------

dists_df <- read.csv("dist_rates_weighted.csv")
names(dists_df)

table(dists_df$dist_id, dists_df$outcome_year)
dists_df <- dists_df %>% select(-c(X))

dists_df$sb_rate <- (dists_df$sb / dists_df$pregs)*1000
dists_df$abort_rate <- (dists_df$abort / dists_df$pregs)*1000
dists_df$ms_rate <- (dists_df$ms / dists_df$pregs)*1000

dist_sb <- att_gt(yname = "sb_rate",
                  tname = "outcome_year",
                  gname = "enrollgroup",
                  idname = "dist_id",
                  xformla = ~ (med_age^2),
                  data = dists_df,
                  panel = TRUE,
                  clustervars = "dist_id",
                  control_group = "notyettreated",
                  print_details = TRUE,
                  bstrap=TRUE, cband=FALSE
)

dist_abort <- att_gt(yname = "abort_rate",
                     tname = "outcome_year",
                     gname = "enrollgroup",
                     idname = "dist_id",
                     xformla = ~ (med_age^2),
                     data = dists_df,
                     panel = TRUE,
                     clustervars = "dist_id",
                     control_group = "notyettreated",
                     print_details = TRUE,
                     bstrap=TRUE, cband=FALSE
)

dist_ms <- att_gt(yname = "ms_rate",
                  tname = "outcome_year",
                  gname = "enrollgroup",
                  idname = "dist_id",
                  xformla = ~ (med_age^2),
                  data = dists_df,
                  panel = TRUE,
                  clustervars = "dist_id",
                  control_group = "notyettreated",
                  print_details = TRUE,
                  bstrap=TRUE, cband=FALSE
)

dists_sb_group <- aggte(dist_sb, type = "group")
dists_sb_dynamic <- aggte(dist_sb, type = "dynamic")
dists_sb_calendar <- aggte(dist_sb, type = "calendar")

dists_abort_group <- aggte(dist_abort, type = "group")
dists_abort_dynamic <- aggte(dist_abort, type = "dynamic")
dists_abort_calendar <- aggte(dist_abort, type = "calendar")

dists_ms_group <- aggte(dist_ms, type = "group")
dists_ms_dynamic <- aggte(dist_ms, type = "dynamic")
dists_ms_calendar <- aggte(dist_ms, type = "calendar")

dists_sb_dynamic.dyn <- data.frame(time=dists_sb_dynamic[["egt"]],
                                                att=dists_sb_dynamic[["att.egt"]],
                                                se=dists_sb_dynamic[["se.egt"]])

dists_sb_dynamic.dyn  <- dists_sb_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))

ggplot(dists_sb_dynamic.dyn)+
  geom_rect( aes(xmin=0.1,
                 xmax=9,
                 ymin=(dists_sb_dynamic[["overall.att"]]-1.96*dists_sb_dynamic[["overall.se"]]),
                 ymax=(dists_sb_dynamic[["overall.att"]]+1.96*dists_sb_dynamic[["overall.se"]]), alpha="95% CI"), fill = alpha("lightblue"),
             color="lightblue")+
  geom_segment( 
    aes(x = 0, y = dists_sb_dynamic[["overall.att"]], xend = 9, yend = dists_sb_dynamic[["overall.att"]], lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att,2),ymin = round((att-1.96*se),2), ymax = round((att+1.96*se),2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of stillbirths per 1,000 pregnancies")+
  xlab("Time since received access to RSBY")+
  scale_x_continuous(breaks=c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9),
                     labels = c("-9","-8","-7","6","-5","-4","-3","-2","-1","0","+1","+2","+3","+4","+5","+6","+7","+8","+9"))+
  ylim(-10,10)+
  
  scale_color_manual(values=c("#e67e00","#024b7a"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  geom_vline(xintercept=-0.1,lty=2,color="gray59")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  theme_bw()+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  theme_cowplot()

fig1_poster <- ggplot(dists_sb_dynamic.dyn)+
  geom_rect( aes(xmin=0.1,
                 xmax=9,
                 ymin=(dists_sb_dynamic[["overall.att"]]-1.96*dists_sb_dynamic[["overall.se"]]),
                 ymax=(dists_sb_dynamic[["overall.att"]]+1.96*dists_sb_dynamic[["overall.se"]]), alpha="95% CI"), fill = alpha("lightblue"),
             color="lightblue")+
  geom_segment( 
    aes(x = 0, y = dists_sb_dynamic[["overall.att"]], xend = 9, yend = dists_sb_dynamic[["overall.att"]], lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att,2),ymin = round((att-1.96*se),2), ymax = round((att+1.96*se),2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of stillbirths per 1,000 pregnancies")+
  xlab("Time since received access to RSBY")+
  scale_x_continuous(breaks=c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9),
                     labels = c("-9","-8","-7","6","-5","-4","-3","-2","-1","0","+1","+2","+3","+4","+5","+6","+7","+8","+9"))+
  ylim(-10,10)+
  
  scale_color_manual(values=c("#e67e00","#024b7a"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  geom_vline(xintercept=-0.1,lty=2,color="gray59")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  theme_bw()+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  theme_cowplot(26)

cowplot::save_plot(
  "fig1poster.png",
  plot = fig1_poster,
  base_height = 10,
  base_width = 20
)



dists_abort_dynamic.dyn <- data.frame(time=dists_abort_dynamic[["egt"]],
                                   att=dists_abort_dynamic[["att.egt"]],
                                   se=dists_abort_dynamic[["se.egt"]])

dists_abort_dynamic.dyn  <- dists_abort_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))

ggplot(dists_abort_dynamic.dyn)+
  geom_rect( aes(xmin=0.1,
                 xmax=9,
                 ymin=(dists_abort_dynamic[["overall.att"]]-1.96*dists_abort_dynamic[["overall.se"]]),
                 ymax=(dists_abort_dynamic[["overall.att"]]+1.96*dists_abort_dynamic[["overall.se"]]), alpha="95% CI"), fill = alpha("lightblue"),
             color="lightblue")+
  geom_segment( 
    aes(x = 0, y = dists_abort_dynamic[["overall.att"]], xend = 9, yend = dists_abort_dynamic[["overall.att"]], lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att,2),ymin = round((att-1.96*se),2), ymax = round((att+1.96*se),2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of abortions per 1,000 pregnancies")+
  xlab("Time since received access to RSBY")+
  scale_x_continuous(breaks=c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9),
                     labels = c("-9","-8","-7","6","-5","-4","-3","-2","-1","0","+1","+2","+3","+4","+5","+6","+7","+8","+9"))+
  ylim(-30,30)+
  
  scale_color_manual(values=c("#e67e00","#024b7a"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  geom_vline(xintercept=-0.1,lty=2,color="gray59")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  theme_bw()+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  theme_cowplot()

fig2_poster <- ggplot(dists_abort_dynamic.dyn)+
  geom_rect( aes(xmin=0.1,
                 xmax=9,
                 ymin=(dists_abort_dynamic[["overall.att"]]-1.96*dists_abort_dynamic[["overall.se"]]),
                 ymax=(dists_abort_dynamic[["overall.att"]]+1.96*dists_abort_dynamic[["overall.se"]]), alpha="95% CI"), fill = alpha("lightblue"),
             color="lightblue")+
  geom_segment( 
    aes(x = 0, y = dists_abort_dynamic[["overall.att"]], xend = 9, yend = dists_abort_dynamic[["overall.att"]], lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att,2),ymin = round((att-1.96*se),2), ymax = round((att+1.96*se),2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of abortions per 1,000 pregnancies")+
  xlab("Time since received access to RSBY")+
  scale_x_continuous(breaks=c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9),
                     labels = c("-9","-8","-7","6","-5","-4","-3","-2","-1","0","+1","+2","+3","+4","+5","+6","+7","+8","+9"))+
  ylim(-30,30)+
  
  scale_color_manual(values=c("#e67e00","#024b7a"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  geom_vline(xintercept=-0.1,lty=2,color="gray59")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  theme_bw()+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  theme_cowplot(26)

cowplot::save_plot(
  "fig2poster.png",
  plot = fig2_poster,
  base_height = 10,
  base_width = 20
)



dists_ms_dynamic.dyn <- data.frame(time=dists_ms_dynamic[["egt"]],
                                      att=dists_ms_dynamic[["att.egt"]],
                                      se=dists_ms_dynamic[["se.egt"]])

dists_ms_dynamic.dyn  <- dists_ms_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))

ggplot(dists_ms_dynamic.dyn)+
  geom_rect( aes(xmin=0.1,
                 xmax=9,
                 ymin=(dists_ms_dynamic[["overall.att"]]-1.96*dists_ms_dynamic[["overall.se"]]),
                 ymax=(dists_ms_dynamic[["overall.att"]]+1.96*dists_ms_dynamic[["overall.se"]]), alpha="95% CI"), fill = alpha("lightblue"),
             color="lightblue")+
  geom_segment( 
    aes(x = 0, y = dists_ms_dynamic[["overall.att"]], xend = 9, yend = dists_ms_dynamic[["overall.att"]], lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att,2),ymin = round((att-1.96*se),2), ymax = round((att+1.96*se),2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of miscarriages per 1,000 pregnancies")+
  xlab("Time since received access to RSBY")+
  scale_x_continuous(breaks=c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9),
                     labels = c("-9","-8","-7","6","-5","-4","-3","-2","-1","0","+1","+2","+3","+4","+5","+6","+7","+8","+9"))+
  ylim(-30,30)+
  
  scale_color_manual(values=c("#e67e00","#024b7a"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  geom_vline(xintercept=-0.1,lty=2,color="gray59")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  theme_bw()+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  theme_cowplot()


fig3_poster <- ggplot(dists_ms_dynamic.dyn)+
  geom_rect( aes(xmin=0.1,
                 xmax=9,
                 ymin=(dists_ms_dynamic[["overall.att"]]-1.96*dists_ms_dynamic[["overall.se"]]),
                 ymax=(dists_ms_dynamic[["overall.att"]]+1.96*dists_ms_dynamic[["overall.se"]]), alpha="95% CI"), fill = alpha("lightblue"),
             color="lightblue")+
  geom_segment( 
    aes(x = 0, y = dists_ms_dynamic[["overall.att"]], xend = 9, yend = dists_ms_dynamic[["overall.att"]], lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att,2),ymin = round((att-1.96*se),2), ymax = round((att+1.96*se),2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of miscarriages per 1,000 pregnancies")+
  xlab("Time since received access to RSBY")+
  scale_x_continuous(breaks=c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9),
                     labels = c("-9","-8","-7","6","-5","-4","-3","-2","-1","0","+1","+2","+3","+4","+5","+6","+7","+8","+9"))+
  ylim(-30,30)+
  
  scale_color_manual(values=c("#e67e00","#024b7a"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  geom_vline(xintercept=-0.1,lty=2,color="gray59")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  theme_bw()+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  theme_cowplot(26)


cowplot::save_plot(
  "fig3poster.png",
  plot = fig3_poster,
  base_height = 10,
  base_width = 20
)


#group plots
dists_sb_group.plot <- data.frame(time=dists_sb_group[["egt"]],
                                   att=dists_sb_group[["att.egt"]],
                                   se=dists_sb_group[["se.egt"]])

dists_sb_group.plot$time <- factor(dists_sb_group.plot$time, 
                         levels = c(2010, 2012, 2014),
                         labels = c("Early", "Mid", "Late"))


pooled_sb_group_plot <- ggplot(data = dists_sb_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-70, 70), n.breaks = 7)+
  ylab("Difference in rate of stillbirths per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

pooled_sb_group_plot_poster <- ggplot(data = dists_sb_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-70, 70), n.breaks = 7)+
  ylab("Difference in rate of stillbirths per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot(26)
  
dists_abort_group.plot <- data.frame(time=dists_abort_group[["egt"]],
                                  att=dists_abort_group[["att.egt"]],
                                  se=dists_abort_group[["se.egt"]])

dists_abort_group.plot$time <- factor(dists_abort_group.plot$time, 
                                   levels = c(2010, 2012, 2014),
                                   labels = c("Early", "Mid", "Late"))


pooled_abort_group_plot <- ggplot(data = dists_abort_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-70, 70), n.breaks = 7)+
  ylab("Difference in rate of abortions per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

pooled_abort_group_plot_poster <- ggplot(data = dists_abort_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-70, 70), n.breaks = 7)+
  ylab("Difference in rate of abortions per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot(26)
  
dists_ms_group.plot <- data.frame(time=dists_ms_group[["egt"]],
                                     att=dists_ms_group[["att.egt"]],
                                     se=dists_ms_group[["se.egt"]])

dists_ms_group.plot$time <- factor(dists_ms_group.plot$time, 
                                   levels = c(2010, 2012, 2014),
                                   labels = c("Early", "Mid", "Late"))


pooled_ms_group_plot <- ggplot(data = dists_ms_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-70, 70), n.breaks = 7)+
  ylab("Difference in rate of miscarriages per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()


pooled_ms_group_plot_poster <- ggplot(data = dists_ms_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-70, 70), n.breaks = 7)+
  ylab("Difference in rate of miscarriages per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot(26)


pooled_group_plots <- ggarrange(pooled_sb_group_plot, pooled_abort_group_plot, pooled_ms_group_plot,
                                labels = c("A", "B", "C"), nrow = 1)

pooled_group_plots_poster <- ggarrange(pooled_sb_group_plot_poster, pooled_abort_group_plot_poster, pooled_ms_group_plot_poster,
                                labels = c("A", "B", "C"), nrow = 1)

cowplot::save_plot(
  "pooled_groups_plots_poster.png",
  plot = pooled_group_plots_poster,
  base_height = 12,
  base_width = 25
)

#calendar plots
ggdid(dists_sb_calendar)

dists_sb_calendar.plot <- data.frame(time=dists_sb_calendar[["egt"]],
                                  att=dists_sb_calendar[["att.egt"]],
                                  se=dists_sb_calendar[["se.egt"]])

ggplot(data = dists_sb_calendar.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  ylim(-20, 20)+
  ylab("Difference in rate of stillbirths per 1,000 pregnancies")+
  xlab("Year")+
  theme_cowplot()

ggdid(dists_sb_calendar)

dists_abort_calendar.plot <- data.frame(time=dists_abort_calendar[["egt"]],
                                     att=dists_abort_calendar[["att.egt"]],
                                     se=dists_abort_calendar[["se.egt"]])

ggplot(data = dists_abort_calendar.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  ylim(-50, 50)+
  ylab("Difference in rate of abortions per 1,000 pregnancies")+
  xlab("Year")+
  theme_cowplot()

dists_ms_calendar.plot <- data.frame(time=dists_ms_calendar[["egt"]],
                                        att=dists_ms_calendar[["att.egt"]],
                                        se=dists_ms_calendar[["se.egt"]])

ggplot(data = dists_ms_calendar.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  ylim(-50, 50)+
  ylab("Difference in rate of miscarriages per 1,000 pregnancies")+
  xlab("Year")+
  theme_cowplot()

##### primary school stratified  #####

noprim <- read.csv("noprim_rates_weighted.csv")
prim <- read.csv("prim_rates_weighted.csv")


noprim$sb_rate <- (noprim$sb / noprim$pregs)*1000
noprim$abort_rate <- (noprim$abort / noprim$pregs)*1000
noprim$ms_rate <- (noprim$ms / noprim$pregs)*1000

prim$sb_rate <- (prim$sb / prim$pregs)*1000
prim$abort_rate <- (prim$abort / prim$pregs)*1000
prim$ms_rate <- (prim$ms / prim$pregs)*1000



#less_primary_dist <- dists_df %>% filter(prop_primary < 0.5)
#more_primary_dist <- dists_df %>% filter(prop_primary >= 0.5)

noprim_sb <- att_gt(yname = "sb_rate",
                  tname = "outcome_year",
                  gname = "enrollgroup",
                  idname = "dist_id",
                  xformla = ~(med_age^2),
                  data = noprim,
                  panel = FALSE,
                  clustervars = "dist_id",
                  control_group = "notyettreated",
                  print_details = TRUE,
                  bstrap=TRUE, cband=FALSE
)

noprim_abort <- att_gt(yname = "abort_rate",
                     tname = "outcome_year",
                     gname = "enrollgroup",
                     idname = "dist_id",
                     xformla = ~(med_age^2),
                     data = noprim,
                     panel = FALSE,
                     clustervars = "dist_id",
                     control_group = "notyettreated",
                     print_details = TRUE,
                     bstrap=TRUE, cband=FALSE
)

noprim_ms <- att_gt(yname = "ms_rate",
                  tname = "outcome_year",
                  gname = "enrollgroup",
                  idname = "dist_id",
                  xformla = ~(med_age^2),
                  data = noprim,
                  panel = FALSE,
                  clustervars = "dist_id",
                  control_group = "notyettreated",
                  print_details = TRUE,
                  bstrap=TRUE, cband=FALSE
)


noprim_sb_group <- aggte(noprim_sb, type = "group", na.rm = T)
noprim_abort_group <- aggte(noprim_abort, type = "group", na.rm = T)
noprim_ms_group <- aggte(noprim_ms, type = "group", na.rm = T)

noprim_sb_dynamic <- aggte(noprim_sb, type = "dynamic", na.rm = T)
noprim_abort_dynamic <- aggte(noprim_abort, type = "dynamic", na.rm = T)
noprim_ms_dynamic <- aggte(noprim_ms, type = "dynamic", na.rm = T)

noprim_sb_calendar <- aggte(noprim_sb, type = "calendar", na.rm = T)
noprim_abort_calendar <- aggte(noprim_abort, type = "calendar", na.rm = T)
noprim_ms_calendar <- aggte(noprim_ms, type = "calendar", na.rm = T)


prim_sb <- att_gt(yname = "sb_rate",
                               tname = "outcome_year",
                               gname = "enrollgroup",
                               idname = "dist_id",
                               xformla = ~ (med_age^2),
                               data = prim,
                               panel = FALSE,
                               clustervars = "dist_id",
                               control_group = "notyettreated",
                               print_details = TRUE,
                               bstrap=TRUE, cband=FALSE
)

prim_abort <- att_gt(yname = "abort_rate",
                                  tname = "outcome_year",
                                  gname = "enrollgroup",
                                  idname = "dist_id",
                                  xformla = ~ (med_age^2),
                                  data = prim,
                                  panel = FALSE,
                                  clustervars = "dist_id",
                                  control_group = "notyettreated",
                                  print_details = TRUE,
                                  bstrap=TRUE, cband=FALSE
)

prim_ms <- att_gt(yname = "ms_rate",
                               tname = "outcome_year",
                               gname = "enrollgroup",
                               idname = "dist_id",
                               xformla = ~ (med_age^2),
                               data = prim,
                               panel = FALSE,
                               clustervars = "dist_id",
                               control_group = "notyettreated",
                               print_details = TRUE,
                               bstrap=TRUE, cband=FALSE
)


prim_sb_group <- aggte(prim_sb, type = "group", na.rm = T)
prim_abort_group <- aggte(prim_abort, type = "group", na.rm = T)
prim_ms_group <- aggte(prim_ms, type = "group", na.rm = T)

prim_sb_dynamic <- aggte(prim_sb, type = "dynamic", na.rm = T)
prim_abort_dynamic <- aggte(prim_abort, type = "dynamic", na.rm = T)
prim_ms_dynamic <- aggte(prim_ms, type = "dynamic", na.rm = T)

prim_sb_calendar <- aggte(prim_sb, type = "calendar", na.rm = T)
prim_abort_calendar <- aggte(prim_abort, type = "calendar", na.rm = T)
prim_ms_calendar <- aggte(prim_ms, type = "calendar", na.rm = T)


#group plots
noprim_sb_group.plot <- data.frame(time=noprim_sb_group[["egt"]],
                                  att=noprim_sb_group[["att.egt"]],
                                  se=noprim_sb_group[["se.egt"]])

noprim_sb_grouped_plot <- ggplot(data = noprim_sb_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-25, 25))+
  ylab("Difference in rate of stillbirths per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

noprim_abort_group.plot <- data.frame(time=noprim_abort_group[["egt"]],
                                     att=noprim_abort_group[["att.egt"]],
                                     se=noprim_abort_group[["se.egt"]])

noprim_abort_grouped_plot <- ggplot(data = noprim_abort_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-25, 25))+
  ylab("Difference in rate of abortions per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

noprim_ms_group.plot <- data.frame(time=noprim_ms_group[["egt"]],
                                  att=noprim_ms_group[["att.egt"]],
                                  se=noprim_ms_group[["se.egt"]])

noprim_ms_grouped_plot <- ggplot(data = noprim_ms_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-25, 25))+
  ylab("Difference in rate of miscarriages per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

noprim_group_plots <- ggarrange(noprim_sb_grouped_plot, noprim_abort_grouped_plot, noprim_ms_grouped_plot,
                               labels = c("A", "B", "C"), nrow = 1)


#tables
noprim_sb_dynamic.tab <- tidy(noprim_sb_dynamic)
noprim_sb_dynamic.tab <- noprim_sb_dynamic.tab %>% select(-c(type, term, point.conf.low, point.conf.high))
noprim_sb_dynamic.tab <- noprim_sb_dynamic.tab %>% round_half_up(.,1)
noprim_sb_dynamic.tab$Outcome <- c("Stillbirth")

noprim_abort_dynamic.tab <- tidy(noprim_abort_dynamic)
noprim_abort_dynamic.tab <- noprim_abort_dynamic.tab %>% select(-c(type, term, point.conf.low, point.conf.high))
noprim_abort_dynamic.tab <- noprim_abort_dynamic.tab %>% round_half_up(.,1)
noprim_abort_dynamic.tab$Outcome <- c("Abortion")


noprim_ms_dynamic.tab <- tidy(noprim_ms_dynamic)
noprim_ms_dynamic.tab <- noprim_ms_dynamic.tab %>% select(-c(type, term, point.conf.low, point.conf.high))
noprim_ms_dynamic.tab <- noprim_ms_dynamic.tab %>% round_half_up(.,1)
noprim_ms_dynamic.tab$Outcome <- c("Miscarriage")

noprim_dynamic <- rbind(noprim_sb_dynamic.tab, noprim_abort_dynamic.tab, noprim_ms_dynamic.tab)
noprim_dynamic_flextable <- flextable(noprim_dynamic)
save_as_docx(noprim_dynamic_flextable, path = "noprim_dynamic_tab.docx")

#group plots
prim_sb_group.plot <- data.frame(time=prim_sb_group[["egt"]],
                                   att=prim_sb_group[["att.egt"]],
                                   se=prim_sb_group[["se.egt"]])

prim_sb_grouped_plot <- ggplot(data = prim_sb_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-35, 35))+
  ylab("Difference in rate of stillbirths per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

prim_abort_group.plot <- data.frame(time=prim_abort_group[["egt"]],
                                      att=prim_abort_group[["att.egt"]],
                                      se=prim_abort_group[["se.egt"]])

prim_abort_grouped_plot <- ggplot(data = prim_abort_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-35, 35))+
  ylab("Difference in rate of abortions per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

prim_ms_group.plot <- data.frame(time=prim_ms_group[["egt"]],
                                   att=prim_ms_group[["att.egt"]],
                                   se=prim_ms_group[["se.egt"]])

prim_ms_grouped_plot <- ggplot(data = prim_ms_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-35, 35))+
  ylab("Difference in rate of miscarriages per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

prim_group_plots <- ggarrange(prim_sb_grouped_plot, prim_abort_grouped_plot, prim_ms_grouped_plot,
                                labels = c("A", "B", "C"), nrow = 1)


#tables
prim_sb_dynamic.tab <- tidy(prim_sb_dynamic)
prim_sb_dynamic.tab <- prim_sb_dynamic.tab %>% select(-c(type, term, point.conf.low, point.conf.high))
prim_sb_dynamic.tab <- prim_sb_dynamic.tab %>% round_half_up(.,1)
prim_sb_dynamic.tab$Outcome <- c("Stillbirth")

prim_abort_dynamic.tab <- tidy(prim_abort_dynamic)
prim_abort_dynamic.tab <- prim_abort_dynamic.tab %>% select(-c(type, term, point.conf.low, point.conf.high))
prim_abort_dynamic.tab <- prim_abort_dynamic.tab %>% round_half_up(.,1)
prim_abort_dynamic.tab$Outcome <- c("Abortion")


prim_ms_dynamic.tab <- tidy(prim_ms_dynamic)
prim_ms_dynamic.tab <- prim_ms_dynamic.tab %>% select(-c(type, term, point.conf.low, point.conf.high))
prim_ms_dynamic.tab <- prim_ms_dynamic.tab %>% round_half_up(.,1)
prim_ms_dynamic.tab$Outcome <- c("Miscarriage")

prim_dynamic <- rbind(prim_sb_dynamic.tab, prim_abort_dynamic.tab, prim_ms_dynamic.tab)
prim_dynamic_flextable <- flextable(prim_dynamic)
save_as_docx(prim_dynamic_flextable, path = "prim_dynamic_tab.docx")



#### Rural Urban Stratified ####

rural <- read.csv("rural_rates_weighted.csv")
names(rural)
rural <- rural %>% select(-c(X))

urban <- read.csv("urban_rates_weighted.csv")


rural$sb_rate <- (rural$sb / rural$pregs)*1000
rural$abort_rate <- (rural$abort / rural$pregs)*1000
rural$ms_rate <- (rural$ms / rural$pregs)*1000

rural_sb <- att_gt(yname = "sb_rate",
                  tname = "outcome_year",
                  gname = "enrollgroup",
                  idname = "dist_id",
                  xformla = ~ (med_age^2),
                  data = rural,
                  panel = TRUE,
                  clustervars = "dist_id",
                  control_group = "notyettreated",
                  print_details = TRUE,
                  bstrap=TRUE, cband=FALSE
)

rural_abort <- att_gt(yname = "abort_rate",
                     tname = "outcome_year",
                     gname = "enrollgroup",
                     idname = "dist_id",
                     xformla = ~ (med_age^2),
                     data = rural,
                     panel = TRUE,
                     clustervars = "dist_id",
                     control_group = "notyettreated",
                     print_details = TRUE,
                     bstrap=TRUE, cband=FALSE
)

rural_ms <- att_gt(yname = "ms_rate",
                  tname = "outcome_year",
                  gname = "enrollgroup",
                  idname = "dist_id",
                  xformla = ~ (med_age^2),
                  data = rural,
                  panel = TRUE,
                  clustervars = "dist_id",
                  control_group = "notyettreated",
                  print_details = TRUE,
                  bstrap=TRUE, cband=FALSE
)

rural_sb_group <- aggte(rural_sb, type = "group")
rural_sb_dynamic <- aggte(rural_sb, type = "dynamic")
rural_sb_calendar <- aggte(rural_sb, type = "calendar")

rural_abort_group <- aggte(rural_abort, type = "group")
rural_abort_dynamic <- aggte(rural_abort, type = "dynamic")
rural_abort_calendar <- aggte(rural_abort, type = "calendar")

rural_ms_group <- aggte(rural_ms, type = "group")
rural_ms_dynamic <- aggte(rural_ms, type = "dynamic")
rural_ms_calendar <- aggte(rural_ms, type = "calendar")

rural_sb_dynamic.dyn <- data.frame(time=rural_sb_dynamic[["egt"]],
                                   att=rural_sb_dynamic[["att.egt"]],
                                   se=rural_sb_dynamic[["se.egt"]])

rural_sb_dynamic.dyn  <- rural_sb_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))

ggplot(rural_sb_dynamic.dyn)+
  geom_rect( aes(xmin=0,
                 xmax=9,
                 ymin=(rural_sb_dynamic[["overall.att"]]-1.96*rural_sb_dynamic[["overall.se"]]),
                 ymax=(rural_sb_dynamic[["overall.att"]]+1.96*rural_sb_dynamic[["overall.se"]]), alpha="95% CI"), fill = alpha("lightblue"),
             color="lightblue")+
  geom_segment( 
    aes(x = 0, y = rural_sb_dynamic[["overall.att"]], xend = 9, yend = rural_sb_dynamic[["overall.att"]], lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att,2),ymin = round((att-1.96*se),2), ymax = round((att+1.96*se),2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of stillbirths per 1,000 pregnancies")+
  xlab("Time since received access to RSBY")+
  scale_x_continuous(breaks=c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9),
                     labels = c("-9","-8","-7","6","-5","-4","-3","-2","-1","0","+1","+2","+3","+4","+5","+6","+7","+8","+9"))+
  scale_y_continuous(limits = c(-20,20), n.breaks = 7)+
  
  scale_color_manual(values=c("#e67e00","#024b7a"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  geom_vline(xintercept=-0.1,lty=2,color="gray59")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  theme_bw()+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  theme_cowplot()


rural_abort_dynamic.dyn <- data.frame(time=rural_abort_dynamic[["egt"]],
                                      att=rural_abort_dynamic[["att.egt"]],
                                      se=rural_abort_dynamic[["se.egt"]])

rural_abort_dynamic.dyn  <- rural_abort_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))

ggplot(rural_abort_dynamic.dyn)+
  geom_rect( aes(xmin=0,
                 xmax=9,
                 ymin=(rural_abort_dynamic[["overall.att"]]-1.96*rural_abort_dynamic[["overall.se"]]),
                 ymax=(rural_abort_dynamic[["overall.att"]]+1.96*rural_abort_dynamic[["overall.se"]]), alpha="95% CI"), fill = alpha("lightblue"),
             color="lightblue")+
  geom_segment( 
    aes(x = 0, y = rural_abort_dynamic[["overall.att"]], xend = 9, yend = rural_abort_dynamic[["overall.att"]], lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att,2),ymin = round((att-1.96*se),2), ymax = round((att+1.96*se),2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of abortions per 1,000 pregnancies")+
  xlab("Time since received access to RSBY")+
  scale_x_continuous(breaks=c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9),
                     labels = c("-9","-8","-7","6","-5","-4","-3","-2","-1","0","+1","+2","+3","+4","+5","+6","+7","+8","+9"))+
  scale_y_continuous(limits = c(-30, 30), n.breaks = 7)+
  
  scale_color_manual(values=c("#e67e00","#024b7a"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  geom_vline(xintercept=-0.1,lty=2,color="gray59")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  theme_bw()+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  theme_cowplot()

rural_ms_dynamic.dyn <- data.frame(time=rural_ms_dynamic[["egt"]],
                                   att=rural_ms_dynamic[["att.egt"]],
                                   se=rural_ms_dynamic[["se.egt"]])

rural_ms_dynamic.dyn  <- rural_ms_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))

ggplot(rural_ms_dynamic.dyn)+
  geom_rect( aes(xmin=0.1,
                 xmax=9,
                 ymin=(rural_ms_dynamic[["overall.att"]]-1.96*rural_ms_dynamic[["overall.se"]]),
                 ymax=(rural_ms_dynamic[["overall.att"]]+1.96*rural_ms_dynamic[["overall.se"]]), alpha="95% CI"), fill = alpha("lightblue"),
             color="lightblue")+
  geom_segment( 
    aes(x = 0, y = rural_ms_dynamic[["overall.att"]], xend = 9, yend = rural_ms_dynamic[["overall.att"]], lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att,2),ymin = round((att-1.96*se),2), ymax = round((att+1.96*se),2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Rural difference in rate of miscarriages per 1,000 pregnancies")+
  xlab("Time since received access to RSBY")+
  scale_x_continuous(breaks=c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9),
                     labels = c("-9","-8","-7","6","-5","-4","-3","-2","-1","0","+1","+2","+3","+4","+5","+6","+7","+8","+9"))+
  ylim(-50,50)+
  
  scale_color_manual(values=c("#e67e00","#024b7a"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  geom_vline(xintercept=-0.1,lty=2,color="gray59")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  theme_bw()+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  theme_cowplot()


#group plots
rural_sb_group.plot <- data.frame(time=rural_sb_group[["egt"]],
                                  att=rural_sb_group[["att.egt"]],
                                  se=rural_sb_group[["se.egt"]])

rural_sb_grouped_plot <- ggplot(data = rural_sb_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-80, 80), n.breaks = 7)+
  ylab("Difference in rate of stillbirths per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

rural_sb_grouped_plot_poster <- ggplot(data = rural_sb_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-80, 80), n.breaks = 7)+
  ylab("Difference in rate of stillbirths per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot(26)




rural_abort_group.plot <- data.frame(time=rural_abort_group[["egt"]],
                                     att=rural_abort_group[["att.egt"]],
                                     se=rural_abort_group[["se.egt"]])

rural_abort_grouped_plot <- ggplot(data = rural_abort_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-80, 80), n.breaks = 7)+
  ylab("Difference in rate of abortions per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

rural_abort_grouped_plot_poster <- ggplot(data = rural_abort_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-80, 80), n.breaks = 7)+
  ylab("Difference in rate of abortions per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot(26)

rural_ms_group.plot <- data.frame(time=rural_ms_group[["egt"]],
                                  att=rural_ms_group[["att.egt"]],
                                  se=rural_ms_group[["se.egt"]])

rural_ms_grouped_plot <- ggplot(data = rural_ms_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-80, 80), n.breaks = 7)+
  ylab("Difference in rate of miscarriages per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

rural_ms_grouped_plot_poster <- ggplot(data = rural_ms_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-80, 80), n.breaks = 7)+
  ylab("Difference in rate of miscarriages per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot(26)

rural_group_plots <- ggarrange(rural_sb_grouped_plot, rural_abort_grouped_plot, rural_ms_grouped_plot,
                                labels = c("A", "B", "C"), nrow = 1)


rural_group_plots_poster <- ggarrange(rural_sb_grouped_plot_poster, rural_abort_grouped_plot_poster, rural_ms_grouped_plot_poster,
                               labels = c("A", "B", "C"), nrow = 1)


cowplot::save_plot(
  "rural_group_plots_poster.png",
  plot = rural_group_plots_poster,
  base_height = 12,
  base_width = 25
)

#calendar plots
ggdid(rural_sb_calendar)

rural_sb_calendar.plot <- data.frame(time=rural_sb_calendar[["egt"]],
                                     att=rural_sb_calendar[["att.egt"]],
                                     se=rural_sb_calendar[["se.egt"]])

ggplot(data = rural_sb_calendar.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  ylim(-20, 20)+
  ylab("Difference in rate of stillbirths per 1,000 pregnancies")+
  xlab("Year")+
  theme_cowplot()


rural_abort_calendar.plot <- data.frame(time=rural_abort_calendar[["egt"]],
                                        att=rural_abort_calendar[["att.egt"]],
                                        se=rural_abort_calendar[["se.egt"]])

ggplot(data = rural_abort_calendar.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  ylim(-50, 50)+
  ylab("Difference in rate of abortions per 1,000 pregnancies")+
  xlab("Year")+
  theme_cowplot()

rural_ms_calendar.plot <- data.frame(time=rural_ms_calendar[["egt"]],
                                     att=rural_ms_calendar[["att.egt"]],
                                     se=rural_ms_calendar[["se.egt"]])

ggplot(data = rural_ms_calendar.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  ylim(-50, 50)+
  ylab("Difference in rate of miscarriages per 1,000 pregnancies")+
  xlab("Year")+
  theme_cowplot()

#tables
rural_sb_dynamic.tab <- tidy(rural_sb_dynamic)
rural_sb_dynamic.tab <- rural_sb_dynamic.tab %>% select(-c(type, term, point.conf.low, point.conf.high))
rural_sb_dynamic.tab <- rural_sb_dynamic.tab %>% round_half_up(.,1)
rural_sb_dynamic.tab$Outcome <- c("Stillbirth")

rural_abort_dynamic.tab <- tidy(rural_abort_dynamic)
rural_abort_dynamic.tab <- rural_abort_dynamic.tab %>% select(-c(type, term, point.conf.low, point.conf.high))
rural_abort_dynamic.tab <- rural_abort_dynamic.tab %>% round_half_up(.,1)
rural_abort_dynamic.tab$Outcome <- c("Abortion")


rural_ms_dynamic.tab <- tidy(rural_ms_dynamic)
rural_ms_dynamic.tab <- rural_ms_dynamic.tab %>% select(-c(type, term, point.conf.low, point.conf.high))
rural_ms_dynamic.tab <- rural_ms_dynamic.tab %>% round_half_up(.,1)
rural_ms_dynamic.tab$Outcome <- c("Miscarriage")

rural_dynamic <- rbind(rural_sb_dynamic.tab, rural_abort_dynamic.tab, rural_ms_dynamic.tab)
rural_dynamic_flextable <- flextable(rural_dynamic)
save_as_docx(rural_dynamic_flextable, path = "rural_dynamic_tab.docx")



urban$sb_rate <- (urban$sb / urban$pregs)*1000
urban$abort_rate <- (urban$abort / urban$pregs)*1000
urban$ms_rate <- (urban$ms / urban$pregs)*1000

urban_sb <- att_gt(yname = "sb_rate",
                   tname = "outcome_year",
                   gname = "enrollgroup",
                   idname = "dist_id",
                   #xformla = ~ (med_age^2),
                   data = urban,
                   panel = TRUE,
                   clustervars = "dist_id",
                   control_group = "notyettreated",
                   print_details = TRUE,
                   bstrap=TRUE, cband=FALSE
)

urban_abort <- att_gt(yname = "abort_rate",
                      tname = "outcome_year",
                      gname = "enrollgroup",
                      idname = "dist_id",
                      #xformla = ~ (med_age^2),
                      data = urban,
                      panel = TRUE,
                      clustervars = "dist_id",
                      control_group = "notyettreated",
                      print_details = TRUE,
                      bstrap=TRUE, cband=FALSE
)

urban_ms <- att_gt(yname = "ms_rate",
                   tname = "outcome_year",
                   gname = "enrollgroup",
                   idname = "dist_id",
                   #xformla = ~ (med_age^2),
                   data = urban,
                   panel = TRUE,
                   clustervars = "dist_id",
                   control_group = "notyettreated",
                   print_details = TRUE,
                   bstrap=TRUE, cband=FALSE
)

urban_sb_group <- aggte(urban_sb, type = "group")
urban_sb_dynamic <- aggte(urban_sb, type = "dynamic")
urban_sb_calendar <- aggte(urban_sb, type = "calendar")

urban_abort_group <- aggte(urban_abort, type = "group")
urban_abort_dynamic <- aggte(urban_abort, type = "dynamic")
urban_abort_calendar <- aggte(urban_abort, type = "calendar")

urban_ms_group <- aggte(urban_ms, type = "group")
urban_ms_dynamic <- aggte(urban_ms, type = "dynamic")
urban_ms_calendar <- aggte(urban_ms, type = "calendar")

urban_sb_dynamic.dyn <- data.frame(time=urban_sb_dynamic[["egt"]],
                                   att=urban_sb_dynamic[["att.egt"]],
                                   se=urban_sb_dynamic[["se.egt"]])

urban_sb_dynamic.dyn  <- urban_sb_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))

ggplot(urban_sb_dynamic.dyn)+
  geom_rect( aes(xmin=0.1,
                 xmax=9,
                 ymin=(urban_sb_dynamic[["overall.att"]]-1.96*urban_sb_dynamic[["overall.se"]]),
                 ymax=(urban_sb_dynamic[["overall.att"]]+1.96*urban_sb_dynamic[["overall.se"]]), alpha="95% CI"), fill = alpha("lightblue"),
             color="lightblue")+
  geom_segment( 
    aes(x = 0, y = urban_sb_dynamic[["overall.att"]], xend = 9, yend = urban_sb_dynamic[["overall.att"]], lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att,2),ymin = round((att-1.96*se),2), ymax = round((att+1.96*se),2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of stillbirths per 1,000 pregnancies")+
  xlab("Time since received access to RSBY")+
  scale_x_continuous(breaks=c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9),
                     labels = c("-9","-8","-7","6","-5","-4","-3","-2","-1","0","+1","+2","+3","+4","+5","+6","+7","+8","+9"))+
  ylim(-15,15)+
  
  scale_color_manual(values=c("#e67e00","#024b7a"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  geom_vline(xintercept=-0.1,lty=2,color="gray59")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  theme_bw()+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  theme_cowplot()


urban_abort_dynamic.dyn <- data.frame(time=urban_abort_dynamic[["egt"]],
                                      att=urban_abort_dynamic[["att.egt"]],
                                      se=urban_abort_dynamic[["se.egt"]])

urban_abort_dynamic.dyn  <- urban_abort_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))

ggplot(urban_abort_dynamic.dyn)+
  geom_rect( aes(xmin=0.1,
                 xmax=9,
                 ymin=(urban_abort_dynamic[["overall.att"]]-1.96*urban_abort_dynamic[["overall.se"]]),
                 ymax=(urban_abort_dynamic[["overall.att"]]+1.96*urban_abort_dynamic[["overall.se"]]), alpha="95% CI"), fill = alpha("lightblue"),
             color="lightblue")+
  geom_segment( 
    aes(x = 0, y = urban_abort_dynamic[["overall.att"]], xend = 9, yend = urban_abort_dynamic[["overall.att"]], lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att,2),ymin = round((att-1.96*se),2), ymax = round((att+1.96*se),2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of abortions per 1,000 pregnancies")+
  xlab("Time since received access to RSBY")+
  scale_x_continuous(breaks=c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9),
                     labels = c("-9","-8","-7","6","-5","-4","-3","-2","-1","0","+1","+2","+3","+4","+5","+6","+7","+8","+9"))+
  ylim(-30,30)+
  
  scale_color_manual(values=c("#e67e00","#024b7a"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  geom_vline(xintercept=-0.1,lty=2,color="gray59")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  theme_bw()+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  theme_cowplot()

urban_ms_dynamic.dyn <- data.frame(time=urban_ms_dynamic[["egt"]],
                                   att=urban_ms_dynamic[["att.egt"]],
                                   se=urban_ms_dynamic[["se.egt"]])

urban_ms_dynamic.dyn  <- urban_ms_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))

ggplot(urban_ms_dynamic.dyn)+
  geom_rect( aes(xmin=0.1,
                 xmax=9,
                 ymin=(urban_ms_dynamic[["overall.att"]]-1.96*urban_ms_dynamic[["overall.se"]]),
                 ymax=(urban_ms_dynamic[["overall.att"]]+1.96*urban_ms_dynamic[["overall.se"]]), alpha="95% CI"), fill = alpha("lightblue"),
             color="lightblue")+
  geom_segment( 
    aes(x = 0, y = urban_ms_dynamic[["overall.att"]], xend = 9, yend = urban_ms_dynamic[["overall.att"]], lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att,2),ymin = round((att-1.96*se),2), ymax = round((att+1.96*se),2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of miscarriages per 1,000 pregnancies")+
  xlab("Time since received access to RSBY")+
  scale_x_continuous(breaks=c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9),
                     labels = c("-9","-8","-7","6","-5","-4","-3","-2","-1","0","+1","+2","+3","+4","+5","+6","+7","+8","+9"))+
  ylim(-30,30)+
  
  scale_color_manual(values=c("#e67e00","#024b7a"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  geom_vline(xintercept=-0.1,lty=2,color="gray59")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  theme_bw()+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  theme_cowplot()


#group plots
urban_sb_group.plot <- data.frame(time=urban_sb_group[["egt"]],
                                  att=urban_sb_group[["att.egt"]],
                                  se=urban_sb_group[["se.egt"]])

urban_sb_grouped_plot <- ggplot(data = urban_sb_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-80, 80), n.breaks = 7)+
  ylab("Difference in rate of stillbirths per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

urban_abort_group.plot <- data.frame(time=urban_abort_group[["egt"]],
                                     att=urban_abort_group[["att.egt"]],
                                     se=urban_abort_group[["se.egt"]])

urban_abort_grouped_plot <- ggplot(data = urban_abort_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-80, 80), n.breaks = 7)+
  ylab("Difference in rate of abortions per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

urban_ms_group.plot <- data.frame(time=urban_ms_group[["egt"]],
                                  att=urban_ms_group[["att.egt"]],
                                  se=urban_ms_group[["se.egt"]])

urban_ms_grouped_plot <- ggplot(data = urban_ms_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-80, 80), n.breaks = 7)+
  ylab("Difference in rate of miscarriages per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

urban_group_plots <- ggarrange(urban_sb_grouped_plot, urban_abort_grouped_plot, urban_ms_grouped_plot,
                               labels = c("A", "B", "C"), nrow = 1)


#tables
urban_sb_dynamic.tab <- tidy(urban_sb_dynamic)
urban_sb_dynamic.tab <- urban_sb_dynamic.tab %>% select(-c(type, term, point.conf.low, point.conf.high))
urban_sb_dynamic.tab <- urban_sb_dynamic.tab %>% round_half_up(.,1)
urban_sb_dynamic.tab$Outcome <- c("Stillbirth")

urban_abort_dynamic.tab <- tidy(urban_abort_dynamic)
urban_abort_dynamic.tab <- urban_abort_dynamic.tab %>% select(-c(type, term, point.conf.low, point.conf.high))
urban_abort_dynamic.tab <- urban_abort_dynamic.tab %>% round_half_up(.,1)
urban_abort_dynamic.tab$Outcome <- c("Abortion")


urban_ms_dynamic.tab <- tidy(urban_ms_dynamic)
urban_ms_dynamic.tab <- urban_ms_dynamic.tab %>% select(-c(type, term, point.conf.low, point.conf.high))
urban_ms_dynamic.tab <- urban_ms_dynamic.tab %>% round_half_up(.,1)
urban_ms_dynamic.tab$Outcome <- c("Miscarriage")

urban_dynamic <- rbind(urban_sb_dynamic.tab, urban_abort_dynamic.tab, urban_ms_dynamic.tab)
urban_dynamic_flextable <- flextable(urban_dynamic)
save_as_docx(urban_dynamic_flextable, path = "urban_dynamic_tab.docx")


#### wealth stratification ####

above <- read.csv("above_rates_weighted.csv")
below <- read.csv("below_rates_weighted.csv")

names(below)
below <- below %>% select(-c(X))

below$sb_rate <- (below$sb / below$pregs)*1000
below$abort_rate <- (below$abort / below$pregs)*1000
below$ms_rate <- (below$ms / below$pregs)*1000

below_sb <- att_gt(yname = "sb_rate",
                   tname = "outcome_year",
                   gname = "enrollgroup",
                   idname = "dist_id",
                   #xformla = ~ (med_age^2),
                   data = below,
                   panel = TRUE,
                   clustervars = "dist_id",
                   control_group = "notyettreated",
                   print_details = TRUE,
                   bstrap=TRUE, cband=FALSE
)

below_abort <- att_gt(yname = "abort_rate",
                      tname = "outcome_year",
                      gname = "enrollgroup",
                      idname = "dist_id",
                      #xformla = ~ (med_age^2),
                      data = below,
                      panel = TRUE,
                      clustervars = "dist_id",
                      control_group = "notyettreated",
                      print_details = TRUE,
                      bstrap=TRUE, cband=FALSE
)

below_ms <- att_gt(yname = "ms_rate",
                   tname = "outcome_year",
                   gname = "enrollgroup",
                   idname = "dist_id",
                   #xformla = ~ (med_age^2),
                   data = below,
                   panel = TRUE,
                   clustervars = "dist_id",
                   control_group = "notyettreated",
                   print_details = TRUE,
                   bstrap=TRUE, cband=FALSE
)

below_sb_group <- aggte(below_sb, type = "group")
below_sb_dynamic <- aggte(below_sb, type = "dynamic")
below_sb_calendar <- aggte(below_sb, type = "calendar")

below_abort_group <- aggte(below_abort, type = "group")
below_abort_dynamic <- aggte(below_abort, type = "dynamic")
below_abort_calendar <- aggte(below_abort, type = "calendar")

below_ms_group <- aggte(below_ms, type = "group")
below_ms_dynamic <- aggte(below_ms, type = "dynamic")
below_ms_calendar <- aggte(below_ms, type = "calendar")

below_sb_dynamic.dyn <- data.frame(time=below_sb_dynamic[["egt"]],
                                   att=below_sb_dynamic[["att.egt"]],
                                   se=below_sb_dynamic[["se.egt"]])

below_sb_dynamic.dyn  <- below_sb_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))

ggplot(below_sb_dynamic.dyn)+
  geom_rect( aes(xmin=0.1,
                 xmax=9,
                 ymin=(below_sb_dynamic[["overall.att"]]-1.96*below_sb_dynamic[["overall.se"]]),
                 ymax=(below_sb_dynamic[["overall.att"]]+1.96*below_sb_dynamic[["overall.se"]]), alpha="95% CI"), fill = alpha("lightblue"),
             color="lightblue")+
  geom_segment( 
    aes(x = 0, y = below_sb_dynamic[["overall.att"]], xend = 9, yend = below_sb_dynamic[["overall.att"]], lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att,2),ymin = round((att-1.96*se),2), ymax = round((att+1.96*se),2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of stillbirths per 1,000 pregnancies")+
  xlab("Time since received access to RSBY")+
  scale_x_continuous(breaks=c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9),
                     labels = c("-9","-8","-7","6","-5","-4","-3","-2","-1","0","+1","+2","+3","+4","+5","+6","+7","+8","+9"))+
  ylim(-30,30)+
  
  scale_color_manual(values=c("#e67e00","#024b7a"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  geom_vline(xintercept=-0.1,lty=2,color="gray59")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  theme_bw()+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  theme_cowplot()


below_abort_dynamic.dyn <- data.frame(time=below_abort_dynamic[["egt"]],
                                      att=below_abort_dynamic[["att.egt"]],
                                      se=below_abort_dynamic[["se.egt"]])

below_abort_dynamic.dyn  <- below_abort_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))

ggplot(below_abort_dynamic.dyn)+
  geom_rect( aes(xmin=0.1,
                 xmax=9,
                 ymin=(below_abort_dynamic[["overall.att"]]-1.96*below_abort_dynamic[["overall.se"]]),
                 ymax=(below_abort_dynamic[["overall.att"]]+1.96*below_abort_dynamic[["overall.se"]]), alpha="95% CI"), fill = alpha("lightblue"),
             color="lightblue")+
  geom_segment( 
    aes(x = 0, y = below_abort_dynamic[["overall.att"]], xend = 9, yend = below_abort_dynamic[["overall.att"]], lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att,2),ymin = round((att-1.96*se),2), ymax = round((att+1.96*se),2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of abortions per 1,000 pregnancies")+
  xlab("Time since received access to RSBY")+
  scale_x_continuous(breaks=c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9),
                     labels = c("-9","-8","-7","6","-5","-4","-3","-2","-1","0","+1","+2","+3","+4","+5","+6","+7","+8","+9"))+
  ylim(-50,50)+
  
  scale_color_manual(values=c("#e67e00","#024b7a"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  geom_vline(xintercept=-0.1,lty=2,color="gray59")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  theme_bw()+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  theme_cowplot()

below_ms_dynamic.dyn <- data.frame(time=below_ms_dynamic[["egt"]],
                                   att=below_ms_dynamic[["att.egt"]],
                                   se=below_ms_dynamic[["se.egt"]])

below_ms_dynamic.dyn  <- below_ms_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))

ggplot(below_ms_dynamic.dyn)+
  geom_rect( aes(xmin=0.1,
                 xmax=9,
                 ymin=(below_ms_dynamic[["overall.att"]]-1.96*below_ms_dynamic[["overall.se"]]),
                 ymax=(below_ms_dynamic[["overall.att"]]+1.96*below_ms_dynamic[["overall.se"]]), alpha="95% CI"), fill = alpha("lightblue"),
             color="lightblue")+
  geom_segment( 
    aes(x = 0, y = below_ms_dynamic[["overall.att"]], xend = 9, yend = below_ms_dynamic[["overall.att"]], lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att,2),ymin = round((att-1.96*se),2), ymax = round((att+1.96*se),2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of miscarriages per 1,000 pregnancies")+
  xlab("Time since received access to RSBY")+
  scale_x_continuous(breaks=c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9),
                     labels = c("-9","-8","-7","6","-5","-4","-3","-2","-1","0","+1","+2","+3","+4","+5","+6","+7","+8","+9"))+
  ylim(-50,50)+
  
  scale_color_manual(values=c("#e67e00","#024b7a"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  geom_vline(xintercept=-0.1,lty=2,color="gray59")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  theme_bw()+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  theme_cowplot()

#group plots
below_sb_group.plot <- data.frame(time=below_sb_group[["egt"]],
                                  att=below_sb_group[["att.egt"]],
                                  se=below_sb_group[["se.egt"]])

ggplot(data = below_sb_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  ylim(-10, 10)+
  ylab("Difference in rate of stillbirths per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

below_abort_group.plot <- data.frame(time=below_abort_group[["egt"]],
                                     att=below_abort_group[["att.egt"]],
                                     se=below_abort_group[["se.egt"]])

ggplot(data = below_abort_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  ylim(-80, 50)+
  ylab("Difference in rate of abortions per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

below_ms_group.plot <- data.frame(time=below_ms_group[["egt"]],
                                  att=below_ms_group[["att.egt"]],
                                  se=below_ms_group[["se.egt"]])

ggplot(data = below_ms_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-80, 80), n.breaks = 7)+
  ylab("Difference in rate of miscarriages per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()



#calendar plots
ggdid(below_sb_calendar)

below_sb_calendar.plot <- data.frame(time=below_sb_calendar[["egt"]],
                                     att=below_sb_calendar[["att.egt"]],
                                     se=below_sb_calendar[["se.egt"]])

ggplot(data = below_sb_calendar.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  ylim(-20, 20)+
  ylab("Difference in rate of stillbirths per 1,000 pregnancies")+
  xlab("Year")+
  theme_cowplot()


below_abort_calendar.plot <- data.frame(time=below_abort_calendar[["egt"]],
                                        att=below_abort_calendar[["att.egt"]],
                                        se=below_abort_calendar[["se.egt"]])

ggplot(data = below_abort_calendar.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  ylim(-50, 50)+
  ylab("Difference in rate of abortions per 1,000 pregnancies")+
  xlab("Year")+
  theme_cowplot()

below_ms_calendar.plot <- data.frame(time=below_ms_calendar[["egt"]],
                                     att=below_ms_calendar[["att.egt"]],
                                     se=below_ms_calendar[["se.egt"]])

ggplot(data = below_ms_calendar.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  ylim(-50, 50)+
  ylab("Difference in rate of miscarriages per 1,000 pregnancies")+
  xlab("Year")+
  theme_cowplot()


#group plots
below_sb_group.plot <- data.frame(time=below_sb_group[["egt"]],
                                  att=below_sb_group[["att.egt"]],
                                  se=below_sb_group[["se.egt"]])

below_sb_grouped_plot <- ggplot(data = below_sb_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-80, 80), n.breaks = 7)+
  ylab("Difference in rate of stillbirths per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

below_abort_group.plot <- data.frame(time=below_abort_group[["egt"]],
                                     att=below_abort_group[["att.egt"]],
                                     se=below_abort_group[["se.egt"]])

below_abort_grouped_plot <- ggplot(data = below_abort_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-80, 80), n.breaks = 7)+
  ylab("Difference in rate of abortions per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

below_ms_group.plot <- data.frame(time=below_ms_group[["egt"]],
                                  att=below_ms_group[["att.egt"]],
                                  se=below_ms_group[["se.egt"]])

below_ms_grouped_plot <- ggplot(data = below_ms_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-80, 80), n.breaks = 7)+
  ylab("Difference in rate of miscarriages per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

below_group_plots <- ggarrange(below_sb_grouped_plot, below_abort_grouped_plot, below_ms_grouped_plot,
                               labels = c("A", "B", "C"), nrow = 1)


#tables
below_sb_dynamic.tab <- tidy(below_sb_dynamic)
below_sb_dynamic.tab <- below_sb_dynamic.tab %>% select(-c(type, term, point.conf.low, point.conf.high))
below_sb_dynamic.tab <- below_sb_dynamic.tab %>% round_half_up(.,1)
below_sb_dynamic.tab$Outcome <- c("Stillbirth")

below_abort_dynamic.tab <- tidy(below_abort_dynamic)
below_abort_dynamic.tab <- below_abort_dynamic.tab %>% select(-c(type, term, point.conf.low, point.conf.high))
below_abort_dynamic.tab <- below_abort_dynamic.tab %>% round_half_up(.,1)
below_abort_dynamic.tab$Outcome <- c("Abortion")


below_ms_dynamic.tab <- tidy(below_ms_dynamic)
below_ms_dynamic.tab <- below_ms_dynamic.tab %>% select(-c(type, term, point.conf.low, point.conf.high))
below_ms_dynamic.tab <- below_ms_dynamic.tab %>% round_half_up(.,1)
below_ms_dynamic.tab$Outcome <- c("Miscarriage")

below_dynamic <- rbind(below_sb_dynamic.tab, below_abort_dynamic.tab, below_ms_dynamic.tab)
below_dynamic_flextable <- flextable(below_dynamic)
save_as_docx(below_dynamic_flextable, path = "below_dynamic_tab.docx")




names(above)
above <- above %>% select(-c(X))

above$sb_rate <- (above$sb / above$pregs)*1000
above$abort_rate <- (above$abort / above$pregs)*1000
above$ms_rate <- (above$ms / above$pregs)*1000

above_sb <- att_gt(yname = "sb_rate",
                   tname = "outcome_year",
                   gname = "enrollgroup",
                   idname = "dist_id",
                   #xformla = ~ (med_age^2),
                   data = above,
                   panel = TRUE,
                   clustervars = "dist_id",
                   control_group = "notyettreated",
                   print_details = TRUE,
                   bstrap=TRUE, cband=FALSE
)

above_abort <- att_gt(yname = "abort_rate",
                      tname = "outcome_year",
                      gname = "enrollgroup",
                      idname = "dist_id",
                      #xformla = ~ (med_age^2),
                      data = above,
                      panel = TRUE,
                      clustervars = "dist_id",
                      control_group = "notyettreated",
                      print_details = TRUE,
                      bstrap=TRUE, cband=FALSE
)

above_ms <- att_gt(yname = "ms_rate",
                   tname = "outcome_year",
                   gname = "enrollgroup",
                   idname = "dist_id",
                   #xformla = ~ (med_age^2),
                   data = above,
                   panel = TRUE,
                   clustervars = "dist_id",
                   control_group = "notyettreated",
                   print_details = TRUE,
                   bstrap=TRUE, cband=FALSE
)

above_sb_group <- aggte(above_sb, type = "group")
above_sb_dynamic <- aggte(above_sb, type = "dynamic")
above_sb_calendar <- aggte(above_sb, type = "calendar")

above_abort_group <- aggte(above_abort, type = "group")
above_abort_dynamic <- aggte(above_abort, type = "dynamic")
above_abort_calendar <- aggte(above_abort, type = "calendar")

above_ms_group <- aggte(above_ms, type = "group")
above_ms_dynamic <- aggte(above_ms, type = "dynamic")
above_ms_calendar <- aggte(above_ms, type = "calendar")

#group plots
above_sb_group.plot <- data.frame(time=above_sb_group[["egt"]],
                                  att=above_sb_group[["att.egt"]],
                                  se=above_sb_group[["se.egt"]])

above_sb_grouped_plot <- ggplot(data = above_sb_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-80, 80), n.breaks = 7)+
  ylab("Difference in rate of stillbirths per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

above_abort_group.plot <- data.frame(time=above_abort_group[["egt"]],
                                     att=above_abort_group[["att.egt"]],
                                     se=above_abort_group[["se.egt"]])

above_abort_grouped_plot <- ggplot(data = above_abort_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-80, 80), n.breaks = 7)+
  ylab("Difference in rate of abortions per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

above_ms_group.plot <- data.frame(time=above_ms_group[["egt"]],
                                  att=above_ms_group[["att.egt"]],
                                  se=above_ms_group[["se.egt"]])

above_ms_grouped_plot <- ggplot(data = above_ms_group.plot, aes(x = as.factor(time), y = round(att, 2))) + geom_point(color = "#024b7a") +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, color = "#024b7a") + 
  #scale_color_binned(values = "#024b7a")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_y_continuous(limits = c(-80, 80), n.breaks = 7)+
  ylab("Difference in rate of miscarriages per 1,000 pregnancies")+
  xlab("Treatment Group")+
  theme_cowplot()

above_group_plots <- ggarrange(above_sb_grouped_plot, above_abort_grouped_plot, above_ms_grouped_plot,
                               labels = c("A", "B", "C"), nrow = 1)


#tables
above_sb_dynamic.tab <- tidy(above_sb_dynamic)
above_sb_dynamic.tab <- above_sb_dynamic.tab %>% select(-c(type, term, point.conf.low, point.conf.high))
above_sb_dynamic.tab <- above_sb_dynamic.tab %>% round_half_up(.,1)
above_sb_dynamic.tab$Outcome <- c("Stillbirth")

above_abort_dynamic.tab <- tidy(above_abort_dynamic)
above_abort_dynamic.tab <- above_abort_dynamic.tab %>% select(-c(type, term, point.conf.low, point.conf.high))
above_abort_dynamic.tab <- above_abort_dynamic.tab %>% round_half_up(.,1)
above_abort_dynamic.tab$Outcome <- c("Abortion")


above_ms_dynamic.tab <- tidy(above_ms_dynamic)
above_ms_dynamic.tab <- above_ms_dynamic.tab %>% select(-c(type, term, point.conf.low, point.conf.high))
above_ms_dynamic.tab <- above_ms_dynamic.tab %>% round_half_up(.,1)
above_ms_dynamic.tab$Outcome <- c("Miscarriage")

above_dynamic <- rbind(above_sb_dynamic.tab, above_abort_dynamic.tab, above_ms_dynamic.tab)
above_dynamic_flextable <- flextable(above_dynamic)
save_as_docx(above_dynamic_flextable, path = "above_dynamic_tab.docx")


#### Stratified results overall ATT plots ####

rural_sb_overall_att <- data.frame(strata=c("Rural"),
                                   att=rural_sb_dynamic[["overall.att"]],
                                   se=rural_sb_dynamic[["overall.se"]],
                                   outcome = "Stillbirth")

rural_abort_overall_att <- data.frame(strata=c("Rural"),
                                   att=rural_abort_dynamic[["overall.att"]],
                                   se=rural_abort_dynamic[["overall.se"]],
                                   outcome = "Abortion")

rural_ms_overall_att <- data.frame(strata=c("Rural"),
                                   att=rural_ms_dynamic[["overall.att"]],
                                   se=rural_ms_dynamic[["overall.se"]],
                                   outcome = "Miscarriage")

rural_atts <- rbind(rural_sb_overall_att, rural_abort_overall_att, rural_ms_overall_att)


urban_sb_overall_att <- data.frame(strata=c("Urban"),
                                   att=urban_sb_dynamic[["overall.att"]],
                                   se=urban_sb_dynamic[["overall.se"]],
                                   outcome = "Stillbirth")

urban_abort_overall_att <- data.frame(strata=c("Urban"),
                                      att=urban_abort_dynamic[["overall.att"]],
                                      se=urban_abort_dynamic[["overall.se"]],
                                      outcome = "Abortion")

urban_ms_overall_att <- data.frame(strata=c("Urban"),
                                   att=urban_ms_dynamic[["overall.att"]],
                                   se=urban_ms_dynamic[["overall.se"]],
                                   outcome = "Miscarriage")

urban_atts <- rbind(urban_sb_overall_att, urban_abort_overall_att, urban_ms_overall_att)

below_sb_overall_att <- data.frame(strata=c("Below Median"),
                                   att=below_sb_dynamic[["overall.att"]],
                                   se=below_sb_dynamic[["overall.se"]],
                                   outcome = "Stillbirth")

below_abort_overall_att <- data.frame(strata=c("Below Median"),
                                      att=below_abort_dynamic[["overall.att"]],
                                      se=below_abort_dynamic[["overall.se"]],
                                      outcome = "Abortion")

below_ms_overall_att <- data.frame(strata=c("Below Median"),
                                   att=below_ms_dynamic[["overall.att"]],
                                   se=below_ms_dynamic[["overall.se"]],
                                   outcome = "Miscarriage")


below_atts <- rbind(below_sb_overall_att, below_abort_overall_att, below_ms_overall_att)

above_sb_overall_att <- data.frame(strata=c("Above Median"),
                                   att=above_sb_dynamic[["overall.att"]],
                                   se=above_sb_dynamic[["overall.se"]],
                                   outcome = "Stillbirth")

above_abort_overall_att <- data.frame(strata=c("Above Median"),
                                      att=above_abort_dynamic[["overall.att"]],
                                      se=above_abort_dynamic[["overall.se"]],
                                      outcome = "Abortion")

above_ms_overall_att <- data.frame(strata=c("Above Median"),
                                   att=above_ms_dynamic[["overall.att"]],
                                   se=above_ms_dynamic[["overall.se"]],
                                   outcome = "Miscarriage")


above_atts <- rbind(above_sb_overall_att, above_abort_overall_att, above_ms_overall_att)


noprim_sb_overall_att <- data.frame(strata=c("No Primary"),
                                   att=noprim_sb_dynamic[["overall.att"]],
                                   se=noprim_sb_dynamic[["overall.se"]],
                                   outcome = "Stillbirth")

noprim_abort_overall_att <- data.frame(strata=c("No Primary"),
                                      att=noprim_abort_dynamic[["overall.att"]],
                                      se=noprim_abort_dynamic[["overall.se"]],
                                      outcome = "Abortion")

noprim_ms_overall_att <- data.frame(strata=c("No Primary"),
                                   att=noprim_ms_dynamic[["overall.att"]],
                                   se=noprim_ms_dynamic[["overall.se"]],
                                   outcome = "Miscarriage")


noprim_atts <- rbind(noprim_sb_overall_att, noprim_abort_overall_att, noprim_ms_overall_att)

prim_sb_overall_att <- data.frame(strata=c("Completed Primary"),
                                   att=prim_sb_dynamic[["overall.att"]],
                                   se=prim_sb_dynamic[["overall.se"]],
                                   outcome = "Stillbirth")

prim_abort_overall_att <- data.frame(strata=c("Completed Primary"),
                                      att=prim_abort_dynamic[["overall.att"]],
                                      se=prim_abort_dynamic[["overall.se"]],
                                      outcome = "Abortion")

prim_ms_overall_att <- data.frame(strata=c("Completed Primary"),
                                   att=prim_ms_dynamic[["overall.att"]],
                                   se=prim_ms_dynamic[["overall.se"]],
                                   outcome = "Miscarriage")


prim_atts <- rbind(prim_sb_overall_att, prim_abort_overall_att, prim_ms_overall_att)



strat_atts <- rbind(rural_atts, urban_atts, below_atts, above_atts, noprim_atts, prim_atts)


#now making plot
mycolors_outcomes <- c("#024b7a", "#e67e00", "#44b7c2")

#making facetwrap variable
strat_atts <- strat_atts %>% mutate(facet = case_when(strata == "Rural" ~ "Location",
                                                                  strata == "Urban" ~ "Location",
                                                                  strata == "No Primary" ~ "Education",
                                                                  strata == "Completed Primary" ~ "Education",
                                                                  TRUE ~ "Wealth"))

ggplot(data = strat_atts, aes(x = as.factor(strata), y = round(att, 2), color = outcome)) + geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_color_manual(values = mycolors_outcomes, name = "Outcome")+
  facet_wrap(~facet,  scales = "free_x")+
  scale_y_continuous(limits = c(-30, 30), n.breaks = 6)+
  ylab("Difference in rate of outcome per 1,000 pregnancies")+
  xlab("Stratification Group")+
  theme_cowplot()


strat_atts_plot_poster <- ggplot(data = strat_atts, aes(x = as.factor(strata), y = round(att, 2), color = outcome)) + geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = round((att - 1.96*se),2), ymax = round((att + 1.96*se),2)), width = 0.2, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept=0,lty=2,color="gray59")+
  scale_color_manual(values = mycolors_outcomes, name = "Outcome")+
  facet_wrap(~facet,  scales = "free_x")+
  scale_y_continuous(limits = c(-30, 30), n.breaks = 6)+
  ylab("Difference in rate of outcome per 1,000 pregnancies")+
  xlab("Stratification Group")+
  theme_cowplot(26)

cowplot::save_plot(
  "strat_atts_plot_poster.png",
  plot = strat_atts_plot_poster,
  base_height = 12,
  base_width = 25
)


#now making tables

flextable::flextable(tidy(rural_sb_group))


##### EAG stratified#####

#not using May 31

dist_eag <- dists_df %>% filter(eag == 1)

dist_eag_sb <- att_gt(yname = "sb_rate",
                      tname = "outcome_year",
                      gname = "enrollgroup",
                      idname = "dist_id",
                      xformla = ~ (med_age^2),
                      data = dist_eag,
                      panel = TRUE,
                      clustervars = "dist_id",
                      control_group = "notyettreated",
                      print_details = TRUE,
                      bstrap=TRUE, cband=FALSE
)

dist_eag_abort <- att_gt(yname = "abort_rate",
                         tname = "outcome_year",
                         gname = "enrollgroup",
                         idname = "dist_id",
                         xformla = ~ (med_age^2),
                         data = dist_eag,
                         panel = TRUE,
                         clustervars = "dist_id",
                         control_group = "notyettreated",
                         print_details = TRUE,
                         bstrap=TRUE, cband=FALSE
)

dist_eag_ms <- att_gt(yname = "ms_rate",
                      tname = "outcome_year",
                      gname = "enrollgroup",
                      idname = "dist_id",
                      xformla = ~ (med_age^2),
                      data = dist_eag,
                      panel = TRUE,
                      clustervars = "dist_id",
                      control_group = "notyettreated",
                      print_details = TRUE,
                      bstrap=TRUE, cband=FALSE
)

dist_eags_sb_group <- aggte(dist_eag_sb, type = "group")
dist_eags_sb_dynamic <- aggte(dist_eag_sb, type = "dynamic")
dist_eags_sb_calendar <- aggte(dist_eag_sb, type = "calendar")

dist_eags_abort_group <- aggte(dist_eag_abort, type = "group")
dist_eags_abort_dynamic <- aggte(dist_eag_abort, type = "dynamic")
dist_eags_abort_calendar <- aggte(dist_eag_abort, type = "calendar")

dist_eags_ms_group <- aggte(dist_eag_ms, type = "group")
dist_eags_ms_dynamic <- aggte(dist_eag_ms, type = "dynamic")
dist_eags_ms_calendar <- aggte(dist_eag_ms, type = "calendar")


dists_ms_dynamic.dyn <- data.frame(time=dists_ms_dynamic[["egt"]],
                                   att=dists_ms_dynamic[["att.egt"]],
                                   se=dists_ms_dynamic[["se.egt"]])

dists_ms_dynamic.dyn  <- dists_ms_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))

ggplot(dists_ms_dynamic.dyn)+
  geom_rect( aes(xmin=0.1,
                 xmax=9,
                 ymin=(dists_ms_dynamic[["overall.att"]]-1.96*dists_ms_dynamic[["overall.se"]]),
                 ymax=(dists_ms_dynamic[["overall.att"]]+1.96*dists_ms_dynamic[["overall.se"]]), alpha="95% CI"), fill = alpha("lightblue"),
             color="lightblue")+
  geom_segment( 
    aes(x = 0, y = dists_ms_dynamic[["overall.att"]], xend = 9, yend = dists_ms_dynamic[["overall.att"]], lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att,2),ymin = round((att-1.96*se),2), ymax = round((att+1.96*se),2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Difference in rate of miscarriages per 1,000 pregnancies")+
  xlab("Time since received access to RSBY")+
  scale_x_continuous(breaks=c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9),
                     labels = c("-9","-8","-7","6","-5","-4","-3","-2","-1","0","+1","+2","+3","+4","+5","+6","+7","+8","+9"))+
  ylim(-30,30)+
  
  scale_color_manual(values=c("#e67e00","#024b7a"),labels=c("Pre Intervention","Post Intervention"))+
  scale_alpha_manual(values=0.3,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                width=25))+
  
  scale_linetype_manual(values=2,labels=str_wrap("Overall summary of dynamic ATT's, 95%CI",
                                                 width=25))+
  labs(color=str_wrap("Event-time average treatment effects (ATT), 95%CI",
                      width=28), alpha="",linetype="")+
  geom_vline(xintercept=-0.1,lty=2,color="gray59")+
  geom_hline(yintercept=0,lty=2,color="gray59")+
  theme_bw()+
  guides(color = guide_legend(order=1,override.aes = list(size =0.5)))+
  guides(alpha=guide_legend(override.aes = list(alpha =0.5),order=2))+
  guides(linetype = guide_legend(override.aes=list(size=1),order=2))+
  theme(legend.text = element_text(size=10),
        legend.title = element_text(size=10))+
  theme_cowplot()

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
library(srvyr)
nfhs_svy_design <- srvyr::as_survey_design(nfhs, ids = psu2, strata = strat_rurb, weights = weight_adj, nest = TRUE)
df_svy_design <- srvyr::as_survey_design(df, ids = psu2, strata = strat_rurb, weights = weight_adj, nest = TRUE)

dist_rates_weighted <- df_svy_design %>% group_by(dist_id, outcome_year) %>% summarize(sb = survey_total(sb>0),
                                                                                       abort = survey_total(abort > 0),
                                                                                       ms = survey_total(miscarriage > 0),
                                                                                       pregs = survey_total(outcome > 0),
                                                                                       wealth = survey_mean(wi_perc_rank),
                                                                                       eag = mean(EAG),
                                                                                       prop_primary = survey_mean(primary_school),
                                                                                       rurb = survey_mean(rural_urban),
                                                                                       sc = survey_total(caste_group == 1),
                                                                                       st = survey_total(caste_group == 2),
                                                                                       non_scheduled = survey_total(caste_group == 0),
                                                                                       med_age = survey_median(age),
                                                                                       enrollgroup = mean(g)
)


dist_rates_weighted <- data.frame(dist_rates_weighted)
#setting design
#making strata variable from rural/urban
df$strat_rurb <- ifelse(df$rural_urban == 0, 2, 1)
table(df$strat_rurb)
df$year <- as.factor(df$outcome_year)
df$dists_fact <- as.factor(df$dist_id)

design <- svydesign(data = df, ids = ~psu2, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)

svy_unweighted <- lm(sb ~ dists_fact * year, data = df)

svy_unweight_nfhs <- lm(sb ~ dist_id * year, data = nfhs)

svy_dist_rate <- svyby(~sb, ~outcome_year*~dist_id, design, svytotal, vartype=c("se","ci"), na.rm.all = TRUE)

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
###### combining rural group estimates into one plot
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

#filtering out only average overall
rural_group_atts_overall <- rural_group_atts %>% filter(group == "Average")

library(cowplot)
rural_atts_group_plot <- ggplot(data = rural_group_atts_overall, mapping = aes(x = outcome, y = (estimate*100)#, 
                                                                               #color = outcome
)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin = (conf.low*100), ymax = (conf.high*100)), width = 0.1, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, color = I("black"), linetype = 2)+
  #ylim(-8, 8)+
  scale_y_continuous(limits = c(-7, 7), breaks = c(-6, -4,-2,0,2, 4,6))+
  scale_color_manual(values = mycolors_outcomes, name = "Outcome", breaks = c("Stillbirth", "Abortion", "Miscarriage"))+
  ylab("Overall ATT -- Rural (Percentage-Points)")+
  xlab("Adverse Pregnancy Outcome") +
  theme_cowplot()

#now add in urban group estimates
urban_abort_group <- aggte(urban_att_abort, type = "group")
urban_abort_group_tidy <- tidy(urban_abort_group)
urban_abort_group_tidy$outcome <- c("Abortion")
urban_abort_group_tidy$est_per1000 <- urban_abort_group_tidy$estimate*1000
urban_abort_group_tidy$stderr_per1000 <- urban_abort_group_tidy$std.error*1000
urban_abort_group_tidy$conflow_per1000 <- urban_abort_group_tidy$conf.low*1000
urban_abort_group_tidy$confhigh_per1000 <- urban_abort_group_tidy$conf.high*1000



urban_sb_group <- aggte(urban_sb_att, type = "group")

urban_sb_group_tidy <- tidy(urban_sb_group)
urban_sb_group_tidy$outcome <- c("Stillbirth")
urban_sb_group_tidy$est_per1000 <- urban_sb_group_tidy$estimate*1000
urban_sb_group_tidy$stderr_per1000 <- urban_sb_group_tidy$std.error*1000
urban_sb_group_tidy$conflow_per1000 <- urban_sb_group_tidy$conf.low*1000
urban_sb_group_tidy$confhigh_per1000 <- urban_sb_group_tidy$conf.high*1000



urban_miscarriage_group <- aggte(urban_att_miscarriage, type = "group")

urban_miscarriage_group_tidy <- tidy(urban_miscarriage_group)
urban_miscarriage_group_tidy$outcome <- c("Miscarriage")
urban_miscarriage_group_tidy$est_per1000 <- urban_miscarriage_group_tidy$estimate*1000
urban_miscarriage_group_tidy$stderr_per1000 <- urban_miscarriage_group_tidy$std.error*1000
urban_miscarriage_group_tidy$conflow_per1000 <- urban_miscarriage_group_tidy$conf.low*1000
urban_miscarriage_group_tidy$confhigh_per1000 <- urban_miscarriage_group_tidy$conf.high*1000


urban_group_atts <- rbind(urban_sb_group_tidy, urban_abort_group_tidy, urban_miscarriage_group_tidy)

#filtering out only average overall
urban_group_atts_overall <- urban_group_atts %>% filter(group == "Average")

#combining urban and rural atts
rural_group_atts_overall$strata <- c("Rural")
urban_group_atts_overall$strata <- c("Urban")

rural_urban_atts_overall <- rbind(rural_group_atts_overall, urban_group_atts_overall)


#now combining into plot
rural_urban_atts_group_plot <- ggplot(data = rural_urban_atts_overall, mapping = aes(x = outcome, y = (estimate*100), color = strata)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin = (conf.low*100), ymax = (conf.high*100)), width = 0.1, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, color = I("black"), linetype = 2)+
  #ylim(-8, 8)+
  scale_y_continuous(limits = c(-7, 7), breaks = c(-6, -4,-2,0,2, 4,6))+
  #scale_color_manual(values = mycolors_outcomes, name = "Outcome", breaks = c("Stillbirth", "Abortion", "Miscarriage"))+
  ylab("Overall ATT (Percentage-Points)")+
  xlab("Adverse Pregnancy Outcome") +
  theme_cowplot()

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

#filtering out only average overall
rural_group_atts_overall <- rural_group_atts %>% filter(group == "Average")

library(cowplot)
rural_atts_group_plot <- ggplot(data = rural_group_atts_overall, mapping = aes(x = outcome, y = (estimate*100)#, 
                                                                               #color = outcome
)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin = (conf.low*100), ymax = (conf.high*100)), width = 0.1, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, color = I("black"), linetype = 2)+
  #ylim(-8, 8)+
  scale_y_continuous(limits = c(-7, 7), breaks = c(-6, -4,-2,0,2, 4,6))+
  scale_color_manual(values = mycolors_outcomes, name = "Outcome", breaks = c("Stillbirth", "Abortion", "Miscarriage"))+
  ylab("Overall ATT -- Rural (Percentage-Points)")+
  xlab("Adverse Pregnancy Outcome") +
  theme_cowplot()

#now add in urban group estimates
urban_abort_group <- aggte(urban_att_abort, type = "group")
urban_abort_group_tidy <- tidy(urban_abort_group)
urban_abort_group_tidy$outcome <- c("Abortion")
urban_abort_group_tidy$est_per1000 <- urban_abort_group_tidy$estimate*1000
urban_abort_group_tidy$stderr_per1000 <- urban_abort_group_tidy$std.error*1000
urban_abort_group_tidy$conflow_per1000 <- urban_abort_group_tidy$conf.low*1000
urban_abort_group_tidy$confhigh_per1000 <- urban_abort_group_tidy$conf.high*1000



urban_sb_group <- aggte(urban_sb_att, type = "group")

urban_sb_group_tidy <- tidy(urban_sb_group)
urban_sb_group_tidy$outcome <- c("Stillbirth")
urban_sb_group_tidy$est_per1000 <- urban_sb_group_tidy$estimate*1000
urban_sb_group_tidy$stderr_per1000 <- urban_sb_group_tidy$std.error*1000
urban_sb_group_tidy$conflow_per1000 <- urban_sb_group_tidy$conf.low*1000
urban_sb_group_tidy$confhigh_per1000 <- urban_sb_group_tidy$conf.high*1000



urban_miscarriage_group <- aggte(urban_att_miscarriage, type = "group")

urban_miscarriage_group_tidy <- tidy(urban_miscarriage_group)
urban_miscarriage_group_tidy$outcome <- c("Miscarriage")
urban_miscarriage_group_tidy$est_per1000 <- urban_miscarriage_group_tidy$estimate*1000
urban_miscarriage_group_tidy$stderr_per1000 <- urban_miscarriage_group_tidy$std.error*1000
urban_miscarriage_group_tidy$conflow_per1000 <- urban_miscarriage_group_tidy$conf.low*1000
urban_miscarriage_group_tidy$confhigh_per1000 <- urban_miscarriage_group_tidy$conf.high*1000


urban_group_atts <- rbind(urban_sb_group_tidy, urban_abort_group_tidy, urban_miscarriage_group_tidy)

#filtering out only average overall
urban_group_atts_overall <- urban_group_atts %>% filter(group == "Average")

#combining urban and rural atts
rural_group_atts_overall$strata <- c("Rural")
urban_group_atts_overall$strata <- c("Urban")

rural_urban_atts_overall <- rbind(rural_group_atts_overall, urban_group_atts_overall)


#now combining into plot
rural_urban_atts_group_plot <- ggplot(data = rural_urban_atts_overall, mapping = aes(x = outcome, y = (estimate*100), color = strata)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin = (conf.low*100), ymax = (conf.high*100)), width = 0.1, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, color = I("black"), linetype = 2)+
  #ylim(-8, 8)+
  scale_y_continuous(limits = c(-7, 7), breaks = c(-6, -4,-2,0,2, 4,6))+
  #scale_color_manual(values = mycolors_outcomes, name = "Outcome", breaks = c("Stillbirth", "Abortion", "Miscarriage"))+
  ylab("Overall ATT (Percentage-Points)")+
  xlab("Adverse Pregnancy Outcome") +
  theme_cowplot()



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
                        xformla = ~1,
                        data = quint1,
                        panel = FALSE,
                        allow_unbalanced_panel = TRUE,
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
                           xformla = ~1,
                           data = quint1,
                           panel = FALSE,
                           allow_unbalanced_panel = TRUE,
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
                                 xformla = ~1,
                                 data = quint1,
                                 panel = FALSE,
                                 allow_unbalanced_panel = TRUE,
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
                        xformla = ~1,
                        data = quint2,
                        panel = FALSE,
                        allow_unbalanced_panel = TRUE,
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
                           xformla = ~1,
                           data = quint2,
                           panel = FALSE,
                           allow_unbalanced_panel = TRUE,
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
                                 xformla = ~1,
                                 data = quint2,
                                 panel = FALSE,
                                 allow_unbalanced_panel = TRUE,
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
                        xformla = ~1,
                        data = quint3,
                        panel = FALSE,
                        allow_unbalanced_panel = TRUE,
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
                           xformla = ~1,
                           data = quint3,
                           panel = FALSE,
                           allow_unbalanced_panel = TRUE,
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
                                 xformla = ~1,
                                 data = quint3,
                                 panel = FALSE,
                                 allow_unbalanced_panel = TRUE,
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
                        xformla = ~1,
                        data = quint4,
                        panel = FALSE,
                        allow_unbalanced_panel = TRUE,
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
                           xformla = ~1,
                           data = quint4,
                           panel = FALSE,
                           allow_unbalanced_panel = TRUE,
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
                                 xformla = ~1,
                                 data = quint4,
                                 panel = FALSE,
                                 allow_unbalanced_panel = TRUE,
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
                        xformla = ~1,
                        data = quint5,
                        panel = FALSE,
                        allow_unbalanced_panel = TRUE,
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
                           xformla = ~1,
                           data = quint5,
                           panel = FALSE,
                           allow_unbalanced_panel = TRUE,
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
                                 xformla = ~1,
                                 data = quint5,
                                 panel = FALSE,
                                 allow_unbalanced_panel = TRUE,
                                 weightsname = "weight_adj",
                                 control_group = "notyettreated",
                                 print_details = TRUE,
                                 #est_method = "reg",
                                 bstrap=TRUE, cband=FALSE
)

#multiplying estimates by 1000 and making event and calendar time models and plots
#quint1_sb_att$attper1000 <- quint1_sb_att$att * 1000
quint1_sb_att.group <- aggte(quint1_sb_att, type = "group", na.rm = TRUE)

#quint1_sb_att.est <- round(quint1_sb_att.group[["overall.att"]]*1000)
#quint1_sb_att.cilow <- round((quint1_sb_att.group[["overall.att"]]-1.96*quint1_sb_att.group[["overall.se"]])*1000)
#quint1_sb_att.ciup <- round((quint1_sb_att.group[["overall.att"]]+1.96*quint1_sb_att.group[["overall.se"]])*1000)

quint1_sb_att.group.plot <- ggdid(quint1_sb_att.group)

quint1_sb_att.dynamic <- aggte(quint1_sb_att, type = "dynamic")
quint1_sb_att.dynamic.plot <- ggdid(quint1_sb_att.dynamic)

quint1_sb_att.calendar <- aggte(quint1_sb_att, type = "calendar")
quint1_sb_att.calendar.plot <- ggdid(quint1_sb_att)

#quint1_att_abort$attper1000 <- quint1_att_abort$att * 1000
quint1_att_abort.group <- aggte(quint1_att_abort, type = "group", na.rm = TRUE)

#quint1_att_abort.est <- round(quint1_att_abort.group[["overall.att"]]*1000)
#quint1_att_abort.cilow <- round((quint1_att_abort.group[["overall.att"]]-1.96*quint1_att_abort.group[["overall.se"]])*1000)
#quint1_att_abort.ciup <- round((quint1_att_abort.group[["overall.att"]]+1.96*quint1_att_abort.group[["overall.se"]])*1000)

quint1_att_abort.group.plot <- ggdid(quint1_att_abort.group)

quint1_att_abort.dynamic <- aggte(quint1_att_abort, type = "dynamic")
quint1_att_abort.dynamic.plot <- ggdid(quint1_att_abort.dynamic)

quint1_att_abort.calendar <- aggte(quint1_att_abort, type = "calendar")
quint1_att_abort.calendar.plot <- ggdid(quint1_att_abort)

#quint1_att_miscarriage$attper1000 <- quint1_att_miscarriage$att * 1000
quint1_att_miscarriage.group <- aggte(quint1_att_miscarriage, type = "group", na.rm = TRUE)

#quint1_att_miscarriage.est <- round(quint1_att_miscarriage.group[["overall.att"]]*1000)
#quint1_att_miscarriage.cilow <- round((quint1_att_miscarriage.group[["overall.att"]]-1.96*quint1_att_miscarriage.group[["overall.se"]])*1000)
#quint1_att_miscarriage.ciup <- round((quint1_att_miscarriage.group[["overall.att"]]+1.96*quint1_att_miscarriage.group[["overall.se"]])*1000)

quint1_att_miscarriage.group.plot <- ggdid(quint1_att_miscarriage.group)

quint1_att_miscarriage.dynamic <- aggte(quint1_att_miscarriage, type = "dynamic")
quint1_att_miscarriage.dynamic.plot <- ggdid(quint1_att_miscarriage.dynamic)

quint1_att_miscarriage.calendar <- aggte(quint1_att_miscarriage, type = "calendar")
quint1_att_miscarriage.calendar.plot <- ggdid(quint1_att_miscarriage)

### Quint 2
#quint2_sb_att$attper1000 <- quint2_sb_att$att * 1000
quint2_sb_att.group <- aggte(quint2_sb_att, type = "group", na.rm = TRUE)

#quint2_sb_att.est <- round(quint2_sb_att.group[["overall.att"]]*1000)
#quint2_sb_att.cilow <- round((quint2_sb_att.group[["overall.att"]]-1.96*quint2_sb_att.group[["overall.se"]])*1000)
#quint2_sb_att.ciup <- round((quint2_sb_att.group[["overall.att"]]+1.96*quint2_sb_att.group[["overall.se"]])*1000)

quint2_sb_att.group.plot <- ggdid(quint2_sb_att.group)

quint2_sb_att.dynamic <- aggte(quint2_sb_att, type = "dynamic")
quint2_sb_att.dynamic.plot <- ggdid(quint2_sb_att.dynamic)

quint2_sb_att.calendar <- aggte(quint2_sb_att, type = "calendar")
quint2_sb_att.calendar.plot <- ggdid(quint2_sb_att)

#quint2_att_abort$attper1000 <- quint2_att_abort$att * 1000
quint2_att_abort.group <- aggte(quint2_att_abort, type = "group", na.rm = TRUE)

#quint2_att_abort.est <- round(quint2_att_abort.group[["overall.att"]]*1000)
#quint2_att_abort.cilow <- round((quint2_att_abort.group[["overall.att"]]-1.96*quint2_att_abort.group[["overall.se"]])*1000)
#quint2_att_abort.ciup <- round((quint2_att_abort.group[["overall.att"]]+1.96*quint2_att_abort.group[["overall.se"]])*1000)

quint2_att_abort.group.plot <- ggdid(quint2_att_abort.group)

quint2_att_abort.dynamic <- aggte(quint2_att_abort, type = "dynamic")
quint2_att_abort.dynamic.plot <- ggdid(quint2_att_abort.dynamic)

quint2_att_abort.calendar <- aggte(quint2_att_abort, type = "calendar")
quint2_att_abort.calendar.plot <- ggdid(quint2_att_abort)

#quint2_att_miscarriage$attper1000 <- quint2_att_miscarriage$att * 1000
quint2_att_miscarriage.group <- aggte(quint2_att_miscarriage, type = "group", na.rm = TRUE)

#quint2_att_miscarriage.est <- round(quint2_att_miscarriage.group[["overall.att"]]*1000)
#quint2_att_miscarriage.cilow <- round((quint2_att_miscarriage.group[["overall.att"]]-1.96*quint2_att_miscarriage.group[["overall.se"]])*1000)
#quint2_att_miscarriage.ciup <- round((quint2_att_miscarriage.group[["overall.att"]]+1.96*quint2_att_miscarriage.group[["overall.se"]])*1000)

quint2_att_miscarriage.group.plot <- ggdid(quint2_att_miscarriage.group)

quint2_att_miscarriage.dynamic <- aggte(quint2_att_miscarriage, type = "dynamic")
quint2_att_miscarriage.dynamic.plot <- ggdid(quint2_att_miscarriage.dynamic)

quint2_att_miscarriage.calendar <- aggte(quint2_att_miscarriage, type = "calendar")
quint2_att_miscarriage.calendar.plot <- ggdid(quint2_att_miscarriage)

## Quint 3 
#quint3_sb_att$attper1000 <- quint3_sb_att$att * 1000
quint3_sb_att.group <- aggte(quint3_sb_att, type = "group", na.rm = TRUE)

#quint3_sb_att.est <- round(quint3_sb_att.group[["overall.att"]]*1000)
#quint3_sb_att.cilow <- round((quint3_sb_att.group[["overall.att"]]-1.96*quint3_sb_att.group[["overall.se"]])*1000)
#quint3_sb_att.ciup <- round((quint3_sb_att.group[["overall.att"]]+1.96*quint3_sb_att.group[["overall.se"]])*1000)

quint3_sb_att.group.plot <- ggdid(quint3_sb_att.group)

quint3_sb_att.dynamic <- aggte(quint3_sb_att, type = "dynamic")
quint3_sb_att.dynamic.plot <- ggdid(quint3_sb_att.dynamic)

quint3_sb_att.calendar <- aggte(quint3_sb_att, type = "calendar")
quint3_sb_att.calendar.plot <- ggdid(quint3_sb_att)

#quint3_att_abort$attper1000 <- quint3_att_abort$att * 1000
quint3_att_abort.group <- aggte(quint3_att_abort, type = "group", na.rm = TRUE)

#quint3_att_abort.est <- round(quint3_att_abort.group[["overall.att"]]*1000)
#quint3_att_abort.cilow <- round((quint3_att_abort.group[["overall.att"]]-1.96*quint3_att_abort.group[["overall.se"]])*1000)
#quint3_att_abort.ciup <- round((quint3_att_abort.group[["overall.att"]]+1.96*quint3_att_abort.group[["overall.se"]])*1000)

quint3_att_abort.group.plot <- ggdid(quint3_att_abort.group)

quint3_att_abort.dynamic <- aggte(quint3_att_abort, type = "dynamic")
quint3_att_abort.dynamic.plot <- ggdid(quint3_att_abort.dynamic)

quint3_att_abort.calendar <- aggte(quint3_att_abort, type = "calendar")
quint3_att_abort.calendar.plot <- ggdid(quint3_att_abort)

#quint3_att_miscarriage$attper1000 <- quint3_att_miscarriage$att * 1000
quint3_att_miscarriage.group <- aggte(quint3_att_miscarriage, type = "group", na.rm = TRUE)

#quint3_att_miscarriage.est <- round(quint3_att_miscarriage.group[["overall.att"]]*1000)
#quint3_att_miscarriage.cilow <- round((quint3_att_miscarriage.group[["overall.att"]]-1.96*quint3_att_miscarriage.group[["overall.se"]])*1000)
#quint3_att_miscarriage.ciup <- round((quint3_att_miscarriage.group[["overall.att"]]+1.96*quint3_att_miscarriage.group[["overall.se"]])*1000)

quint3_att_miscarriage.group.plot <- ggdid(quint3_att_miscarriage.group)

quint3_att_miscarriage.dynamic <- aggte(quint3_att_miscarriage, type = "dynamic")
quint3_att_miscarriage.dynamic.plot <- ggdid(quint3_att_miscarriage.dynamic)

quint3_att_miscarriage.calendar <- aggte(quint3_att_miscarriage, type = "calendar")
quint3_att_miscarriage.calendar.plot <- ggdid(quint3_att_miscarriage)


# Quint 4
#quint4_sb_att$attper1000 <- quint4_sb_att$att * 1000
quint4_sb_att.group <- aggte(quint4_sb_att, type = "group", na.rm = TRUE)

#quint4_sb_att.est <- round(quint4_sb_att.group[["overall.att"]]*1000)
#quint4_sb_att.cilow <- round((quint4_sb_att.group[["overall.att"]]-1.96*quint4_sb_att.group[["overall.se"]])*1000)
#quint4_sb_att.ciup <- round((quint4_sb_att.group[["overall.att"]]+1.96*quint4_sb_att.group[["overall.se"]])*1000)

quint4_sb_att.group.plot <- ggdid(quint4_sb_att.group)

quint4_sb_att.dynamic <- aggte(quint4_sb_att, type = "dynamic")
quint4_sb_att.dynamic.plot <- ggdid(quint4_sb_att.dynamic)

quint4_sb_att.calendar <- aggte(quint4_sb_att, type = "calendar")
quint4_sb_att.calendar.plot <- ggdid(quint4_sb_att)

#quint4_att_abort$attper1000 <- quint4_att_abort$att * 1000
quint4_att_abort.group <- aggte(quint4_att_abort, type = "group", na.rm = TRUE)

#quint4_att_abort.est <- round(quint4_att_abort.group[["overall.att"]]*1000)
#quint4_att_abort.cilow <- round((quint4_att_abort.group[["overall.att"]]-1.96*quint4_att_abort.group[["overall.se"]])*1000)
#quint4_att_abort.ciup <- round((quint4_att_abort.group[["overall.att"]]+1.96*quint4_att_abort.group[["overall.se"]])*1000)

quint4_att_abort.group.plot <- ggdid(quint4_att_abort.group)

quint4_att_abort.dynamic <- aggte(quint4_att_abort, type = "dynamic")
quint4_att_abort.dynamic.plot <- ggdid(quint4_att_abort.dynamic)

quint4_att_abort.calendar <- aggte(quint4_att_abort, type = "calendar")
quint4_att_abort.calendar.plot <- ggdid(quint4_att_abort)

#quint4_att_miscarriage$attper1000 <- quint4_att_miscarriage$att * 1000
quint4_att_miscarriage.group <- aggte(quint4_att_miscarriage, type = "group", na.rm = TRUE)

#quint4_att_miscarriage.est <- round(quint4_att_miscarriage.group[["overall.att"]]*1000)
#quint4_att_miscarriage.cilow <- round((quint4_att_miscarriage.group[["overall.att"]]-1.96*quint4_att_miscarriage.group[["overall.se"]])*1000)
#quint4_att_miscarriage.ciup <- round((quint4_att_miscarriage.group[["overall.att"]]+1.96*quint4_att_miscarriage.group[["overall.se"]])*1000)

quint4_att_miscarriage.group.plot <- ggdid(quint4_att_miscarriage.group)

quint4_att_miscarriage.dynamic <- aggte(quint4_att_miscarriage, type = "dynamic")
quint4_att_miscarriage.dynamic.plot <- ggdid(quint4_att_miscarriage.dynamic)

quint4_att_miscarriage.calendar <- aggte(quint4_att_miscarriage, type = "calendar")
quint4_att_miscarriage.calendar.plot <- ggdid(quint4_att_miscarriage)

# Quint 5

#quint5_sb_att$attper1000 <- quint5_sb_att$att * 1000
quint5_sb_att.group <- aggte(quint5_sb_att, type = "group", na.rm = TRUE)

#quint5_sb_att.est <- round(quint5_sb_att.group[["overall.att"]]*1000)
#quint5_sb_att.cilow <- round((quint5_sb_att.group[["overall.att"]]-1.96*quint5_sb_att.group[["overall.se"]])*1000)
#quint5_sb_att.ciup <- round((quint5_sb_att.group[["overall.att"]]+1.96*quint5_sb_att.group[["overall.se"]])*1000)

quint5_sb_att.group.plot <- ggdid(quint5_sb_att.group)

quint5_sb_att.dynamic <- aggte(quint5_sb_att, type = "dynamic")
quint5_sb_att.dynamic.plot <- ggdid(quint5_sb_att.dynamic)

quint5_sb_att.calendar <- aggte(quint5_sb_att, type = "calendar")
quint5_sb_att.calendar.plot <- ggdid(quint5_sb_att)

#quint5_att_abort$attper1000 <- quint5_att_abort$att * 1000
quint5_att_abort.group <- aggte(quint5_att_abort, type = "group", na.rm = TRUE)

#quint5_att_abort.est <- round(quint5_att_abort.group[["overall.att"]]*1000)
#quint5_att_abort.cilow <- round((quint5_att_abort.group[["overall.att"]]-1.96*quint5_att_abort.group[["overall.se"]])*1000)
#quint5_att_abort.ciup <- round((quint5_att_abort.group[["overall.att"]]+1.96*quint5_att_abort.group[["overall.se"]])*1000)

quint5_att_abort.group.plot <- ggdid(quint5_att_abort.group)

quint5_att_abort.dynamic <- aggte(quint5_att_abort, type = "dynamic")
quint5_att_abort.dynamic.plot <- ggdid(quint5_att_abort.dynamic)

quint5_att_abort.calendar <- aggte(quint5_att_abort, type = "calendar")
quint5_att_abort.calendar.plot <- ggdid(quint5_att_abort)

#quint5_att_miscarriage$attper1000 <- quint5_att_miscarriage$att * 1000
quint5_att_miscarriage.group <- aggte(quint5_att_miscarriage, type = "group", na.rm = TRUE)

#quint5_att_miscarriage.est <- round(quint5_att_miscarriage.group[["overall.att"]]*1000)
#quint5_att_miscarriage.cilow <- round((quint5_att_miscarriage.group[["overall.att"]]-1.96*quint5_att_miscarriage.group[["overall.se"]])*1000)
#quint5_att_miscarriage.ciup <- round((quint5_att_miscarriage.group[["overall.att"]]+1.96*quint5_att_miscarriage.group[["overall.se"]])*1000)

quint5_att_miscarriage.group.plot <- ggdid(quint5_att_miscarriage.group)

quint5_att_miscarriage.dynamic <- aggte(quint5_att_miscarriage, type = "dynamic")
quint5_att_miscarriage.dynamic.plot <- ggdid(quint5_att_miscarriage.dynamic)

quint5_att_miscarriage.calendar <- aggte(quint5_att_miscarriage, type = "calendar")
quint5_att_miscarriage.calendar.plot <- ggdid(quint5_att_miscarriage)


#tidying and making overall ATT plots and event study plots for miscarriage Quints 2 and 3


ggdid(quint1_abort_group)

quint1_abort_group_tidy <- tidy(quint1_att_abort.group)
quint1_abort_group_tidy$outcome <- c("Abortion")
quint1_abort_group_tidy$est_per1000 <- quint1_abort_group_tidy$estimate*1000
quint1_abort_group_tidy$stderr_per1000 <- quint1_abort_group_tidy$std.error*1000
quint1_abort_group_tidy$conflow_per1000 <- quint1_abort_group_tidy$conf.low*1000
quint1_abort_group_tidy$confhigh_per1000 <- quint1_abort_group_tidy$conf.high*1000





quint1_sb_group_tidy <- tidy(quint1_sb_att.group)
quint1_sb_group_tidy$outcome <- c("Stillbirth")
quint1_sb_group_tidy$est_per1000 <- quint1_sb_group_tidy$estimate*1000
quint1_sb_group_tidy$stderr_per1000 <- quint1_sb_group_tidy$std.error*1000
quint1_sb_group_tidy$conflow_per1000 <- quint1_sb_group_tidy$conf.low*1000
quint1_sb_group_tidy$confhigh_per1000 <- quint1_sb_group_tidy$conf.high*1000





quint1_miscarriage_group_tidy <- tidy(quint1_att_miscarriage.group)
quint1_miscarriage_group_tidy$outcome <- c("Miscarriage")
quint1_miscarriage_group_tidy$est_per1000 <- quint1_miscarriage_group_tidy$estimate*1000
quint1_miscarriage_group_tidy$stderr_per1000 <- quint1_miscarriage_group_tidy$std.error*1000
quint1_miscarriage_group_tidy$conflow_per1000 <- quint1_miscarriage_group_tidy$conf.low*1000
quint1_miscarriage_group_tidy$confhigh_per1000 <- quint1_miscarriage_group_tidy$conf.high*1000

quint2_abort_group_tidy <- tidy(quint2_att_abort.group)
quint2_abort_group_tidy$outcome <- c("Abortion")
quint2_abort_group_tidy$est_per1000 <- quint2_abort_group_tidy$estimate*1000
quint2_abort_group_tidy$stderr_per1000 <- quint2_abort_group_tidy$std.error*1000
quint2_abort_group_tidy$conflow_per1000 <- quint2_abort_group_tidy$conf.low*1000
quint2_abort_group_tidy$confhigh_per1000 <- quint2_abort_group_tidy$conf.high*1000





quint2_sb_group_tidy <- tidy(quint2_sb_att.group)
quint2_sb_group_tidy$outcome <- c("Stillbirth")
quint2_sb_group_tidy$est_per1000 <- quint2_sb_group_tidy$estimate*1000
quint2_sb_group_tidy$stderr_per1000 <- quint2_sb_group_tidy$std.error*1000
quint2_sb_group_tidy$conflow_per1000 <- quint2_sb_group_tidy$conf.low*1000
quint2_sb_group_tidy$confhigh_per1000 <- quint2_sb_group_tidy$conf.high*1000





quint2_miscarriage_group_tidy <- tidy(quint2_att_miscarriage.group)
quint2_miscarriage_group_tidy$outcome <- c("Miscarriage")
quint2_miscarriage_group_tidy$est_per1000 <- quint2_miscarriage_group_tidy$estimate*1000
quint2_miscarriage_group_tidy$stderr_per1000 <- quint2_miscarriage_group_tidy$std.error*1000
quint2_miscarriage_group_tidy$conflow_per1000 <- quint2_miscarriage_group_tidy$conf.low*1000
quint2_miscarriage_group_tidy$confhigh_per1000 <- quint2_miscarriage_group_tidy$conf.high*1000

quint3_abort_group_tidy <- tidy(quint3_att_abort.group)
quint3_abort_group_tidy$outcome <- c("Abortion")
quint3_abort_group_tidy$est_per1000 <- quint3_abort_group_tidy$estimate*1000
quint3_abort_group_tidy$stderr_per1000 <- quint3_abort_group_tidy$std.error*1000
quint3_abort_group_tidy$conflow_per1000 <- quint3_abort_group_tidy$conf.low*1000
quint3_abort_group_tidy$confhigh_per1000 <- quint3_abort_group_tidy$conf.high*1000





quint3_sb_group_tidy <- tidy(quint3_sb_att.group)
quint3_sb_group_tidy$outcome <- c("Stillbirth")
quint3_sb_group_tidy$est_per1000 <- quint3_sb_group_tidy$estimate*1000
quint3_sb_group_tidy$stderr_per1000 <- quint3_sb_group_tidy$std.error*1000
quint3_sb_group_tidy$conflow_per1000 <- quint3_sb_group_tidy$conf.low*1000
quint3_sb_group_tidy$confhigh_per1000 <- quint3_sb_group_tidy$conf.high*1000





quint3_miscarriage_group_tidy <- tidy(quint3_att_miscarriage.group)
quint3_miscarriage_group_tidy$outcome <- c("Miscarriage")
quint3_miscarriage_group_tidy$est_per1000 <- quint3_miscarriage_group_tidy$estimate*1000
quint3_miscarriage_group_tidy$stderr_per1000 <- quint3_miscarriage_group_tidy$std.error*1000
quint3_miscarriage_group_tidy$conflow_per1000 <- quint3_miscarriage_group_tidy$conf.low*1000
quint3_miscarriage_group_tidy$confhigh_per1000 <- quint3_miscarriage_group_tidy$conf.high*1000


quint4_abort_group_tidy <- tidy(quint4_att_abort.group)
quint4_abort_group_tidy$outcome <- c("Abortion")
quint4_abort_group_tidy$est_per1000 <- quint4_abort_group_tidy$estimate*1000
quint4_abort_group_tidy$stderr_per1000 <- quint4_abort_group_tidy$std.error*1000
quint4_abort_group_tidy$conflow_per1000 <- quint4_abort_group_tidy$conf.low*1000
quint4_abort_group_tidy$confhigh_per1000 <- quint4_abort_group_tidy$conf.high*1000


quint4_sb_group_tidy <- tidy(quint4_sb_att.group)
quint4_sb_group_tidy$outcome <- c("Stillbirth")
quint4_sb_group_tidy$est_per1000 <- quint4_sb_group_tidy$estimate*1000
quint4_sb_group_tidy$stderr_per1000 <- quint4_sb_group_tidy$std.error*1000
quint4_sb_group_tidy$conflow_per1000 <- quint4_sb_group_tidy$conf.low*1000
quint4_sb_group_tidy$confhigh_per1000 <- quint4_sb_group_tidy$conf.high*1000


quint4_miscarriage_group_tidy <- tidy(quint4_att_miscarriage.group)
quint4_miscarriage_group_tidy$outcome <- c("Miscarriage")
quint4_miscarriage_group_tidy$est_per1000 <- quint4_miscarriage_group_tidy$estimate*1000
quint4_miscarriage_group_tidy$stderr_per1000 <- quint4_miscarriage_group_tidy$std.error*1000
quint4_miscarriage_group_tidy$conflow_per1000 <- quint4_miscarriage_group_tidy$conf.low*1000
quint4_miscarriage_group_tidy$confhigh_per1000 <- quint4_miscarriage_group_tidy$conf.high*1000

quint5_abort_group_tidy <- tidy(quint5_att_abort.group)
quint5_abort_group_tidy$outcome <- c("Abortion")
quint5_abort_group_tidy$est_per1000 <- quint5_abort_group_tidy$estimate*1000
quint5_abort_group_tidy$stderr_per1000 <- quint5_abort_group_tidy$std.error*1000
quint5_abort_group_tidy$conflow_per1000 <- quint5_abort_group_tidy$conf.low*1000
quint5_abort_group_tidy$confhigh_per1000 <- quint5_abort_group_tidy$conf.high*1000





quint5_sb_group_tidy <- tidy(quint5_sb_att.group)
quint5_sb_group_tidy$outcome <- c("Stillbirth")
quint5_sb_group_tidy$est_per1000 <- quint5_sb_group_tidy$estimate*1000
quint5_sb_group_tidy$stderr_per1000 <- quint5_sb_group_tidy$std.error*1000
quint5_sb_group_tidy$conflow_per1000 <- quint5_sb_group_tidy$conf.low*1000
quint5_sb_group_tidy$confhigh_per1000 <- quint5_sb_group_tidy$conf.high*1000





quint5_miscarriage_group_tidy <- tidy(quint5_att_miscarriage.group)
quint5_miscarriage_group_tidy$outcome <- c("Miscarriage")
quint5_miscarriage_group_tidy$est_per1000 <- quint5_miscarriage_group_tidy$estimate*1000
quint5_miscarriage_group_tidy$stderr_per1000 <- quint5_miscarriage_group_tidy$std.error*1000
quint5_miscarriage_group_tidy$conflow_per1000 <- quint5_miscarriage_group_tidy$conf.low*1000
quint5_miscarriage_group_tidy$confhigh_per1000 <- quint5_miscarriage_group_tidy$conf.high*1000


quint1_group_atts <- rbind(quint1_sb_group_tidy, quint1_abort_group_tidy, quint1_miscarriage_group_tidy)
quint2_group_atts <- rbind(quint2_sb_group_tidy, quint2_abort_group_tidy, quint2_miscarriage_group_tidy)
quint3_group_atts <- rbind(quint3_sb_group_tidy, quint3_abort_group_tidy, quint3_miscarriage_group_tidy)
quint4_group_atts <- rbind(quint4_sb_group_tidy, quint4_abort_group_tidy, quint4_miscarriage_group_tidy)
quint5_group_atts <- rbind(quint5_sb_group_tidy, quint5_abort_group_tidy, quint5_miscarriage_group_tidy)

#filtering out only average overall
quint1_group_atts_overall <- quint1_group_atts %>% filter(group == "Average")

quint2_group_atts_overall <- quint2_group_atts %>% filter(group == "Average")

quint3_group_atts_overall <- quint3_group_atts %>% filter(group == "Average")

quint4_group_atts_overall <- quint4_group_atts %>% filter(group == "Average")

quint5_group_atts_overall <- quint5_group_atts %>% filter(group == "Average")

#combining urban and rural atts
quint1_group_atts_overall$strata <- c("quint1")

quint2_group_atts_overall$strata <- c("quint2")

quint3_group_atts_overall$strata <- c("quint3")

quint4_group_atts_overall$strata <- c("quint4")

quint5_group_atts_overall$strata <- c("quint5")

wealth_atts_overall <- rbind(quint1_group_atts_overall, quint2_group_atts_overall, quint3_group_atts_overall, quint4_group_atts_overall,
                             quint5_group_atts_overall)

mycolors_wi <- c("#024b7a", "#44b7c2", "#5ddf6c", "#458e48", "#e67e00")

wealth_atts_group_plot <- ggplot(data = wealth_atts_overall, mapping = aes(x = outcome, y = (estimate*100), color = strata)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin = (conf.low*100), ymax = (conf.high*100)), width = 0.1, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, color = I("black"), linetype = 2)+
  scale_color_manual(values = mycolors_wi,name = "Wealth Quintile"
                     #, breaks =c("quint1", "quint2", "quint3", "quint4", "quint5"), 
                     #labels = c("1 (Poorest)", "2", "3", "4", "5 (Richest)"
                     # )
  )+
  #ylim(-8, 8)+
  scale_y_continuous(limits = c(-7, 7), breaks = c(-6, -4,-2,0,2, 4,6))+
  #scale_color_manual(values = mycolors_outcomes, name = "Outcome", breaks = c("Stillbirth", "Abortion", "Miscarriage"))+
  ylab("Overall ATT (Percentage-Points)")+
  xlab("Adverse Pregnancy Outcome") +
  theme_cowplot()

#Event study plot quint 2 miscarriage

quint2_att_miscarriage.dynamic.dyn <- data.frame(time=quint2_att_miscarriage.dynamic[["egt"]],
                                                 att=quint2_att_miscarriage.dynamic[["att.egt"]],
                                                 se=quint2_att_miscarriage.dynamic[["se.egt"]])

quint2_att_miscarriage.dynamic.dyn  <- quint2_att_miscarriage.dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))



ggplot(quint2_att_miscarriage.dynamic.dyn)+
  geom_vline(xintercept=-0,lty=2,color="grey59")+
  geom_hline(yintercept=0,lty=2,color="grey59")+
  
  geom_rect( aes(xmin=0,
                 xmax=9,
                 ymin=(quint2_att_miscarriage.dynamic[["overall.att"]]-1.96*quint2_att_miscarriage.dynamic[["overall.se"]])*100,
                 ymax=(quint2_att_miscarriage.dynamic[["overall.att"]]+1.96*quint2_att_miscarriage.dynamic[["overall.se"]])*100, alpha="95% CI"), fill="lightblue",
             color=NA)+
  geom_segment( 
    aes(x = 0, y = quint2_att_miscarriage.dynamic[["overall.att"]]*100, xend = 9, yend = quint2_att_miscarriage.dynamic[["overall.att"]]*100, lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att*100,2),ymin = round((att-1.96*se)*100,2), ymax = round((att+1.96*se)*100,2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Percentage-point difference in rate of miscarriage -- quint2")+
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

#Event study plot quint 3 miscarriage

quint3_att_miscarriage.dynamic.dyn <- data.frame(time=quint3_att_miscarriage.dynamic[["egt"]],
                                                 att=quint3_att_miscarriage.dynamic[["att.egt"]],
                                                 se=quint3_att_miscarriage.dynamic[["se.egt"]])

quint3_att_miscarriage.dynamic.dyn  <- quint3_att_miscarriage.dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))



ggplot(quint3_att_miscarriage.dynamic.dyn)+
  geom_vline(xintercept=-0,lty=2,color="grey59")+
  geom_hline(yintercept=0,lty=2,color="grey59")+
  
  geom_rect( aes(xmin=0,
                 xmax=9,
                 ymin=(quint3_att_miscarriage.dynamic[["overall.att"]]-1.96*quint3_att_miscarriage.dynamic[["overall.se"]])*100,
                 ymax=(quint3_att_miscarriage.dynamic[["overall.att"]]+1.96*quint3_att_miscarriage.dynamic[["overall.se"]])*100, alpha="95% CI"), fill="lightblue",
             color=NA)+
  geom_segment( 
    aes(x = 0, y = quint3_att_miscarriage.dynamic[["overall.att"]]*100, xend = 9, yend = quint3_att_miscarriage.dynamic[["overall.att"]]*100, lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att*100,2),ymin = round((att-1.96*se)*100,2), ymax = round((att+1.96*se)*100,2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Percentage-point difference in rate of miscarriage -- quint3")+
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

str(df$age)

#making age category 
df <- df %>% mutate(age_cat = case_when(age < 20 ~ 1,
                                        age > 19 & age < 25 ~ 2,
                                        age > 24 & age < 30 ~ 3,
                                        age > 29 & age < 35 ~ 4,
                                        age > 34 & age < 40 ~ 5,
                                        age > 39 & age < 45 ~ 6,
                                        age >= 45 ~ 7))

df$age_cat <- as.factor(df$age_cat)
df$age_squred <- df$age^2

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


df <- df %>% mutate(g = case_when(treat == 0 ~ 0,
                                  treat == 1 ~ 2010,
                                  treat == 2 ~ 2012, 
                                  treat == 3 ~ 2014,
                                  TRUE ~ NA_real_))



clustervars <- c(df$id, df$dist_id)
#df$treat_wealth <- df$g * df$wi_perc_rank


did_att_sb <- att_gt(yname = "sb",
                     tname = "outcome_year",
                     gname = "g",
                     idname = "id",
                     xformla = ~ 1,
                     data = df,
                     panel = FALSE,
                     allow_unbalanced_panel = TRUE,
                     weightsname = "weight_adj",
                     clustervars = c("dist_id"),
                     control_group = "notyettreated",
                     print_details = TRUE,
                     bstrap=TRUE, cband=FALSE
)

did_att_abort <- att_gt(yname = "abort",
                        tname = "outcome_year",
                        gname = "g",
                        idname = "id",
                        xformla = ~ 1,
                        data = df,
                        panel = FALSE,
                        allow_unbalanced_panel = TRUE,
                        weightsname = "weight_adj",
                        clustervars = c("dist_id"),
                        control_group = "notyettreated",
                        print_details = TRUE,
                        bstrap=TRUE, cband=FALSE
)

did_att_ms <- att_gt(yname = "miscarriage",
                     tname = "outcome_year",
                     gname = "g",
                     idname = "id",
                     xformla = ~ 1,
                     data = df,
                     panel = FALSE,
                     allow_unbalanced_panel = TRUE,
                     weightsname = "weight_adj",
                     clustervars = c("dist_id"),
                     control_group = "notyettreated",
                     print_details = TRUE,
                     bstrap=TRUE, cband=FALSE
)


ggdid(did_att_sb)
ggdid(did_att_abort)
ggdid(did_att_ms)

did_att_sb_group <- aggte(did_att_sb, type = "group")
did_att_abort_group <- aggte(did_att_abort, type = "group")
did_att_ms_group <- aggte(did_att_ms, type = "group")

did_att_sb_dynamic <- aggte(did_att_sb, type = "dynamic")
did_att_abort_dynamic <- aggte(did_att_abort, type = "dynamic")
did_att_ms_dynamic <- aggte(did_att_ms, type = "dynamic")

ggdid(did_att_sb_dynamic)

#make into one plot
did.es <- aggte(did_check_att_sb, type="dynamic")
ggdid(did.es)

ggdid(quint1_abort_group)

did_sb_group_tidy <- tidy(did_att_sb_group)
did_sb_group_tidy$outcome <- c("Stillbirth")
did_sb_group_tidy$est_per1000 <- did_sb_group_tidy$estimate*1000
did_sb_group_tidy$stderr_per1000 <- did_sb_group_tidy$std.error*1000
did_sb_group_tidy$conflow_per1000 <- did_sb_group_tidy$conf.low*1000
did_sb_group_tidy$confhigh_per1000 <- did_sb_group_tidy$conf.high*1000

did_abort_group_tidy <- tidy(did_att_abort_group)
did_abort_group_tidy$outcome <- c("Abortion")
did_abort_group_tidy$est_per1000 <- did_abort_group_tidy$estimate*1000
did_abort_group_tidy$stderr_per1000 <- did_abort_group_tidy$std.error*1000
did_abort_group_tidy$conflow_per1000 <- did_abort_group_tidy$conf.low*1000
did_abort_group_tidy$confhigh_per1000 <- did_abort_group_tidy$conf.high*1000

did_ms_group_tidy <- tidy(did_att_ms_group)
did_ms_group_tidy$outcome <- c("Miscarriage")
did_ms_group_tidy$est_per1000 <- did_ms_group_tidy$estimate*1000
did_ms_group_tidy$stderr_per1000 <- did_ms_group_tidy$std.error*1000
did_ms_group_tidy$conflow_per1000 <- did_ms_group_tidy$conf.low*1000
did_ms_group_tidy$confhigh_per1000 <- did_ms_group_tidy$conf.high*1000



#filtering out only average overall
did_sb_group_tidy_overall <- did_sb_group_tidy %>% filter(group == "Average")

did_abort_group_tidy_overall <- did_abort_group_tidy %>% filter(group == "Average")

did_ms_group_tidy_overall <- did_ms_group_tidy %>% filter(group == "Average")



did_group_overall <- rbind(did_sb_group_tidy_overall, did_abort_group_tidy_overall, did_ms_group_tidy_overall)

mycolors_outcomes <- c("#024b7a", "#e67e00", "#44b7c2")

did_overall_plot <- ggplot(data = did_group_overall, mapping = aes(x = outcome, y = (estimate*1000)#, 
                                                                   #color = outcome
)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin = (conf.low*1000), ymax = (conf.high*1000)), width = 0.1, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, color = I("black"), linetype = 2)+
  scale_x_discrete(breaks = c("Stillbirth", "Abortion", "Miscarriage"),labels = c("Stillbirth", "Abortion", "Miscarriage"))+
  #scale_color_manual(values = mycolors_outcomes, name = "Outcome", breaks = c("Stillbirth", "Abortion", "Miscarriage"))+
  #ylim(-8, 8)+
  scale_y_continuous(limits = c(-50, 50), n.breaks = 10)+
  ylab("Overall ATT per 1,000 pregnancies")+
  xlab("Adverse Pregnancy Outcome") +
  theme_cowplot()


# Stratified DiD ----------------------------------------------------------

###### wealth stratificatoin ########

#subsetting into above / below the median

medianwealth <-  median(df$wi_perc_rank)

wealth_above_median <- df %>% filter(wi_perc_rank >= medianwealth)
wealth_below_median <- df %>% filter(wi_perc_rank < medianwealth)


did_att_sb_abovemed <- att_gt(yname = "sb",
                              tname = "outcome_year",
                              gname = "g",
                              idname = "id",
                              xformla = ~ 1,
                              data = wealth_above_median,
                              panel = FALSE,
                              allow_unbalanced_panel = TRUE,
                              weightsname = "weight_adj",
                              clustervars = c("dist_id"),
                              control_group = "notyettreated",
                              print_details = TRUE,
                              bstrap=TRUE, cband=FALSE
)

did_att_abort_abovemed <- att_gt(yname = "abort",
                                 tname = "outcome_year",
                                 gname = "g",
                                 idname = "id",
                                 xformla = ~ 1,
                                 data = wealth_above_median,
                                 panel = FALSE,
                                 allow_unbalanced_panel = TRUE,
                                 weightsname = "weight_adj",
                                 clustervars = c("dist_id"),
                                 control_group = "notyettreated",
                                 print_details = TRUE,
                                 bstrap=TRUE, cband=FALSE
)

did_att_ms_abovemed <- att_gt(yname = "miscarriage",
                              tname = "outcome_year",
                              gname = "g",
                              idname = "id",
                              xformla = ~ 1,
                              data = wealth_above_median,
                              panel = FALSE,
                              allow_unbalanced_panel = TRUE,
                              weightsname = "weight_adj",
                              clustervars = c("dist_id"),
                              control_group = "notyettreated",
                              print_details = TRUE,
                              bstrap=TRUE, cband=FALSE
)

did_att_sb_belowmed <- att_gt(yname = "sb",
                              tname = "outcome_year",
                              gname = "g",
                              idname = "id",
                              xformla = ~ 1,
                              data = wealth_below_median,
                              panel = FALSE,
                              allow_unbalanced_panel = TRUE,
                              weightsname = "weight_adj",
                              clustervars = c("dist_id"),
                              control_group = "notyettreated",
                              print_details = TRUE,
                              bstrap=TRUE, cband=FALSE
)

did_att_abort_belowmed <- att_gt(yname = "abort",
                                 tname = "outcome_year",
                                 gname = "g",
                                 idname = "id",
                                 xformla = ~ 1,
                                 data = wealth_below_median,
                                 panel = FALSE,
                                 allow_unbalanced_panel = TRUE,
                                 weightsname = "weight_adj",
                                 clustervars = c("dist_id"),
                                 control_group = "notyettreated",
                                 print_details = TRUE,
                                 bstrap=TRUE, cband=FALSE
)

did_att_ms_belowmed <- att_gt(yname = "miscarriage",
                              tname = "outcome_year",
                              gname = "g",
                              idname = "id",
                              xformla = ~ 1,
                              data = wealth_below_median,
                              panel = FALSE,
                              allow_unbalanced_panel = TRUE,
                              weightsname = "weight_adj",
                              clustervars = c("dist_id"),
                              control_group = "notyettreated",
                              print_details = TRUE,
                              bstrap=TRUE, cband=FALSE
)


did_att_sb_abovemed_group <- aggte(didatta)



#dynamic effects
abovemed_sb_dyn <- aggte(did_att_sb_abovemed, type = "dynamic")
abovemed_abort_dyn <- aggte(did_att_abort_abovemed, type = "dynamic")
abovemed_ms_dyn <- aggte(did_att_ms_abovemed, type = "dynamic")

#now add in abovemed group estimates
abovemed_abort_group <- aggte(did_att_abort_abovemed, type = "group")
abovemed_abort_group_tidy <- tidy(abovemed_abort_group)
abovemed_abort_group_tidy$outcome <- c("Abortion")
abovemed_abort_group_tidy$est_per1000 <- abovemed_abort_group_tidy$estimate*1000
abovemed_abort_group_tidy$stderr_per1000 <- abovemed_abort_group_tidy$std.error*1000
abovemed_abort_group_tidy$conflow_per1000 <- abovemed_abort_group_tidy$conf.low*1000
abovemed_abort_group_tidy$confhigh_per1000 <- abovemed_abort_group_tidy$conf.high*1000



abovemed_sb_group <- aggte(did_att_sb_abovemed, type = "group")

abovemed_sb_group_tidy <- tidy(abovemed_sb_group)
abovemed_sb_group_tidy$outcome <- c("Stillbirth")
abovemed_sb_group_tidy$est_per1000 <- abovemed_sb_group_tidy$estimate*1000
abovemed_sb_group_tidy$stderr_per1000 <- abovemed_sb_group_tidy$std.error*1000
abovemed_sb_group_tidy$conflow_per1000 <- abovemed_sb_group_tidy$conf.low*1000
abovemed_sb_group_tidy$confhigh_per1000 <- abovemed_sb_group_tidy$conf.high*1000



abovemed_miscarriage_group <- aggte(did_att_ms_abovemed, type = "group")

abovemed_miscarriage_group_tidy <- tidy(abovemed_miscarriage_group)
abovemed_miscarriage_group_tidy$outcome <- c("Miscarriage")
abovemed_miscarriage_group_tidy$est_per1000 <- abovemed_miscarriage_group_tidy$estimate*1000
abovemed_miscarriage_group_tidy$stderr_per1000 <- abovemed_miscarriage_group_tidy$std.error*1000
abovemed_miscarriage_group_tidy$conflow_per1000 <- abovemed_miscarriage_group_tidy$conf.low*1000
abovemed_miscarriage_group_tidy$confhigh_per1000 <- abovemed_miscarriage_group_tidy$conf.high*1000


abovemed_group_atts <- rbind(abovemed_sb_group_tidy, abovemed_abort_group_tidy, abovemed_miscarriage_group_tidy)

#filtering out only average overall
abovemed_group_atts_overall <- abovemed_group_atts %>% filter(group == "Average")


#now add in belowmed group estimates
belowmed_abort_group <- aggte(did_att_abort_belowmed, type = "group")
belowmed_abort_group_tidy <- tidy(belowmed_abort_group)
belowmed_abort_group_tidy$outcome <- c("Abortion")
belowmed_abort_group_tidy$est_per1000 <- belowmed_abort_group_tidy$estimate*1000
belowmed_abort_group_tidy$stderr_per1000 <- belowmed_abort_group_tidy$std.error*1000
belowmed_abort_group_tidy$conflow_per1000 <- belowmed_abort_group_tidy$conf.low*1000
belowmed_abort_group_tidy$confhigh_per1000 <- belowmed_abort_group_tidy$conf.high*1000



belowmed_sb_group <- aggte(did_att_sb_belowmed, type = "group")

belowmed_sb_group_tidy <- tidy(belowmed_sb_group)
belowmed_sb_group_tidy$outcome <- c("Stillbirth")
belowmed_sb_group_tidy$est_per1000 <- belowmed_sb_group_tidy$estimate*1000
belowmed_sb_group_tidy$stderr_per1000 <- belowmed_sb_group_tidy$std.error*1000
belowmed_sb_group_tidy$conflow_per1000 <- belowmed_sb_group_tidy$conf.low*1000
belowmed_sb_group_tidy$confhigh_per1000 <- belowmed_sb_group_tidy$conf.high*1000



belowmed_miscarriage_group <- aggte(did_att_ms_belowmed, type = "group")

belowmed_miscarriage_group_tidy <- tidy(belowmed_miscarriage_group)
belowmed_miscarriage_group_tidy$outcome <- c("Miscarriage")
belowmed_miscarriage_group_tidy$est_per1000 <- belowmed_miscarriage_group_tidy$estimate*1000
belowmed_miscarriage_group_tidy$stderr_per1000 <- belowmed_miscarriage_group_tidy$std.error*1000
belowmed_miscarriage_group_tidy$conflow_per1000 <- belowmed_miscarriage_group_tidy$conf.low*1000
belowmed_miscarriage_group_tidy$confhigh_per1000 <- belowmed_miscarriage_group_tidy$conf.high*1000


belowmed_group_atts <- rbind(belowmed_sb_group_tidy, belowmed_abort_group_tidy, belowmed_miscarriage_group_tidy)

#filtering out only average overall
belowmed_group_atts_overall <- belowmed_group_atts %>% filter(group == "Average")

#combining belowmed and sc atts
abovemed_group_atts_overall$strata <- c("≥ Median Wealth Index")
belowmed_group_atts_overall$strata <- c("< Median Wealth Index")

abovemed_belowmed_atts_overall <- rbind(abovemed_group_atts_overall, belowmed_group_atts_overall)


#now combining into plot
abovemed_belowmed_atts_group_plot <- ggplot(data = abovemed_belowmed_atts_overall, 
                                            mapping = aes(x = outcome, y = (estimate*1000), color = strata)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin = (conf.low*1000), ymax = (conf.high*1000)), width = 0.1, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, color = I("black"), linetype = 2)+
  #ylim(-8, 8)+
  scale_y_continuous(limits = c(-150, 150), n.breaks = 10)+
  #scale_color_manual(values = mycolors_outcomes, name = "Outcome", breaks = c("Stillbirth", "Abortion", "Miscarriage"))+
  ylab("Overall ATT per 1,000 pregnancies")+
  xlab("Adverse Pregnancy Outcome") +
  theme_cowplot()


######### Rural / urban stratification ##############
table(df$rural_urban)

rural <- df %>% filter(rural_urban == 0)
urban <- df %>% filter(rural_urban == 1)

rural_sb_att <- att_gt(yname = "sb",
                       tname = "outcome_year",
                       gname = "g",
                       idname = "id",
                       xformla = ~ 1,
                       data = rural,
                       panel = FALSE,
                       allow_unbalanced_panel = TRUE,
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
                          xformla = ~1,
                          data = rural,
                          panel = FALSE,
                          allow_unbalanced_panel = TRUE,
                          weightsname = "weight_adj",
                          control_group = "notyettreated",
                          clustervars = c("dist_id"),
                          print_details = TRUE,
                          #est_method = "reg",
                          bstrap=TRUE, cband=FALSE
)

rural_att_miscarriage <- att_gt(yname = "miscarriage",
                                tname = "outcome_year",
                                gname = "g",
                                idname = "id",
                                xformla = ~1,
                                data = rural,
                                panel = FALSE,
                                allow_unbalanced_panel = TRUE,
                                weightsname = "weight_adj",
                                control_group = "notyettreated",
                                clustervars = c("dist_id"),
                                print_details = TRUE,
                                #est_method = "reg",
                                bstrap=TRUE, cband=FALSE
)

urban_sb_att <- att_gt(yname = "sb",
                       tname = "outcome_year",
                       gname = "g",
                       idname = "id",
                       xformla = ~1,
                       data = urban,
                       panel = FALSE,
                       allow_unbalanced_panel = TRUE,
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
                          xformla = ~1,
                          data = urban,
                          panel = FALSE,
                          allow_unbalanced_panel = TRUE,
                          weightsname = "weight_adj",
                          control_group = "notyettreated",
                          clustervars = c("dist_id"),
                          print_details = TRUE,
                          #est_method = "reg",
                          bstrap=TRUE, cband=FALSE
)

urban_att_miscarriage <- att_gt(yname = "miscarriage",
                                tname = "outcome_year",
                                gname = "g",
                                idname = "id",
                                xformla = ~1,
                                data = urban,
                                panel = FALSE,
                                allow_unbalanced_panel = TRUE,
                                weightsname = "weight_adj",
                                control_group = "notyettreated",
                                clustervars = c("dist_id"),
                                print_details = TRUE,
                                #est_method = "reg",
                                bstrap=TRUE, cband=FALSE
)


aggte(rural_sb_att, type = "dynamic")
ggdid(rural_sb_att)

#### RURAL ABORTION DYNAMIC EFFECTS AND PLOTS

rural_atta_abort_dynamic <- aggte(rural_att_abort, type = "dynamic")

#rural_abot_dynamic_plot <- ggdid(rural_atta_abort_dynamic)

rural_atta_abort_calendar <- aggte(rural_att_abort, type = "calendar")

#rural_abot_calendar_plot <- ggdid(rural_atta_abort_calendar)

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
                 ymin=(rural_atta_abort_dynamic[["overall.att"]]-1.96*rural_atta_abort_dynamic[["overall.se"]])*100,
                 ymax=(rural_atta_abort_dynamic[["overall.att"]]+1.96*rural_atta_abort_dynamic[["overall.se"]])*100, alpha="95% CI"), fill="lightblue",
             color=NA)+
  geom_segment( 
    aes(x = 0, y = rural_atta_abort_dynamic[["overall.att"]]*100, xend = 9, yend = rural_atta_abort_dynamic[["overall.att"]]*100, lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att*100,2),ymin = round((att-1.96*se)*100,2), ymax = round((att+1.96*se)*100,2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Percentage-point difference in rate of abortions -- RURAL")+
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
                 ymin=(rural_att_miscarriage_dynamic[["overall.att"]]-1.96*rural_att_miscarriage_dynamic[["overall.se"]])*100,
                 ymax=(rural_att_miscarriage_dynamic[["overall.att"]]+1.96*rural_att_miscarriage_dynamic[["overall.se"]])*100, alpha="95% CI"), fill="lightblue",
             color=NA)+
  geom_segment( 
    aes(x = 0, y = rural_att_miscarriage_dynamic[["overall.att"]]*100, xend = 9, yend = rural_att_miscarriage_dynamic[["overall.att"]]*100, lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att*100,2),ymin = round((att-1.96*se)*100,2), ymax = round((att+1.96*se)*100,2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Percentage-point difference in rate of miscarriages -- RURAL")+
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


###### combining rural group estimates into one plot
#rural_abort_group <- aggte(rural_att_abort, type = "group")
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

#filtering out only average overall
rural_group_atts_overall <- rural_group_atts %>% filter(group == "Average")

library(cowplot)
rural_atts_group_plot <- ggplot(data = rural_group_atts_overall, mapping = aes(x = outcome, y = (estimate*100)#, 
                                                                               #color = outcome
)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin = (conf.low*100), ymax = (conf.high*100)), width = 0.1, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, color = I("black"), linetype = 2)+
  #ylim(-8, 8)+
  scale_y_continuous(limits = c(-7, 7), breaks = c(-6, -4,-2,0,2, 4,6))+
  scale_color_manual(values = mycolors_outcomes, name = "Outcome", breaks = c("Stillbirth", "Abortion", "Miscarriage"))+
  ylab("Overall ATT -- Rural (Percentage-Points)")+
  xlab("Adverse Pregnancy Outcome") +
  theme_cowplot()

#now add in urban group estimates
urban_abort_group <- aggte(urban_att_abort, type = "group")
urban_abort_group_tidy <- tidy(urban_abort_group)
urban_abort_group_tidy$outcome <- c("Abortion")
urban_abort_group_tidy$est_per1000 <- urban_abort_group_tidy$estimate*1000
urban_abort_group_tidy$stderr_per1000 <- urban_abort_group_tidy$std.error*1000
urban_abort_group_tidy$conflow_per1000 <- urban_abort_group_tidy$conf.low*1000
urban_abort_group_tidy$confhigh_per1000 <- urban_abort_group_tidy$conf.high*1000



urban_sb_group <- aggte(urban_sb_att, type = "group")

urban_sb_group_tidy <- tidy(urban_sb_group)
urban_sb_group_tidy$outcome <- c("Stillbirth")
urban_sb_group_tidy$est_per1000 <- urban_sb_group_tidy$estimate*1000
urban_sb_group_tidy$stderr_per1000 <- urban_sb_group_tidy$std.error*1000
urban_sb_group_tidy$conflow_per1000 <- urban_sb_group_tidy$conf.low*1000
urban_sb_group_tidy$confhigh_per1000 <- urban_sb_group_tidy$conf.high*1000



urban_miscarriage_group <- aggte(urban_att_miscarriage, type = "group")

urban_miscarriage_group_tidy <- tidy(urban_miscarriage_group)
urban_miscarriage_group_tidy$outcome <- c("Miscarriage")
urban_miscarriage_group_tidy$est_per1000 <- urban_miscarriage_group_tidy$estimate*1000
urban_miscarriage_group_tidy$stderr_per1000 <- urban_miscarriage_group_tidy$std.error*1000
urban_miscarriage_group_tidy$conflow_per1000 <- urban_miscarriage_group_tidy$conf.low*1000
urban_miscarriage_group_tidy$confhigh_per1000 <- urban_miscarriage_group_tidy$conf.high*1000


urban_group_atts <- rbind(urban_sb_group_tidy, urban_abort_group_tidy, urban_miscarriage_group_tidy)

#filtering out only average overall
urban_group_atts_overall <- urban_group_atts %>% filter(group == "Average")

#combining urban and rural atts
rural_group_atts_overall$strata <- c("Rural")
urban_group_atts_overall$strata <- c("Urban")

rural_urban_atts_overall <- rbind(rural_group_atts_overall, urban_group_atts_overall)


#now combining into plot
rural_urban_atts_group_plot <- ggplot(data = rural_urban_atts_overall, mapping = aes(x = outcome, y = (estimate*1000), color = strata)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin = (conf.low*1000), ymax = (conf.high*1000)), width = 0.1, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, color = I("black"), linetype = 2)+
  #ylim(-8, 8)+
  scale_y_continuous(limits = c(-50, 50), n.breaks = 10)+
  #scale_color_manual(values = mycolors_outcomes, name = "Outcome", breaks = c("Stillbirth", "Abortion", "Miscarriage"))+
  ylab("Overall ATT per 1,000 pregnancies")+
  xlab("Adverse Pregnancy Outcome") +
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


#### urban ABORTION DYNAMIC EFFECTS AND PLOTS

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



######## urban MISCARRIAGE EVENT STUDY PLOTS

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


######## Primary school stratification ##############
table(df$primary_school)

primary <- df %>% filter(primary_school == 1)
nonprimary <- df %>% filter(primary_school == 0)

primary_sb_att <- att_gt(yname = "sb",
                         tname = "outcome_year",
                         gname = "g",
                         idname = "id",
                         xformla = ~1,
                         data = primary,
                         panel = FALSE,
                         allow_unbalanced_panel = TRUE,
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
                            xformla = ~1,
                            data = primary,
                            panel = FALSE,
                            allow_unbalanced_panel = TRUE,
                            weightsname = "weight_adj",
                            clustervars = c("dist_id"),
                            control_group = "notyettreated",
                            print_details = TRUE,
                            #est_method = "reg",
                            bstrap=TRUE, cband=FALSE
)

primary_att_miscarriage <- att_gt(yname = "miscarriage",
                                  tname = "outcome_year",
                                  gname = "g",
                                  idname = "id",
                                  xformla = ~1,
                                  data = primary,
                                  panel = FALSE,
                                  allow_unbalanced_panel = TRUE,
                                  weightsname = "weight_adj",
                                  clustervars = c("dist_id"),
                                  control_group = "notyettreated",
                                  print_details = TRUE,
                                  #est_method = "reg",
                                  bstrap=TRUE, cband=FALSE
)


nonprimary_sb_att <- att_gt(yname = "sb",
                            tname = "outcome_year",
                            gname = "g",
                            idname = "id",
                            xformla = ~1,
                            data = nonprimary,
                            panel = FALSE,
                            allow_unbalanced_panel = TRUE,
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
                               xformla = ~1,
                               data = nonprimary,
                               panel = FALSE,
                               allow_unbalanced_panel = TRUE,
                               weightsname = "weight_adj",
                               clustervars = c("dist_id"),
                               control_group = "notyettreated",
                               print_details = TRUE,
                               #est_method = "reg",
                               bstrap=TRUE, cband=FALSE
)

nonprimary_att_miscarriage <- att_gt(yname = "miscarriage",
                                     tname = "outcome_year",
                                     gname = "g",
                                     idname = "id",
                                     xformla = ~1,
                                     data = nonprimary,
                                     panel = FALSE,
                                     allow_unbalanced_panel = TRUE,
                                     weightsname = "weight_adj",
                                     clustervars = c("dist_id"),
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
primary_abort_group_plot <- ggdid(primary_att_group_abort)

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


###### combining nonprimary group estimates into one plot
nonprimary_abort_group <- aggte(nonprimary_att_abort, type = "group")
ggdid(nonprimary_abort_group)

nonprimary_abort_group_tidy <- tidy(nonprimary_abort_group)
nonprimary_abort_group_tidy$outcome <- c("Abortion")
nonprimary_abort_group_tidy$est_per1000 <- nonprimary_abort_group_tidy$estimate*1000
nonprimary_abort_group_tidy$stderr_per1000 <- nonprimary_abort_group_tidy$std.error*1000
nonprimary_abort_group_tidy$conflow_per1000 <- nonprimary_abort_group_tidy$conf.low*1000
nonprimary_abort_group_tidy$confhigh_per1000 <- nonprimary_abort_group_tidy$conf.high*1000



nonprimary_sb_group <- aggte(nonprimary_sb_att, type = "group")

nonprimary_sb_group_tidy <- tidy(nonprimary_sb_group)
nonprimary_sb_group_tidy$outcome <- c("Stillbirth")
nonprimary_sb_group_tidy$est_per1000 <- nonprimary_sb_group_tidy$estimate*1000
nonprimary_sb_group_tidy$stderr_per1000 <- nonprimary_sb_group_tidy$std.error*1000
nonprimary_sb_group_tidy$conflow_per1000 <- nonprimary_sb_group_tidy$conf.low*1000
nonprimary_sb_group_tidy$confhigh_per1000 <- nonprimary_sb_group_tidy$conf.high*1000



nonprimary_miscarriage_group <- aggte(nonprimary_att_miscarriage, type = "group")

nonprimary_miscarriage_group_tidy <- tidy(nonprimary_miscarriage_group)
nonprimary_miscarriage_group_tidy$outcome <- c("Miscarriage")
nonprimary_miscarriage_group_tidy$est_per1000 <- nonprimary_miscarriage_group_tidy$estimate*1000
nonprimary_miscarriage_group_tidy$stderr_per1000 <- nonprimary_miscarriage_group_tidy$std.error*1000
nonprimary_miscarriage_group_tidy$conflow_per1000 <- nonprimary_miscarriage_group_tidy$conf.low*1000
nonprimary_miscarriage_group_tidy$confhigh_per1000 <- nonprimary_miscarriage_group_tidy$conf.high*1000

nonprimary_group_atts <- rbind(nonprimary_sb_group_tidy, nonprimary_abort_group_tidy, nonprimary_miscarriage_group_tidy)

#filtering out only average overall
nonprimary_group_atts_overall <- nonprimary_group_atts %>% filter(group == "Average")

library(cowplot)
nonprimary_atts_group_plot <- ggplot(data = nonprimary_group_atts_overall, mapping = aes(x = outcome, y = (estimate*100)#, 
                                                                                         #color = outcome
)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin = (conf.low*100), ymax = (conf.high*100)), width = 0.1, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, color = I("black"), linetype = 2)+
  #ylim(-8, 8)+
  scale_y_continuous(limits = c(-7, 7), breaks = c(-6, -4,-2,0,2, 4,6))+
  scale_color_manual(values = mycolors_outcomes, name = "Outcome", breaks = c("Stillbirth", "Abortion", "Miscarriage"))+
  ylab("Overall ATT -- nonprimary (Percentage-Points)")+
  xlab("Adverse Pregnancy Outcome") +
  theme_cowplot()

#now add in primary group estimates
primary_abort_group <- aggte(primary_att_abort, type = "group")
primary_abort_group_tidy <- tidy(primary_abort_group)
primary_abort_group_tidy$outcome <- c("Abortion")
primary_abort_group_tidy$est_per1000 <- primary_abort_group_tidy$estimate*1000
primary_abort_group_tidy$stderr_per1000 <- primary_abort_group_tidy$std.error*1000
primary_abort_group_tidy$conflow_per1000 <- primary_abort_group_tidy$conf.low*1000
primary_abort_group_tidy$confhigh_per1000 <- primary_abort_group_tidy$conf.high*1000



primary_sb_group <- aggte(primary_sb_att, type = "group")

primary_sb_group_tidy <- tidy(primary_sb_group)
primary_sb_group_tidy$outcome <- c("Stillbirth")
primary_sb_group_tidy$est_per1000 <- primary_sb_group_tidy$estimate*1000
primary_sb_group_tidy$stderr_per1000 <- primary_sb_group_tidy$std.error*1000
primary_sb_group_tidy$conflow_per1000 <- primary_sb_group_tidy$conf.low*1000
primary_sb_group_tidy$confhigh_per1000 <- primary_sb_group_tidy$conf.high*1000



primary_miscarriage_group <- aggte(primary_att_miscarriage, type = "group")

primary_miscarriage_group_tidy <- tidy(primary_miscarriage_group)
primary_miscarriage_group_tidy$outcome <- c("Miscarriage")
primary_miscarriage_group_tidy$est_per1000 <- primary_miscarriage_group_tidy$estimate*1000
primary_miscarriage_group_tidy$stderr_per1000 <- primary_miscarriage_group_tidy$std.error*1000
primary_miscarriage_group_tidy$conflow_per1000 <- primary_miscarriage_group_tidy$conf.low*1000
primary_miscarriage_group_tidy$confhigh_per1000 <- primary_miscarriage_group_tidy$conf.high*1000


primary_group_atts <- rbind(primary_sb_group_tidy, primary_abort_group_tidy, primary_miscarriage_group_tidy)

#filtering out only average overall
primary_group_atts_overall <- primary_group_atts %>% filter(group == "Average")

#combining primary and nonprimary atts
nonprimary_group_atts_overall$strata <- c("nonprimary")
primary_group_atts_overall$strata <- c("primary")

nonprimary_primary_atts_overall <- rbind(nonprimary_group_atts_overall, primary_group_atts_overall)


#now combining into plot
nonprimary_primary_atts_group_plot <- ggplot(data = nonprimary_primary_atts_overall, 
                                             mapping = aes(x = outcome, y = (estimate*1000), color = strata)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin = (conf.low*1000), ymax = (conf.high*1000)), width = 0.1, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, color = I("black"), linetype = 2)+
  #ylim(-8, 8)+
  scale_y_continuous(limits = c(-70, 70), n.breaks = 10)+
  #scale_color_manual(values = strata, name = "Completed Primary School", breaks = c("No", "Yes"))+
  ylab("Overall ATT per 1,000 pregnancies")+
  xlab("Adverse Pregnancy Outcome") +
  theme_cowplot()

#event study plot

nonprimary_att_abort_dynamic.dyn <- data.frame(time=nonprimary_att_abort_dynamic[["egt"]],
                                               att=nonprimary_att_abort_dynamic[["att.egt"]],
                                               se=nonprimary_att_abort_dynamic[["se.egt"]])

nonprimary_att_abort_dynamic.dyn  <- nonprimary_att_abort_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))

ggplot(nonprimary_att_abort_dynamic.dyn)+
  geom_vline(xintercept=-0,lty=2,color="grey59")+
  geom_hline(yintercept=0,lty=2,color="grey59")+
  
  geom_rect( aes(xmin=0,
                 xmax=9,
                 ymin=(nonprimary_att_abort_dynamic[["overall.att"]]-1.96*nonprimary_att_abort_dynamic[["overall.se"]])*100,
                 ymax=(nonprimary_att_abort_dynamic[["overall.att"]]+1.96*nonprimary_att_abort_dynamic[["overall.se"]])*100, alpha="95% CI"), fill="lightblue",
             color=NA)+
  geom_segment( 
    aes(x = 0, y = nonprimary_att_abort_dynamic[["overall.att"]]*100, xend = 9, yend = nonprimary_att_abort_dynamic[["overall.att"]]*100, lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att*100,2),ymin = round((att-1.96*se)*100,2), ymax = round((att+1.96*se)*100,2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Percentage-point difference in rate of abortions -- nonprimary")+
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




#nonprimary calendar plot sb & ms

nonprimary_att_sb_calendar.cal <- data.frame(time=nonprimary_att_sb_calendar[["egt"]],
                                             att=nonprimary_att_sb_calendar[["att.egt"]],
                                             se=nonprimary_att_sb_calendar[["se.egt"]])

#nonprimary_att_sb_calendar.cal  <- nonprimary_att_sb_calendar.cal  %>% mutate(post=ifelse(time>=0,1,0))


ggplot(nonprimary_att_sb_calendar.cal)+
  #geom_vline(xintercept=-0,lty=2,color="grey59")+
  geom_hline(yintercept=0,lty=2,color="grey59")+
  
  geom_rect( aes(xmin=2010,
                 xmax=2019,
                 ymin=(nonprimary_att_sb_calendar[["overall.att"]]-1.96*nonprimary_att_sb_calendar[["overall.se"]])*1000,
                 ymax=(nonprimary_att_sb_calendar[["overall.att"]]+1.96*nonprimary_att_sb_calendar[["overall.se"]])*1000, 
                 alpha="95% CI"), fill="lightblue",
             color=NA)+
  geom_segment( 
    aes(x = 2010, y = nonprimary_att_sb_calendar[["overall.att"]]*1000, xend = 2019, 
        yend = nonprimary_att_sb_calendar[["overall.att"]]*1000, lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att*1000,2),ymin = round((att-1.96*se)*1000,2), ymax = round((att+1.96*se)*1000,2)
                      #,
                      #color=as.factor(post)
  ), 
  size=1,fatten = 1)+
  
  ylab("ATT Stillbirth per 1,000 pregnancies among women without a primary education")+
  xlab("Year")+
  scale_x_continuous(breaks=c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),
                     labels = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))+
  ylim(-100, 100)+
  
  #scale_color_manual(values=c("red","blue2"),labels=c("Pre Intervention","Post Intervention"))+
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
  #ggtitle(str_wrap("ATT Calendar Plot",
  #                width=80))
  theme_cowplot()


nonprimary_att_ms_calendar.cal <- data.frame(time=nonprimary_att_ms_calendar[["egt"]],
                                             att=nonprimary_att_ms_calendar[["att.egt"]],
                                             se=nonprimary_att_ms_calendar[["se.egt"]])

#nonprimary_att_ms_calendar.cal  <- nonprimary_att_ms_calendar.cal  %>% mutate(post=ifelse(time>=0,1,0))


ggplot(nonprimary_att_ms_calendar.cal)+
  #geom_vline(xintercept=-0,lty=2,color="grey59")+
  geom_hline(yintercept=0,lty=2,color="grey59")+
  
  geom_rect( aes(xmin=2010,
                 xmax=2019,
                 ymin=(nonprimary_att_ms_calendar[["overall.att"]]-1.96*nonprimary_att_ms_calendar[["overall.se"]])*1000,
                 ymax=(nonprimary_att_ms_calendar[["overall.att"]]+1.96*nonprimary_att_ms_calendar[["overall.se"]])*1000, 
                 alpha="95% CI"), fill="lightblue",
             color=NA)+
  geom_segment( 
    aes(x = 2010, y = nonprimary_att_ms_calendar[["overall.att"]]*1000, xend = 2019, 
        yend = nonprimary_att_ms_calendar[["overall.att"]]*1000, lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att*1000,2),ymin = round((att-1.96*se)*1000,2), ymax = round((att+1.96*se)*1000,2)
                      #,
                      #color=as.factor(post)
  ), 
  size=1,fatten = 1)+
  
  ylab("ATT Stillbirth per 1,000 pregnancies among women without a primary education")+
  xlab("Year")+
  scale_x_continuous(breaks=c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),
                     labels = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))+
  ylim(-100, 100)+
  
  #scale_color_manual(values=c("red","blue2"),labels=c("Pre Intervention","Post Intervention"))+
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
  #ggtitle(str_wrap("ATT Calendar Plot",
  #                width=80))
  theme_cowplot()




####### Scheduled Caste / Scheduled Tribe stratification #########


table(df$caste_group)

st <- df %>% filter(caste_group == 2)
sc <- df %>% filter(caste_group == 1)

st_sb_att <- att_gt(yname = "sb",
                    tname = "outcome_year",
                    gname = "g",
                    idname = "id",
                    xformla = ~1,
                    data = st,
                    panel = FALSE,
                    allow_unbalanced_panel = TRUE,
                    weightsname = "weight_adj",
                    control_group = "notyettreated",
                    clustervars = c("dist_id"),
                    print_details = TRUE,
                    #est_method = "reg",
                    bstrap=TRUE, cband=FALSE
)



st_att_abort <- att_gt(yname = "abort",
                       tname = "outcome_year",
                       gname = "g",
                       idname = "id",
                       xformla = ~1,
                       data = st,
                       panel = FALSE,
                       allow_unbalanced_panel = TRUE,
                       weightsname = "weight_adj",
                       clustervars = c("dist_id"),
                       control_group = "notyettreated",
                       print_details = TRUE,
                       #est_method = "reg",
                       bstrap=TRUE, cband=FALSE
)

st_att_miscarriage <- att_gt(yname = "miscarriage",
                             tname = "outcome_year",
                             gname = "g",
                             idname = "id",
                             xformla = ~1,
                             data = st,
                             panel = FALSE,
                             allow_unbalanced_panel = TRUE,
                             weightsname = "weight_adj",
                             clustervars = c("dist_id"),
                             control_group = "notyettreated",
                             print_details = TRUE,
                             #est_method = "reg",
                             bstrap=TRUE, cband=FALSE
)


sc_sb_att <- att_gt(yname = "sb",
                    tname = "outcome_year",
                    gname = "g",
                    idname = "id",
                    xformla = ~1,
                    data = sc,
                    panel = FALSE,
                    allow_unbalanced_panel = TRUE,
                    weightsname = "weight_adj",
                    control_group = "notyettreated",
                    clustervars = c("dist_id"),
                    print_details = TRUE,
                    #est_method = "reg",
                    bstrap=TRUE, cband=FALSE
)



sc_att_abort <- att_gt(yname = "abort",
                       tname = "outcome_year",
                       gname = "g",
                       idname = "id",
                       xformla = ~1,
                       data = sc,
                       panel = FALSE,
                       allow_unbalanced_panel = TRUE,
                       weightsname = "weight_adj",
                       clustervars = c("dist_id"),
                       control_group = "notyettreated",
                       print_details = TRUE,
                       #est_method = "reg",
                       bstrap=TRUE, cband=FALSE
)

sc_att_miscarriage <- att_gt(yname = "miscarriage",
                             tname = "outcome_year",
                             gname = "g",
                             idname = "id",
                             xformla = ~1,
                             data = sc,
                             panel = FALSE,
                             allow_unbalanced_panel = TRUE,
                             weightsname = "weight_adj",
                             clustervars = c("dist_id"),
                             control_group = "notyettreated",
                             print_details = TRUE,
                             #est_method = "reg",
                             bstrap=TRUE, cband=FALSE
)


st_att_abort_dynamic <- aggte(st_att_abort, type = "dynamic")

st_abort_dynamic_plot <- ggdid(st_att_abort_dynamic)

st_att_abort_calendar <- aggte(st_att_abort, type = "calendar")

st_abort_calendar_plot <- ggdid(st_att_abort_calendar)

st_att_group_abort <- aggte(st_att_abort, type = "group")
st_abort_group_plot <- ggdid(st_att_group_abprt)

st_att_sb_dynamic <- aggte(st_sb_att, type = "dynamic")

st_sb_dynamic_plot <- ggdid(st_att_sb_dynamic)

st_att_sb_calendar <- aggte(st_sb_att, type = "calendar")

st_sb_calendar_plot <- ggdid(st_att_sb_calendar)

st_att_group_sb <- aggte(st_sb_att, type = "group")
st_sb_group_plot <- ggdid(st_att_group_sb)

st_att_miscarriage_dynamic <- aggte(st_att_miscarriage, type = "dynamic")

st_miscarriage_dynamic_plot <- ggdid(st_att_miscarriage_dynamic)

st_att_miscarriage_calendar <- aggte(st_att_miscarriage, type = "calendar")

st_miscarriage_calendar_plot <- ggdid(st_att_miscarriage_calendar)

st_att_group_miscarriage <- aggte(st_att_miscarriage, type = "group")
st_miscarriage_group_plot <- ggdid(st_att_group_miscarriage)

sc_att_abort_dynamic <- aggte(sc_att_abort, type = "dynamic")

sc_abort_dynamic_plot <- ggdid(sc_att_abort_dynamic)

sc_att_abort_calendar <- aggte(sc_att_abort, type = "calendar")

sc_abort_calendar_plot <- ggdid(sc_att_abort_calendar)

sc_att_group_abort <- aggte(sc_att_abort, type = "group")
sc_abort_group_plot <- ggdid(sc_att_group_abort)

sc_att_sb_dynamic <- aggte(sc_sb_att, type = "dynamic")

sc_sb_dynamic_plot <- ggdid(sc_att_sb_dynamic)

sc_att_sb_calendar <- aggte(sc_sb_att, type = "calendar")

sc_sb_calendar_plot <- ggdid(sc_att_sb_calendar)

sc_att_group_sb <- aggte(sc_sb_att, type = "group")
sc_sb_group_plot <- ggdid(sc_att_group_sb)

sc_att_miscarriage_dynamic <- aggte(sc_att_miscarriage, type = "dynamic")

sc_miscarriage_dynamic_plot <- ggdid(sc_att_miscarriage_dynamic)

sc_att_miscarriage_calendar <- aggte(sc_att_miscarriage, type = "calendar")

sc_miscarriage_calendar_plot <- ggdid(sc_att_miscarriage_calendar)

sc_att_group_miscarriage <- aggte(sc_att_miscarriage, type = "group")
sc_miscarriage_group_plot <- ggdid(sc_att_group_miscarriage)


###### combining sc group estimates into one plot
sc_abort_group <- aggte(sc_att_abort, type = "group")
#ggdid(sc_abort_group)

sc_abort_group_tidy <- tidy(sc_abort_group)
sc_abort_group_tidy$outcome <- c("Abortion")
sc_abort_group_tidy$est_per1000 <- sc_abort_group_tidy$estimate*1000
sc_abort_group_tidy$stderr_per1000 <- sc_abort_group_tidy$std.error*1000
sc_abort_group_tidy$conflow_per1000 <- sc_abort_group_tidy$conf.low*1000
sc_abort_group_tidy$confhigh_per1000 <- sc_abort_group_tidy$conf.high*1000



sc_sb_group <- aggte(sc_sb_att, type = "group")

sc_sb_group_tidy <- tidy(sc_sb_group)
sc_sb_group_tidy$outcome <- c("Stillbirth")
sc_sb_group_tidy$est_per1000 <- sc_sb_group_tidy$estimate*1000
sc_sb_group_tidy$stderr_per1000 <- sc_sb_group_tidy$std.error*1000
sc_sb_group_tidy$conflow_per1000 <- sc_sb_group_tidy$conf.low*1000
sc_sb_group_tidy$confhigh_per1000 <- sc_sb_group_tidy$conf.high*1000



sc_miscarriage_group <- aggte(sc_att_miscarriage, type = "group")

sc_miscarriage_group_tidy <- tidy(sc_miscarriage_group)
sc_miscarriage_group_tidy$outcome <- c("Miscarriage")
sc_miscarriage_group_tidy$est_per1000 <- sc_miscarriage_group_tidy$estimate*1000
sc_miscarriage_group_tidy$stderr_per1000 <- sc_miscarriage_group_tidy$std.error*1000
sc_miscarriage_group_tidy$conflow_per1000 <- sc_miscarriage_group_tidy$conf.low*1000
sc_miscarriage_group_tidy$confhigh_per1000 <- sc_miscarriage_group_tidy$conf.high*1000

sc_group_atts <- rbind(sc_sb_group_tidy, sc_abort_group_tidy, sc_miscarriage_group_tidy)

#filtering out only average overall
sc_group_atts_overall <- sc_group_atts %>% filter(group == "Average")

library(cowplot)
sc_atts_group_plot <- ggplot(data = sc_group_atts_overall, mapping = aes(x = outcome, y = (estimate*100)#, 
                                                                         #color = outcome
)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin = (conf.low*100), ymax = (conf.high*100)), width = 0.1, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, color = I("black"), linetype = 2)+
  #ylim(-8, 8)+
  scale_y_continuous(limits = c(-7, 7), breaks = c(-6, -4,-2,0,2, 4,6))+
  scale_color_manual(values = mycolors_outcomes, name = "Outcome", breaks = c("Stillbirth", "Abortion", "Miscarriage"))+
  ylab("Overall ATT -- sc (Percentage-Points)")+
  xlab("Adverse Pregnancy Outcome") +
  theme_cowplot()

#now add in st group estimates
st_abort_group <- aggte(st_att_abort, type = "group")
st_abort_group_tidy <- tidy(st_abort_group)
st_abort_group_tidy$outcome <- c("Abortion")
st_abort_group_tidy$est_per1000 <- st_abort_group_tidy$estimate*1000
st_abort_group_tidy$stderr_per1000 <- st_abort_group_tidy$std.error*1000
st_abort_group_tidy$conflow_per1000 <- st_abort_group_tidy$conf.low*1000
st_abort_group_tidy$confhigh_per1000 <- st_abort_group_tidy$conf.high*1000



st_sb_group <- aggte(st_sb_att, type = "group")

st_sb_group_tidy <- tidy(st_sb_group)
st_sb_group_tidy$outcome <- c("Stillbirth")
st_sb_group_tidy$est_per1000 <- st_sb_group_tidy$estimate*1000
st_sb_group_tidy$stderr_per1000 <- st_sb_group_tidy$std.error*1000
st_sb_group_tidy$conflow_per1000 <- st_sb_group_tidy$conf.low*1000
st_sb_group_tidy$confhigh_per1000 <- st_sb_group_tidy$conf.high*1000



st_miscarriage_group <- aggte(st_att_miscarriage, type = "group")

st_miscarriage_group_tidy <- tidy(st_miscarriage_group)
st_miscarriage_group_tidy$outcome <- c("Miscarriage")
st_miscarriage_group_tidy$est_per1000 <- st_miscarriage_group_tidy$estimate*1000
st_miscarriage_group_tidy$stderr_per1000 <- st_miscarriage_group_tidy$std.error*1000
st_miscarriage_group_tidy$conflow_per1000 <- st_miscarriage_group_tidy$conf.low*1000
st_miscarriage_group_tidy$confhigh_per1000 <- st_miscarriage_group_tidy$conf.high*1000


st_group_atts <- rbind(st_sb_group_tidy, st_abort_group_tidy, st_miscarriage_group_tidy)

#filtering out only average overall
st_group_atts_overall <- st_group_atts %>% filter(group == "Average")

#combining st and sc atts
sc_group_atts_overall$strata <- c("Scheduled Caste")
st_group_atts_overall$strata <- c("Scheduled Tribe")

sc_st_atts_overall <- rbind(sc_group_atts_overall, st_group_atts_overall)


#now combining into plot
sc_st_atts_group_plot <- ggplot(data = sc_st_atts_overall, 
                                mapping = aes(x = outcome, y = (estimate*1000), color = strata)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin = (conf.low*1000), ymax = (conf.high*1000)), width = 0.1, position=position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, color = I("black"), linetype = 2)+
  #ylim(-8, 8)+
  scale_y_continuous(limits = c(-150, 150), n.breaks = 10)+
  #scale_color_manual(values = mycolors_outcomes, name = "Outcome", breaks = c("Stillbirth", "Abortion", "Miscarriage"))+
  ylab("Overall ATT per 1,000 pregnancies")+
  xlab("Adverse Pregnancy Outcome") +
  theme_cowplot()

#event study plot

sc_att_abort_dynamic.dyn <- data.frame(time=sc_att_abort_dynamic[["egt"]],
                                       att=sc_att_abort_dynamic[["att.egt"]],
                                       se=sc_att_abort_dynamic[["se.egt"]])

sc_att_abort_dynamic.dyn  <- sc_att_abort_dynamic.dyn  %>% mutate(post=ifelse(time>=0,1,0))

ggplot(sc_att_abort_dynamic.dyn)+
  geom_vline(xintercept=-0,lty=2,color="grey59")+
  geom_hline(yintercept=0,lty=2,color="grey59")+
  
  geom_rect( aes(xmin=0,
                 xmax=9,
                 ymin=(sc_att_abort_dynamic[["overall.att"]]-1.96*sc_att_abort_dynamic[["overall.se"]])*100,
                 ymax=(sc_att_abort_dynamic[["overall.att"]]+1.96*sc_att_abort_dynamic[["overall.se"]])*100, alpha="95% CI"), fill="lightblue",
             color=NA)+
  geom_segment( 
    aes(x = 0, y = sc_att_abort_dynamic[["overall.att"]]*100, xend = 9, yend = sc_att_abort_dynamic[["overall.att"]]*100, lty="95% CI"),
    color="blue3")+
  
  geom_pointrange(aes(x=time,y=round(att*100,2),ymin = round((att-1.96*se)*100,2), ymax = round((att+1.96*se)*100,2),
                      color=as.factor(post)), size=1,fatten = 1)+
  
  ylab("Percentage-point difference in rate of abortions -- sc")+
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



ggdid(quint1_sb_att)
#make into one plot

quint1.sb.es <- aggte(quint1_sb_att, type="dynamic")
ggdid(quint1.sb.es)



