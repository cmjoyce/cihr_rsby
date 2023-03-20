library(tidyverse)
library(cowplot)

setwd("./Caroline Thesis/Aim 2")

df <- read.csv("df_socioeconomic.csv")


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

#renaming Kalaburagi as Gulbarga in dist
dist$namefix <- ifelse(dist$namefix == "Kalaburagi","Gulbarga", dist$namefix)

#found error in matching in district harmonization. DLHS3 state_dist == 2211 should be Raipur (466) not Garhwa (190). Fixing
df$dist_id <- ifelse(df$survey == "DLHS3" & df$state_dist == 2211, 466, df$dist_id)

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

#found duplicate in df districts. Kheri (300) Lakhimpur Kheri (332) are all the same districts. Renaming and fixing to just be 331 and correct name.
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

#gives 1.95 million pregnancies in states in which we have enrollment data  

