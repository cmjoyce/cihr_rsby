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

districts <- read.csv("District_Codebook.csv")
glimpse(districts)

districts <- unique(districts)
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
                                                            State == "Madhya Pardesh" ~ 23,
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



