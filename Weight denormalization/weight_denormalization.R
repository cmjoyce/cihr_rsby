setwd("./Weight denormalization")

#loading in full census tables

census2011 <- read.csv("2011_EverMarried_women.csv")
census2001 <- read.csv("2001_Ever_Married_Women.csv")

library(tidyverse)
names(census2011)


#keeping only female count columns for census tables
census2011 <- census2011 %>% select(-c(X, X.1, X.2, X.5, X.6, X.7, X.8, X.9, X.10, X.11, X.12, X.14, X.15, X.17, X.18, X.20, X.21, X.23, X.24, X.25))
names(census2011)

#renaming columns

census2011 <- census2011 %>% dplyr::rename(State = X.3, Strat = X.4, AgeGroup = C.2..MARITAL.STATUS.BY.AGE.AND.SEX, Married = X.13, Widowed = X.16, 
                                           Separated = X.19, Divorced = X.22)

#filtering out rural and urban
census2011 <- census2011 %>% filter(Strat == "Total")

#removing all India estimates
census2011 <- census2011 %>% filter(State != "INDIA")

# keeping only ages 15-49
table(census2011$AgeGroup)
census2011 <- census2011 %>% filter(AgeGroup %in% c("15-19", "20-24", "25-29", "30-34", "35-39",
                                                    "40-44", "45-49"))

#removing stratification column
census2011 <- census2011 %>% select(-c(Strat))

#grouping by state
census2011 <- census2011 %>% group_by(State)

#making numeric
census2011$Married <- as.numeric(census2011$Married)
census2011$Widowed <- as.numeric(census2011$Widowed)
census2011$Separated <- as.numeric(census2011$Separated)
census2011$Divorced <- as.numeric(census2011$Divorced)

check2011 <- census2011 %>% 
  group_by(State) %>% 
  summarise(across(c(Married, Widowed, Separated, Divorced), sum))

census2011 <- check2011

#making total
census2011$total <- rowSums(census2011[2:5])

#fixing name 
census2011$state_check <- sub(".*-", "", census2011$State)
census2011$state_check <- gsub('.{4}$', '', census2011$state_check)

#making state variable
census2011 <- census2011 %>%select(-c(State))
census2011 <- census2011 %>% rename(state = state_check)

#adding in code to match df
table(census2011$state)

#using harmonized combined codebook

#first trimming leading and trailing spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
census2011$state <- trim(census2011$state)


census2011 <- census2011 %>% 
  mutate(state_match = case_when(state == "ANDAMAN & NICOBAR ISLANDS" ~ 34,
                                 state == "ANDHRA PRADESH" ~ 27,
                                 state == "ARUNACHAL PRADESH" ~ 12,
                                 state == "ASSAM" ~ 18,
                                 state == "BIHAR" ~ 10,
                                 state == "CHANDIGARH" ~ 4,
                                 state == "CHHATTISGARH" ~ 22,
                                 state == "DADRA & NAGAR HAVELI" ~25,
                                 state == "DAMAN & DIU" ~ 25,
                                 state == "GOA" ~ 29,
                                 state == "GUJARAT" ~ 24,
                                 state == "HARYANA" ~ 6,
                                 state == "HIMACHAL PRADESH" ~ 2,
                                 state == "JAMMU & KASHMIR" ~ 1,
                                 state == "JHARKHAND" ~ 20,
                                 state == "KARNATAKA" ~ 28,
                                 state == "KERALA" ~ 31,
                                 state == "LAKSHADWEEP" ~ 30,
                                 state == "MADHYA PRADESH" ~ 23,
                                 state == "MAHARASHTRA" ~ 26,
                                 state == "MANIPUR" ~14,
                                 state == "MEGHALAYA" ~17,
                                 state == "MIZORAM" ~ 15,
                                 state == "NAGALAND" ~ 13,
                                 state == "NCT OF DELHI" ~7,
                                 state == "ODISHA" ~ 21,
                                 state == "PUDUCHERRY" ~ 33,
                                 state == "PUNJAB" ~ 3,
                                 state == "RAJASTHAN" ~8,
                                 state == "SIKKIM" ~ 11,
                                 state == "TAMIL NADU" ~ 32,
                                 state == "TRIPURA" ~ 16,
                                 state == "UTTAR PRADESH" ~ 9,
                                 state == "UTTARAKHAND" ~ 5,
                                 state == "WEST BENGAL" ~ 19,
                                 TRUE ~ NA_real_))


#now with census 2001
#keeping only female count columns for census tables
census2001 <- census2001 %>% select(c(X.4, X.5, C.2..MARITAL.STATUS.BY.AGE.AND.SEX, X.14, X.17, X.20))
names(census2001)

#renaming columns

census2001 <- census2001 %>% dplyr::rename(State = X.4, Strat = X.5, AgeGroup = C.2..MARITAL.STATUS.BY.AGE.AND.SEX,
                                           Married = X.14, Widowed = X.17,SepDiv = X.20)
#filtering out rural and urban
census2001 <- census2001 %>% filter(Strat == "Total")

#removing all India estimates
census2001 <- census2001 %>% filter(State != "INDIA")

# keeping only ages 15-49
table(census2001$AgeGroup)
census2001 <- census2001 %>% filter(AgeGroup %in% c("15-19", "20-24", "25-29", "30-34", "35-39",
                                                    "40-44", "45-49"))

#removing stratification column
census2001 <- census2001 %>% select(-c(Strat))

#grouping by state
census2001 <- census2001 %>% group_by(State)

#making numeric
census2001$Married <- as.numeric(census2001$Married)
census2001$Widowed <- as.numeric(census2001$Widowed)
census2001$SepDiv <- as.numeric(census2001$SepDiv)

#summing across
check2001 <- census2001 %>% 
  group_by(State) %>% 
  summarise(across(c(Married, Widowed, SepDiv), sum))

census2001 <- check2001

#making total
census2001$total <- rowSums(census2001[2:4])

#fixing name 
census2001$state_check <- sub(".*-", "", census2001$State)
census2001$state_check <- gsub('.{4}$', '', census2001$state_check)

#making state variable
census2001 <- census2001 %>%select(-c(State))
census2001 <- census2001 %>% rename(state = state_check)

#adding in code to match df
table(census2001$state)

#using harmonized combined codebook

#first trimming leading and trailing spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
census2001$state <- trim(census2001$state)


census2001 <- census2001 %>% 
  mutate(state_match = case_when(state == "ANDAMAN & NICOBAR ISLANDS" ~ 34,
                                 state == "ANDHRA PRADESH" ~ 27,
                                 state == "ARUNACHAL PRADESH" ~ 12,
                                 state == "ASSAM" ~ 18,
                                 state == "BIHAR" ~ 10,
                                 state == "CHANDIGARH" ~ 4,
                                 state == "CHHATTISGARH" ~ 22,
                                 state == "DADRA & NAGAR HAVELI" ~25,
                                 state == "DAMAN & DIU" ~ 25,
                                 state == "GOA" ~ 29,
                                 state == "GUJARAT" ~ 24,
                                 state == "HARYANA" ~ 6,
                                 state == "HIMACHAL PRADESH" ~ 2,
                                 state == "JAMMU & KASHMIR" ~ 1,
                                 state == "JHARKHAND" ~ 20,
                                 state == "KARNATAKA" ~ 28,
                                 state == "KERALA" ~ 31,
                                 state == "LAKSHADWEEP" ~ 30,
                                 state == "MADHYA PRADESH" ~ 23,
                                 state == "MAHARASHTRA" ~ 26,
                                 state == "MANIPUR" ~14,
                                 state == "MEGHALAYA" ~17,
                                 state == "MIZORAM" ~ 15,
                                 state == "NAGALAND" ~ 13,
                                 state == "DELHI" ~7,
                                 state == "ORISSA" ~ 21,
                                 state == "PONDICHERRY" ~ 33,
                                 state == "PUNJAB" ~ 3,
                                 state == "RAJASTHAN" ~8,
                                 state == "SIKKIM" ~ 11,
                                 state == "TAMIL NADU" ~ 32,
                                 state == "TRIPURA" ~ 16,
                                 state == "UTTAR PRADESH" ~ 9,
                                 state == "UTTARANCHAL" ~ 5,
                                 state == "WEST BENGAL" ~ 19,
                                 TRUE ~ NA_real_))

#combining dadra and nagar havel and daman and diu
census2011check <- census2011 %>%
  group_by(state_match) %>%
  summarise(across(c(total), sum))

census2011 <- census2011check

census2001check <- census2001 %>%
  group_by(state_match) %>%
  summarise(across(c(total), sum))

census2001 <- census2001check

#now combining 
census2011 <- census2011 %>% rename(EM_2011 = total)
census2001 <- census2001 %>% rename(EM_2001 = total)

census <- full_join(census2001, census2011)

#write.csv(census, "ever_married_women_by_state.csv")

census <- read.csv("ever_married_women_by_state.csv")

#now add in sampled fraction by state.
#read in state variable for each survey, group by state and get row sums (total interviewed/state),
#then weight_adj=wt × (total females age 15-49 in each state at the time of the survey)/(number of women age 15-49 interviewed in the survey)

library(haven)
library(data.table)

dlhs3 <- fread("dlhs3_numeric.csv", select = c(1))

dlhs3 <- dlhs3 %>%
  #group_by(state) %>%
  count(state)



dlhs4 <- fread("DLHS-4 Women.csv", select = c(1))

dlhs4 <- dlhs4 %>% count(state)

dlhs3 <- dlhs3 %>% rename(dlhs3_sampled = n)
dlhs4 <- dlhs4 %>% rename(dlhs4_sampled = n)

dlhs3$survey <- c("dlhs3")
dlhs4$survey <- c("dlhs4")


nfhs4 <- read_dta("IAIR73FL_DC.DTA", col_select = c(1:30))

nfhs4 <- nfhs4 %>% select(v024) %>% count(v024)

nfhs4$survey <- c("nfhs4")

nfhs5 <- fread("IAIR7AFL.csv", select = c(1:30))
nfhs5 <- nfhs5 %>% select(v024) %>% count(v024)

nfhs5$survey <- c("nfhs5")

nfhs4 <- nfhs4 %>% rename(nfhs4_sampled = n)
nfhs5 <- nfhs5 %>% rename(nfhs5_sampled = n)

nfhs4 <- nfhs4 %>% rename(state = v024)
nfhs5 <- nfhs5 %>% rename(state = v024) 


ahs <- read_dta("women.dta", col_select = c(1))
ahs <- ahs %>% count(STATE)
ahs$survey <- c("ahs")
ahs <- ahs %>% rename(ahs_sampled = n)
ahs <- ahs %>% rename(state = STATE)

#then must adjust state (use previously written code) and match on census
#Harmonizing state coding

#changing state variable. This will go back to DLHS-3 state. Putting Telangana back into Andhra Pradesh as it was not formed until 2014. 
dlhs4$state_new <- dlhs4$state #Making new state variable that is clone of original state.


#taking DLHS-4 telangana responses and making them andhra pradesh
dlhs4$state_new <- ifelse(dlhs4$state_new == 36, 28, dlhs4$state_new)

#now missing response 26. Subtracting 1 from all responses over 25 to rectify

dlhs4$state_new <- ifelse(dlhs4$state_new > 25, (dlhs4$state_new-1), dlhs4$state_new)


#DLHS3 and 4 are now matched from that one change. Moving onto NFHS.

#NFHS-4 has no matching values with DLHS3 and 4 for state. Easier to start from fresh. 

#Harmonizing state coding
nfhs4 <- nfhs4 %>% mutate(state_try = case_when(state == 14 ~ 1,
                                                     state == 13 ~ 2,
                                                      state == 28 ~ 3,
                                                      state == 6 ~ 4,
                                                      state == 34 ~ 5,
                                                      state == 12 ~ 6,
                                                      state == 25 ~ 7,
                                                      state == 29 ~ 8,
                                                      state == 33 ~ 9,
                                                      state == 5 ~ 10,
                                                      state == 30 ~ 11, 
                                                      state == 3 ~ 12,
                                                      state == 24 ~ 13,
                                                      state == 21 ~ 14,
                                                      state == 23 ~ 15, 
                                                      state == 32 ~ 16,
                                                      state == 22 ~ 17,
                                                      state == 4 ~ 18,
                                                      state == 35 ~ 19,
                                                      state == 15 ~ 20,
                                                      state == 26 ~ 21,
                                                      state == 7 ~ 22,
                                                      state == 19 ~ 23,
                                                      state == 11 ~ 24,
                                                      state == 9 ~ 25,
                                                      state == 8 ~ 26,
                                                      state == 20 ~ 27,
                                                      state == 2 ~ 28,
                                                      state == 16 ~ 29,
                                                      state == 10 ~ 30,
                                                      state == 18 ~ 31,
                                                      state == 17 ~ 32,
                                                      state == 31 ~ 33,
                                                      state == 27 ~ 34,
                                                      state == 1 ~ 35,
                                                      state == 36 ~ 28, #telangana responses and making them andhra pradesh
                                                     ))

#putting all dadra & nagar haveli responses into daman & diu 
nfhs4$state_try <- ifelse(nfhs4$state_try == 26, 25, nfhs4$state_try)

#now missing response 26. Subtracting 1 from all responses over 25 to rectify

nfhs4$state_try <- ifelse(nfhs4$state_try > 25, (nfhs4$state_try-1), nfhs4$state_try)

#doing the same for dlhs3
#putting all dadra & nagar haveli responses into daman & diu 
dlhs3$state_try <- ifelse(dlhs3$state == 26, 25, dlhs3$state)

#now missing response 26. Subtracting 1 from all responses over 25 to rectify

dlhs3$state_try <- ifelse(dlhs3$state_try > 25, (dlhs3$state_try-1), dlhs3$state_try)

dlhs3 <- dlhs3 %>% select(-c(state))
dlhs3 <- dlhs3 %>% rename(state = state_try)

nfhs4 <- nfhs4 %>% select(-c(state))
nfhs4 <- nfhs4 %>% rename(state = state_try)

dlhs4 <- dlhs4 %>% select(-c(state))
dlhs4 <- dlhs4 %>% rename(state = state_new)

#NFHS-4 now matches DLHS

#Now matching  NFHS-5. 

nfhs5$new_state <- ifelse(nfhs5$state == 37, 1, nfhs5$state) #putting all of ladakh into jammu and kashmir

#Putting telangana into andhra pradesh
nfhs5$new_state <- ifelse(nfhs5$state == 36, 28, nfhs5$new_state)

#putting all dadra & nagar haveli responses into daman & diu 
nfhs5$new_state <- ifelse(nfhs5$new_state == 26, 25, nfhs5$new_state)

#now missing response 26. Subtracting 1 from all responses over 25 to rectify

nfhs5$new_state <- ifelse(nfhs5$new_state > 25, (nfhs5$new_state-1), nfhs5$new_state)

#states now match across surveys. Dropping newstate and state
nfhs5 <- nfhs5 %>% select(-c(state))
nfhs5 <- nfhs5 %>% rename(state = new_state)

#combining rows of new same state
nfhs4 <- nfhs4 %>%
  group_by(state) %>%
  summarise(across(c(nfhs4_sampled), sum))

nfhs5 <- nfhs5 %>%
  group_by(state) %>%
  summarise(across(c(nfhs5_sampled), sum))


#ahs state stays as is. Merging upon all surveys.

nfhs <- left_join(nfhs4, nfhs5, by = "state")


dlhs4 <- dlhs4 %>%
  group_by(state) %>%
  summarise(across(c(dlhs4_sampled), sum))

dlhs3 <- dlhs3 %>%
  group_by(state) %>%
  summarise(across(c(dlhs3_sampled), sum))


dlhs3 <- dlhs3 %>% select(-c(survey))
dlhs4 <- dlhs4 %>% select(-c(survey))

dlhs <- left_join(dlhs3, dlhs4, by = "state")

dlhsnfhs <- full_join(nfhs, dlhs, by = "state")

ahs <- ahs %>% select(-c(survey))
ahsdlhsnfhs <- full_join(dlhsnfhs, ahs, by = "state")

census <- census %>% rename(state = state_match)

sampled <- full_join(census, ahsdlhsnfhs, by = "state")

#now calculating proportions as: 
#(total females age 15-49 in the country at the time of the survey)/(number of women age 15-49 interviewed in the survey)

#using closest census to outcome year. Only DLHS3 will use census 2001. Will calculate proportion for both 2001 and 2011, outcome year
#will determine which proportion to use. If outcome year is closer 2004 or 2005 we will use 2001. 2006-2008 will use 2011.

sampled$dlhs3_prop_2001 <- sampled$EM_2001 / sampled$dlhs3_sampled
sampled$dlhs3_prop_2011 <- sampled$EM_2011 / sampled$dlhs3_sampled

sampled$dlhs4_prop <- sampled$EM_2011 / sampled$dlhs4_sampled

sampled$ahs_prop <- sampled$EM_2011 / sampled$ahs_sampled

sampled$nfhs4_prop <- sampled$EM_2011 / sampled$nfhs4_sampled
sampled$nfhs5_prop <- sampled$EM_2011 / sampled$nfhs5_sampled

#saving as csv
#write.csv(sampled, "sampled_prop_by_state.csv")
weights_sample <- read.csv("sampled_prop_by_state.csv")
