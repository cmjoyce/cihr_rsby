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


nfhs4 <- read_dta("IAIR73FL_DC.DTA", col_select = c(1:30))

nfhs5 <- fread("IAIR7AFL.csv", select = c(1:30))


ahs <- read_dta("women.dta", col_select = c(1))

#then must adjust state (use previously written code) and match on census


