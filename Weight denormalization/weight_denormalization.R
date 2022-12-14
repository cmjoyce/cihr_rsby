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



