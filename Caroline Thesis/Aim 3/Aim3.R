# Begin -------------------------------------------------------------------


library(tidyverse)
library(survey)
library(ggpubr)
library(flextable)
library(janitor)
library(AER)
library(broom)
#library(kableExtra)



setwd("./Caroline Thesis/Aim 3")

#df <- read.csv("df_dist_all_years_w_treatment.csv")

#June 24 2023 bringing in AHS health insurance + rsby answers from later rounds for pregnancies
# in first round AHS where these questions weren't answered. See ahslink.R file

try <- read.csv("ahs_health_ins_later_rounds.csv")

#variables that are being updates for AHS *only* are health_insurance, esis, rsby, cghsshis, reimburse,
# chip, other_private, & other_insurance

#renaming in try
try <- try %>% rename(health_ins = health_insurance)

# segmenting out ahs
ahs <- df %>% filter(survey == "AHS")
ahs_nas_hi <- ahs %>% filter(is.na(health_ins))
ahs_merge <- left_join(ahs_nas_hi, try, by = c("caseid")) ## matched on multiple. Keeping unique
ahs_fixed <- unique(ahs_merge)

names(ahs_fixed)
ahs_fixed <- ahs_fixed %>% select(-c(X.1, X))
ahs_fixed <- ahs_fixed %>% select(-c(health_ins.x, esis.x, rsby.x, cghsshis.x, reimburse.x, chip.x, other_private.x, other_insurance.x,
                                     survey.x))

ahs_fixed <- ahs_fixed %>% rename(health_ins = health_ins.y, esis = esis.y, rsby = rsby.y, cghsshis = cghsshis.y, reimburse = reimburse.y,
                                  chip = chip.y, other_private = other_private.y, other_insurance = other_insurance.y,
                                  survey = survey.y)

#harmonizing health_ins variable
table(ahs_fixed$health_ins)
ahs_fixed$health_ins <- ifelse(ahs_fixed$health_ins == 3, NA, ahs_fixed$health_ins)
ahs_fixed$health_ins <- ifelse(ahs_fixed$health_ins == 2, 0, ahs_fixed$health_ins)


list_fixed_ids <- c(ahs_fixed$caseid)

df_dropped <- df %>% filter(!caseid %in% list_fixed_ids)

nrow(df) - nrow(df_dropped) #654,714

df_fixed <- rbind(df_dropped, ahs_fixed)


#write.csv(df_fixed, "df_updated_primary_w_socioeconomic_fixed_ahs_insurance.csv")
df <- df_fixed

df <- read.csv("df_updated_primary_w_socioeconomic_fixed_ahs_insurance.csv")

#redoing design
df$strat_rurb <- ifelse(df$rural_urban == 0, 2, 1)
design <- svydesign(data = df, ids = ~psu2, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)


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


##looking at whether individual RSBY enrollment aligns to district level RSBY enrollment
#in all surveys 1 is enrolled. In some 2 is no, and others it is 0 == no. Making match variable, 1 = enrolled

df$rsby_match <- ifelse(df$rsby == 1, 1, 0)
table(df$survey, is.na(df$rsby_match))
df <- df %>% mutate(rsby_match = case_when(survey == "AHS" & !is.na(health_ins) &is.na(rsby_match) ~ 0,
                                           TRUE ~ rsby_match))


df$rsby_match_fact <- factor(df$rsby_match,
                             levels = c(0,1),
                             labels = c("No", "Yes"))

tab <- addmargins(table(df$survey, df$rsby_match_fact))
tab %>% kbl %>% kable_styling()

sum(table(df$survey)) - length(which(df$survey == "DLHS3"))

#Excluding DLHS-3 (not possible to be enrolled) enrollment of 5.6%
(95081/1703312)*100

df$treated <- ifelse(df$treatment > 0, 1, 0)
table(df$treated)

table(df$rsby_match, df$treated, df$survey)
#1,992 respondants who say they are enrolled but we say they don't have access to the policy

table(df$g)
table(df$treat)


# IV analysis -------------------------------------------------------------
length(which(is.na(df$rsby)))

#creating new exposure variable so 0 if in year RSBY not yet eligible
df <- df %>% mutate(rsby_iv = case_when(outcome_year < 2008 ~ 0,
                                        outcome_year >= 2008 & rsby_match == 0 ~ 0,
                                        outcome_year >= 2008 & rsby_match == 1 ~ 1,
                                        outcome_year >= 2008 & is.na(rsby_match) ~ NA_real_))




#subsetting dataset so only those who answered RSBY question are in it
#df$rsby_match_full <- ifelse(is.na(df$rsby_match), 0, df$rsby_match)
sum(table(df$rsby_match))



df_rsby <- df %>% filter(!is.na(rsby_iv))

#removing 9 obs with missing age 
df_rsby <- df_rsby %>% filter(!is.na(age))
#redoing design
design_rsby <- svydesign(data = df_rsby, ids = ~psu2, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)

#filtering out NAs
#instrument is treatment group 0, 1, 2, 3
table(df$treat)
#making so matches outcome year
#creating new exposure variable so 0 if in year RSBY not yet eligible
df_rsby <- df_rsby %>% mutate(treat_iv = case_when(outcome_year < 2008 ~ 0,
                                         treat == 0 ~ 0,
                                         outcome_year >= 2008 & treat == 1 ~ 1,
                                         outcome_year >= 2010 & treat == 2 ~ 2,
                                         outcome_year < 2010 & treat == 2 ~ 0,
                                         outcome_year >= 2012 & treat == 3 ~ 3,
                                         outcome_year < 2012 & treat == 3 ~ 0,
                                         TRUE ~ NA_real_))

#redoing design
design_rsby <- svydesign(data = df_rsby, ids = ~psu2, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)

#design_rsby <- svydesign(data = df_rsby, ids = ~psu2, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)

sb_iv <- svyivreg(sb ~ rsby_iv + age + rural_urban + wi_perc_rank + outcome_year 
                  | . - rsby_iv + treat_iv, design = design_rsby)

abort_iv <- svyivreg(abort ~ rsby_iv + age + rural_urban + wi_perc_rank + outcome_year 
                     | . - rsby_iv + treat_iv, design = design_rsby)

ms_iv <- svyivreg(miscarriage ~ rsby_iv + age + rural_urban + wi_perc_rank + outcome_year 
                  | . - rsby_iv + treat_iv, design = design_rsby)

summary(sb_iv)
summary(abort_iv)
summary(ms_iv)

sb_iv <- tidy(sb_iv)
abort_iv <- tidy(abort_iv)
ms_iv <- tidy(ms_iv)


sb_iv$est_per_1000 <- (sb_iv$estimate * 1000) 
sb_iv$std.error_per1000 <- sb_iv$std.error*1000 
sb_iv[6:7] <- sb_iv[6:7] %>% round_half_up(digits = 2)

abort_iv$est_per_1000 <- (abort_iv$estimate * 1000) 
abort_iv$std.error_per1000 <- abort_iv$std.error*1000 
abort_iv[6:7] <- abort_iv[6:7] %>% round_half_up(digits = 2)


ms_iv$est_per_1000 <- (ms_iv$estimate * 1000) 
ms_iv$std.error_per1000 <- ms_iv$std.error*1000 
ms_iv[6:7] <- ms_iv[6:7] %>% round_half_up(digits = 2)


# ARCHIVE -----------------------------------------------------------------



#### Looking at non-missing data ####
table(df$health_ins, useNA = "always")

df$rsby_match <- ifelse(df$survey == "AHS" & !is.na(df$health_ins) & is.na(df$rsby_match), 0, df$rsby_match)

insurance <- df %>% group_by(survey) %>% summarize(percent_answered_insurance_question = (mean(is.na(health_ins)))*100,
                                                   percent_with_health_insurance = (mean(health_ins, na.rm = TRUE))*100,
                                                   percent_of_insured_with_rsby = (mean(rsby_match, na.rm = TRUE))*100,
                                                   percent_missing_rsby = (mean(is.na(rsby_match)))*100,
                                                   number_rsby_enrolled = sum(rsby_match, na.rm = TRUE)) 

insurance[,-1] <- insurance[,-1] %>% round_half_up(digits = 1)

insurance_tab <- flextable(insurance)
insurance_tab <- set_header_labels(insurance_tab,
                                   values = list(
                                     survey = "Survey",
                                     percent_answered_insurance_question = "% non-responses for Health Insurance",
                                     percent_with_health_insurance = "% reporting any health insurance (excluding NAs)",
                                     percent_of_insured_with_rsby = "% of insured reporting RSBY (excluding NAs)",
                                     percent_missing_rsby = "% non-responses for RSBY",
                                     number_rsby_enrolled = "Total count by group of RSBY enrolled"
                                   ))
#save_as_docx(insurance_tab, path = "insurance_summarized.docx")

year <- df %>% group_by(outcome_year) %>% summarize(percent_answered_insurance_question = (mean(is.na(health_ins)))*100,
                                                    percent_with_health_insurance = (mean(health_ins, na.rm = TRUE))*100,
                                                    percent_of_insured_with_rsby = (mean(rsby_match, na.rm = TRUE))*100,
                                                    percent_missing_rsby = (mean(is.na(rsby_match)))*100,
                                                    number_rsby_enrolled = sum(rsby_match, na.rm = TRUE)) 

year[,-1] <- year[,-1] %>% round_half_up(digits = 1)
year_tab <- flextable(year)

year_tab <- set_header_labels(year_tab,
                                   values = list(
                                     survey = "Survey",
                                     percent_answered_insurance_question = "% non-responses for Health Insurance",
                                     percent_with_health_insurance = "% reporting any health insurance (excluding NAs)",
                                     percent_of_insured_with_rsby = "% of insured reporting RSBY (excluding NAs)",
                                     percent_missing_rsby = "% non-responses for RSBY",
                                     number_rsby_enrolled = "Total count by group of RSBY enrolled"
                                   ))

year_tab <- colformat_num(x = year_tab, j = 1, big.mark = "")
year_tab

save_as_docx()


dlhs3_insurance <- df %>% group_by(survey) %>% summarize(notmissing_hi = length(which(!is.na(health_ins)))/ nrow(health_ins))

dlhs3_insurance <- df %>% filter(survey == "DLHS3") %>% summarise(notmissing_hi = sum(!is.na(health_ins))/n(health_ins),
                                                                 prop_hi = (length(which(dlhs3$health_ins == 1)) / sum(table(dlhs3$health_ins)))*100) %>%
  round_half_up(digits = 1) %>% mutate(prop_rsby = c(0), number_rsby_enrolled = c(0), survey = c("DLHS-3"))

dlhs4_insurance <- df %>% filter(survey == "DLHS4") %>% summarise(notmissing_hi = (length(which(!is.na(dlhs4$health_ins))) / nrow(dlhs4))*100,
                                                                  prop_hi = (length(which(dlhs4$health_ins == 1)) / sum(table(dlhs4$health_ins)))*100,
                                                                  prop_rsby = (length(which(dlhs4$rsby_match == 1)) / sum(table(dlhs4$rsby_match)))*100,
                                                                  number_rsby_enrolled = length(which(rsby_match == 1)))%>% 
   round_half_up(digits = 1) %>%mutate(survey = c("DLHS-4"))



AHS <- df %>% filter(survey == "AHS")
AHS$rsby_match <- ifelse(!is.na(AHS$health_ins) & is.na(AHS$rsby_match), 0, AHS$rsby_match)

AHS_insurance <- AHS %>% summarise(notmissing_hi = (length(which(!is.na(AHS$health_ins))) / nrow(AHS))*100,
                                   prop_hi = (length(which(AHS$health_ins == 1)) / sum(table(AHS$health_ins)))*100,
                                   prop_rsby = (length(which(AHS$rsby_match == 1)) / sum(table(AHS$rsby_match)))*100,
                                   number_rsby_enrolled = length(which(rsby_match == 1))) %>%
  round_half_up(digits = 1) %>% mutate(survey = c("AHS"))


NFHS4_insurance <- df %>% filter(survey == "NFHS4") %>% summarise(notmissing_hi = (length(which(!is.na(health_ins))) / nrow(health_ins))*100,
                                                                  prop_hi = (length(which(health_ins == 1)) / sum(table(health_ins)))*100,
                                                                  prop_rsby = (length(which(rsby_match == 1)) / sum(table(rsby_match)))*100,
                                                                  number_rsby_enrolled = length(which(rsby_match == 1)))%>% 
  round_half_up(digits = 1) %>% mutate(survey = c("NFHS-4"))


NFHS5_insurance <- df %>% filter(survey == "NFHS5") %>% summarise(notmissing_hi = (length(which(!is.na(NFHS5$health_ins))) / nrow(NFHS5))*100,
                                                                  prop_hi = (length(which(NFHS5$health_ins == 1)) / sum(table(NFHS5$health_ins)))*100,
                                                                  prop_rsby = (length(which(NFHS5$rsby_match == 1)) / sum(table(NFHS5$rsby_match)))*100)%>% 
  round_half_up(digits = 1) %>% mutate(survey = c("NFHS-5"))

surveyed_health_ins <- rbind(dlhs3_insurance, dlhs4_insurance, AHS_insurance, NFHS4_insurance, NFHS5_insurance)



df_healthins <- df %>% filter(!is.na(health_ins))

table(df_healthins$survey, is.na(df_healthins$rsby_match))
table(df_healthins$rsby_match, df_healthins$survey)

#making no rsby if has health insurance but rsby == 0
df_healthins$rsby_match <- ifelse(df_healthins$health_ins == 1 & df_healthins$rsby_match != 1, 0, df_healthins$rsby_match)

df_healthins <- df_healthins %>% mutate(rsby = case_when(survey == "AHS" & is.na(rsby_match) ~ 0,
                                                         TRUE ~ rsby_match))

df_eligible <- df_healthins %>% filter(survey != "DLHS3")
table(df_eligible$survey, df_eligible$rsby)
length(which(df_eligible$rsby == 1)) / sum(table(df_eligible$rsby))

tab <- table(df_eligible$outcome_year, df_eligible$rsby)
#library(kableExtra)
df_eligible$rsby_match_fact <- factor(df_eligible$rsby,
                             levels = c(0,1),
                             labels = c("No", "Yes"))

tab <- addmargins(table(df_eligible$outcome_year, df_eligible$rsby_match_fact))
tab %>% kbl %>% kable_styling()


df_healthins$rsby_match_fact <- factor(df_healthins$rsby,
                                      levels = c(0,1),
                                      labels = c("No", "Yes"))

tab_survey <- addmargins(table(df_healthins$survey, df_healthins$rsby_match_fact))
tab_survey %>% kbl %>% kable_styling()




df_rsby <- df %>% filter(survey == "DLHS4" | survey == "NFHS4" | survey == "NFHS5")
df_rsby <- df_rsby %>% filter(!is.na(rsby))
df_rsby$rsby_match <- ifelse(df_rsby$rsby == 1, 1, 0)
#creating new exposure variable so 0 if in year RSBY not yet eligible
df_eligible <- df_eligible %>% mutate(rsby_iv = case_when(outcome_year < 2011 & g == 2010 ~ 1,
                                                  outcome_year < 2011 & g > 2010 ~ 0,
                                                  outcome_year == 2011 & g < 2014 ~ 1,
                                                  outcome_year == 2012 & g < 2014 ~ 1,
                                                  outcome_year == 2011 & g == 2014 ~ 0,
                                                  outcome_year == 2012 & g == 2014 ~ 0,
                                                  outcome_year > 2012 & g > 0 ~ 1,
                                                  g == 0 ~ 0,
                                                  TRUE ~ NA_real_))


table(df_eligible$rsby_iv, df_eligible$outcome_year)

table(df_eligible$rsby_iv, df_eligible$year_of_intr, df_eligible$g)
#### NFHS IV ####

nfhs <- df %>% filter(survey == "NFHS4" | survey == "NFHS5")

table(nfhs$g, nfhs$year_of_intr)
table(nfhs$rsby_match)
table(nfhs$state)

design_nfhs <- svydesign(data = nfhs, ids = ~psu2, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)

#instrumnet is treatment group. Using 0/1/2/3 variable
table(nfhs$treat)

sb_iv <- svyivreg(sb ~ rsby_match + age + rural_urban + wi_perc_rank + outcome_year + primary_school
                  | . - rsby_match + treat, design = design_nfhs)

abort_iv <- svyivreg(abort ~ rsby_match + age + rural_urban + wi_perc_rank + outcome_year + primary_school
                     | . - rsby_match + treat, design = design_nfhs)

ms_iv <- svyivreg(miscarriage ~ rsby_match + age + rural_urban + wi_perc_rank + outcome_year + primary_school
                  | . - rsby_match + treat, design = design_nfhs)


table(nfhs$treat)
table(nfhs$g)



summary(sb_iv)
summary(abort_iv)
summary(ms_iv)



