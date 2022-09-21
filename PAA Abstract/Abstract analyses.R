
# PAA abstract start ------------------------------------------------------
library(tidyverse)
setwd("./PAA Abstract")

df <- read.csv("dlhsnfhs.csv")


# SES varaibles -----------------------------------------------------------

#looking at schooling as SES variable

table(df$years_school)

#making 98 and 99 NAs
df$years_school <- ifelse(df$years_school > 97, NA, df$years_school)

#separating into at least primary (grade 8) and less than primary.
#could later seperate into lower primary or higher, lower primary is class 1-IV
length(which(df$years_school > df$age)) #42 women who have more years of school than their age. Must be error.
#making into NA

df$years_school <- ifelse(df$years_school > df$age, NA, df$years_school)

df$primary <- ifelse(df$years_school >= 8, 1, 0)
table(df$primary)

# correcting rural / urban coding. In DLHS 1 is rural and 2 is urban, in NFHS it is the opposite.

table(df$rural_urban, df$survey)

#there are no NAs so we can use code below to swap. Changing to 0 = rural and 1 = urban. Using casewhen to 
# change all rural codes by survey to 0, and making the rest 1s as they must be urbans.

df <- df %>% mutate(urban = case_when(survey == "DLHS3" & rural_urban == 1 ~ 0,
                                      survey == "DLHS4" & rural_urban == 1 ~ 0,
                                      survey == "NFHS4" & rural_urban == 2 ~ 0,
                                      survey == "NFHS5" & rural_urban == 2 ~ 0,
                                      TRUE ~ 1))


#changing state variable. This will go back to DLHS-3 state. Putting Telangana back into Andhra Pradesh as it was not formed until 2014. 
df$state_new <- df$state #Making new state variable that is clone of original state.


#taking DLHS-4 telangana responses and making them andhra pradesh
df$state_new <- ifelse(df$survey == "DLHS4" & df$state_new == 36, 28, df$state_new)

#DLHS3 and 4 are now matched from that one change. Moving onto NFHS.

#NFHS-4 has no matching values with DLHS3 and 4 for state. Easier to start from fresh. 


#Removing all values in new to repopulate.
df$state_new <- ifelse(df$survey == "NFHS4",is.na(df$state_new), df$state_new)

#Harmonizing state coding
df <- df %>% mutate(state_try = case_when(survey == "NFHS4" & state == 14 ~ 1,
                                          survey == "NFHS4" & state == 13 ~ 2,
                                          survey == "NFHS4" & state == 28 ~ 3,
                                          survey == "NFHS4" & state == 6 ~ 4,
                                          survey == "NFHS4" & state == 34 ~ 5,
                                          survey == "NFHS4" & state == 12 ~ 6,
                                          survey == "NFHS4" & state == 25 ~ 7,
                                          survey == "NFHS4" & state == 29 ~ 8,
                                          survey == "NFHS4" & state == 33 ~ 9,
                                          survey == "NFHS4" & state == 5 ~ 10,
                                          survey == "NFHS4" & state == 30 ~ 11, 
                                          survey == "NFHS4" & state == 3 ~ 12,
                                          survey == "NFHS4" & state == 24 ~ 13,
                                          survey == "NFHS4" & state == 21 ~ 14,
                                          survey == "NFHS4" & state == 23 ~ 15, 
                                          survey == "NFHS4" & state == 32 ~ 16,
                                          survey == "NFHS4" & state == 22 ~ 17,
                                          survey == "NFHS4" & state == 4 ~ 18,
                                          survey == "NFHS4" & state == 35 ~ 19,
                                          survey == "NFHS4" & state == 15 ~ 20,
                                          survey == "NFHS4" & state == 26 ~ 21,
                                          survey == "NFHS4" & state == 7 ~ 22,
                                          survey == "NFHS4" & state == 19 ~ 23,
                                          survey == "NFHS4" & state == 11 ~ 24,
                                          survey == "NFHS4" & state == 9 ~ 25,
                                          survey == "NFHS4" & state == 8 ~ 26,
                                          survey == "NFHS4" & state == 20 ~ 27,
                                          survey == "NFHS4" & state == 2 ~ 28,
                                          survey == "NFHS4" & state == 16 ~ 29,
                                          survey == "NFHS4" & state == 10 ~ 30,
                                          survey == "NFHS4" & state == 18 ~ 31,
                                          survey == "NFHS4" & state == 17 ~ 32,
                                          survey == "NFHS4" & state == 31 ~ 33,
                                          survey == "NFHS4" & state == 27 ~ 34,
                                          survey == "NFHS4" & state == 1 ~ 35,
                                          survey == "NFHS4" & state == 36 ~ 28, #telangana responses and making them andhra pradesh
                                          TRUE ~ state_new))

df$newstate <- df$state_try
df <- df %>% select(-c(state_try, state_new))
#NFHS-4 now matches DLHS

#Now matching  NFHS-5. 

df$new_state <- ifelse(df$survey == "NFHS5" & df$state == 37, 1, df$newstate) #putting all of ladakh into jammu and kashmir

#Putting telangana into andhra pradesh
df$new_state <- ifelse(df$survey == "NFHS5" & df$state == 36, 28, df$new_state)

#putting all dadra & nagar haveli responses into daman & diu 
df$new_state <- ifelse(df$new_state == 26, 25, df$new_state)

#now missing response 26. Subtracting 1 from all responses over 25 to rectify

df$new_state <- ifelse(df$new_state > 25, (df$new_state-1), df$new_state)

#states now match across surveys. Dropping newstate and state
df <- df %>% select(-c(newstate, state))

#renaming new_state to state
df$state <- df$new_state
df <- df %>% select(-c(new_state))

#making factor
df$state <- as.factor(df$state)

# now looking at scheduled caste and tribe. These are coded differently in all surveys.
table(df$caste_group)

#1 is scheduled caste and 2 is scheduled tribe in all surveys. 6, 8, and 9 are all variations of none or other.
#Becuase of this, we will keep only responses that are 1 or 2 as this indicates that the participant is a member of an SC or ST.
#38.3% of women are in an SC or ST.

df$scheduled_group <- ifelse(df$caste_group < 3, 1, 0)

df <- df %>% select(-c(caste_group))


# Outcome -----------------------------------------------------------------



#now looking at outcomes. Coded differently across surveys. Harmonizing.
table(df$miscarriage_abortion_stillbirth)

#In DLHS the coding is as follows: 2  still birth 3   induced abortion 4 spontaneous abortion
#will re-code to match NFHS: 1 miscarriage 2 abortion 3 stillbirth

#making the one DLHS4 response of "0" NA

df$miscarriage_abortion_stillbirth <- ifelse(df$miscarriage_abortion_stillbirth == 0, NA, df$miscarriage_abortion_stillbirth)

#initializing variable as empty set. 
df$nonbirth <- NA

#creating new variable all using same scale
df <- df %>% mutate(nonbirth = case_when(survey == "DLHS3" & miscarriage_abortion_stillbirth == 4 ~ 1,
                                         survey == "DLHS3" & miscarriage_abortion_stillbirth == 3 ~ 2,
                                         survey == "DLHS3" & miscarriage_abortion_stillbirth == 2 ~ 3,
                                         survey == "DLHS4" & miscarriage_abortion_stillbirth == 4 ~ 1,
                                         survey == "DLHS4" & miscarriage_abortion_stillbirth == 3 ~ 2,
                                         survey == "DLHS4" & miscarriage_abortion_stillbirth == 2 ~ 3,
                                         survey == "NFHS4" & miscarriage_abortion_stillbirth == 1 ~ 1,
                                         survey == "NFHS4" & miscarriage_abortion_stillbirth == 2 ~ 2,
                                         survey == "NFHS4" & miscarriage_abortion_stillbirth == 3 ~ 3,
                                         survey == "NFHS5" & miscarriage_abortion_stillbirth == 1 ~ 1,
                                         survey == "NFHS5" & miscarriage_abortion_stillbirth == 2 ~ 2,
                                         survey == "NFHS5" & miscarriage_abortion_stillbirth == 3 ~ 3,
                                         TRUE ~ NA_real_))



#removing old miscarriage, aboriton, stillbirth variable
df <- df %>% select(-c(miscarriage_abortion_stillbirth))

#making any non live birth outcome variable
df$any_nonbirth <- ifelse(df$nonbirth > 0, 1, 0) 
df$any_nonbirth <- ifelse(is.na(df$any_nonbirth), 0, df$any_nonbirth)

#making separate miscarriage and stillbirth variable
df$miscarriage_sb <- ifelse(df$nonbirth != 2, 1, 0)

table(df$miscarriage_sb)

#Making NAs into 0s for miscarriage and stillbirth, as it means they did not occur. Currently only indcued abortion is labeled as 0.
df$miscarriage_sb <- ifelse(is.na(df$nonbirth), 0, df$miscarriage_sb)

#making just induced abortion variable
df$abortion <- ifelse(df$nonbirth == 2, 1, 0)
df$abortion <- ifelse(is.na(df$nonbirth), 0, df$abortion)

#making just stillbirth variable
df$stillbirth <- ifelse(df$nonbirth == 3, 1, 0)
df$stillbirth <- ifelse(is.na(df$nonbirth), 0, df$stillbirth)

#making just miscarriage variable
df$miscarriage <- ifelse(df$nonbirth == 1, 1, 0)
df$miscarriage <- ifelse(is.na(df$nonbirth), 0, df$miscarriage)

# analyses ----------------------------------------------------------------

#calculating weights. For NFHS surveys weights must be divided by 1000000

#dropping 166 DLSH4 observations that do not have weights. making new dataset.
df <- df %>% filter(complete.cases(wt))

df <- df %>% mutate(weight = case_when(survey == "NFHS4" ~ wt/1000000,
                                       survey == "NFHS5" ~ wt/1000000,
                                       survey == "DLHS3" ~ wt,
                                       survey == "DLHS4" ~ wt,
                                       TRUE ~ NA_real_))

# using new covariates of state, any primary, urban location, and if part of scheduled group

#lm <- lm(any_nonbirth ~ state + primary + urban + scheduled_group, data = df)

#summary(lm)

#lm_msb <- lm(miscarriage_sb ~ state + primary + urban + scheduled_group, data = df)
#summary(lm_msb)


#lm_abort <- lm(abortion ~ state + primary + urban + scheduled_group, data = df)
#summary(lm_abort)

#summary(glm(miscarriage_sb ~ state + primary + urban + scheduled_group, family = binomial(link = "logit"), data = df))

#complex model run on cluster. Code below copied over

library(survey)
library(broom)

#complex survey design when combining dlhs and nfhs?
#dropping 166 DLSH4 observations that do not have weights. making new dataset.

design <- svydesign(ids = df$psu, weights = df$weight, nest = TRUE, data = df)

#PSUs are repeated between NFHSs and DLHSs, have set nest = TRUE to account for that. Will look into this more.


complexglm_rate_m <- svyglm(miscarriage ~ bpl + primary + urban + scheduled_group + age + state, design = design,
                          data = df)


complexglm_rate_a <- svyglm(abortion ~ bpl + primary + urban + scheduled_group + age + state, design = design,
                              data = df)

complexglm_rate_s <- svyglm(stillbirth ~ bpl + primary + urban + scheduled_group + age + state, design = design,
                                data = df)


summary(complexglm_rate_m)
summary(complexglm_rate_a)
summary(complexglm_rate_s)




library(stargazer)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

tab_model(complexglm_rate_s, transform = NULL)

#rates_any <- complexglm_rate$coefficients*100

#rates_msb <- complexglm_rate_msb$coefficients*100
#rates_abort <- complexglm_rate_abort$coefficients*100

#95% confidence intervals
#confint(complexglm_rate)*100
#confint(complexglm_rate_msb)*100
#confint(complexglm_rate_abort)*100

#Putting into RD scale by multiplying esitmates and confidence intervals by 100. This becomes table2 

ts <- tidy(complexglm_rate_s, conf.int = TRUE)
ts$estimate <- ts$estimate*100
ts$conf.low <- ts$conf.low*100
ts$conf.high <- ts$conf.high*100

tm <- tidy(complexglm_rate_m, conf.int = TRUE)
tm$estimate <- tm$estimate*100
tm$conf.low <- tm$conf.low*100
tm$conf.high <- tm$conf.high*100

ta <- tidy(complexglm_rate_a, conf.int = TRUE)
ta$estimate <- ta$estimate*100
ta$conf.low <- ta$conf.low*100
ta$conf.high <- ta$conf.high*100

## One way of getting regression tables 
#stargazer(complexglm_rate, complexglm_rate_msb, complexglm_rate_abort,
          #covariate.labels = c("Constant", "Policy Implementation", "Month of Birth"),
          #dep.var.caption  = "",
          #dep.var.labels   = c("Paid Job","Formal Labor Contract"), 
#          model.numbers = FALSE, intercept.bottom = FALSE, 
#          type = "html", 
#          out = "abstract_table.htm")

library(gtsummary)

#harmonizing bpl card variable. In all surveys 1 is they do hold a BPL card. 0 and 2 are no, 8 is don't know. Making all of those values == to 0 to be
# no does not hold one, even if it's don't know.
df$bpl <- ifelse(df$bpl == 1, 1, 0)

#rural == 0, 1s as they must be urbans.

df$urban_rural <- factor(df$urban, 
                        levels = c(0, 1),
                        labels = c("Rural", "Urban"))


df %>% select(age, urban_rural, years_school, bpl, scheduled_group, tot_live_births) %>% tbl_summary(
  label = list(
    age ~ "Age",
    urban_rural ~ "Rural / Urban",
    years_school ~ "Years of School Completed",
    bpl ~ "Holds BPL Card",
    scheduled_group ~ "Scheduled Caste or Tribe",
    tot_live_births ~ "Total Live Births"),
  missing_text =  "Missing") %>% modify_header(label = "**Variable**")  %>%  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman")

#filter out NAs for plot so not showing NAs for miscarriage/abortion/stillbirth
df_plot <- df %>% drop_na(nonbirth)

#making factor for labeling
df_plot$Outcome <- factor(df_plot$nonbirth, 
                         levels = c(1, 2, 3),
                         labels = c("Miscarriage", "Abortion", "Stillbirth"))




library(cowplot)

#counts
g <- ggplot(data = df_plot, mapping = aes(x = survey, fill = nonbirth_plot))
g+geom_bar(position = "dodge") +  
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  theme_cowplot()

#percentages of total
library(ggsci)
library(RColorBrewer)

df_plot %>%
  count(survey = factor(survey), Outcome)  %>%
  group_by(survey) %>%
  mutate(n = prop.table(n) * 100) %>%
  ggplot(aes(survey, n, fill = Outcome)) +
  geom_col(position = 'dodge', na.rm = TRUE) + 
  ylim(0,100) +
#  scale_fill_discrete(na.translate=FALSE) +
  labs(x = "Survey",
       y = "Percentage of all non-live births") +
  scale_fill_brewer(palette = "Paired") +
#  theme_minimal() +
  theme_cowplot()





# OR Tables ---------------------------------------------------------------


## Second way to get regression tables 
tbl_regression(complexglm_log, exponentiate = TRUE) %>% modify_header(label = "**Variable**") %>% modify_column_hide(columns = p.value)%>%  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman")

theme_gtsummary_journal(journal = "jama")
theme_gtsummary_compact()

t1 <- svyglm(miscarriage ~ state + bpl + primary + urban + scheduled_group + age, design = design, 
         #family = quasibinomial(),
         data = df) %>%
  tbl_regression(#exponentiate = TRUE,
                include = -c(state, age),
                 label = list(
                   bpl ~ "Below the Povery Line",
                   primary ~ "Completed Primary School",
                   urban ~ "Lives Urban Locale",
                   scheduled_group ~ "Scheduled Caste or Tribe")) %>% modify_header(label = "") %>% modify_column_hide(columns = p.value) 

t2 <- svyglm(abortion ~ state + bpl + primary + urban + scheduled_group + age, design = design, 
             family = quasibinomial(),
             data = df) %>%
  tbl_regression(exponentiate = TRUE,
                 label = list(
                   state ~ "State",
                   bpl ~ "Below the Povery Line",
                   primary ~ "Completed Primary School",
                   urban ~ "Lives Urban Locale",
                   scheduled_group ~ "Scheduled Caste or Tribe",
                   age ~ "Age")) %>% modify_header(label = "") %>% modify_column_hide(columns = p.value)

t3 <- svyglm(stillbirth ~ state + bpl + primary + urban + scheduled_group + age, design = design, 
             family = quasibinomial(),
             data = df) %>%
  tbl_regression(exponentiate = TRUE,
                 label = list(
                   state ~ "State",
                   bpl ~ "Below the Povery Line",
                   primary ~ "Completed Primary School",
                   urban ~ "Lives Urban Locale",
                   scheduled_group ~ "Scheduled Caste or Tribe",
                   age ~ "Age")) %>% modify_header(label = "") %>% modify_column_hide(columns = p.value)


tbl_merge <-
  tbl_merge(
    tbls = list(t1, t2, t3),
    tab_spanner = c("**Miscarriage**", "**Abortion**", "**Stillbirth**")
  ) 
