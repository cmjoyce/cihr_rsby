# Aim 1 import ------------------------------------------------------
library(tidyverse)
setwd("./Caroline Thesis/Aim 1")

df <- read.csv("harmonized_variables.csv")


# Making binary pregnancy outcome variables -------------------------------

df$stillbirth




# analyses ----------------------------------------------------------------

#calculating weights. For NFHS surveys weights must be divided by 1000000

#dropping 166 DLSH4 observations that do not have weights. making new dataset.
df <- df %>% filter(complete.cases(wt))

df <- df %>% mutate(weight = case_when(survey == "NFHS4" ~ wt/1000000,
                                       survey == "NFHS5" ~ wt/1000000,
                                       survey == "DLHS3" ~ wt,
                                       survey == "DLHS4" ~ wt,
                                       survey == "AHS" ~ wt,
                                       TRUE ~ NA_real_))


# calculating jackknife and bootstrap weights in survey ---------------------------------------------------------

library(survey)

##### Run on cluster ######

weightdesign <- svydesign(data = df,ids = ~state, strata =  ~psu, nest = TRUE)

dfboot <- as.svrepdesign(weightdesign, type="bootstrap")

write.csv(dfboot, "dfboot.csv")

dfboot <- read.csv("dfboot.csv")





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

design <- svydesign(ids = ~psu, weights = ~weight, nest = TRUE, data = df)

#PSUs are repeated between NFHSs and DLHSs, have set nest = TRUE to account for that. Will look into this more.


complexglm_rate_m <- svyglm(miscarriage ~ bpl + primary + urban + scheduled_group + age + state, design = design,
                            data = df)

complexglm_rate_m_bpl <- svyglm(miscarriage ~ bpl + age + state, design = design,
                                data = df)

tidy_mbpl <- tidy(complexglm_rate_m_bpl, conf.int = T)

complexglm_rate_m_prim <- svyglm(miscarriage ~ primary + age + state, design = design,
                                 data = df)

tidy_mprim <- tidy(complexglm_rate_m_prim, conf.int = T)

complexglm_rate_m_urb <- svyglm(miscarriage ~  urban + age + state, design = design,
                                data = df)

tidy_murb <- tidy(complexglm_rate_m_urb, conf.int = T)

complexglm_rate_m_sch <- svyglm(miscarriage ~ scheduled_group + age + state, design = design,
                                data = df)

tidy_msch <- tidy(complexglm_rate_m_sch, conf.int = T)


#library(margins)
#devtools::install_github("tzoltak/margins")
#complexglm_rate_m_log <- svyglm(miscarriage ~ age + state, design = design,
#                            family = quasibinomial(), data = df)


complexglm_rate_a <- svyglm(abortion ~ bpl + primary + urban + scheduled_group + age + state, design = design,
                            data = df)

complexglm_rate_a_bpl <- svyglm(abortion ~ bpl+ age + state, design = design,
                                data = df)

tidy_abpl <- tidy(complexglm_rate_a_bpl, conf.int = TRUE)

complexglm_rate_a_prim <- svyglm(abortion ~ primary + age + state, design = design,
                                 data = df)

tidy_aprim <- tidy(complexglm_rate_a_prim, conf.int = T)

complexglm_rate_a_urb <- svyglm(abortion ~ urban  + age + state, design = design,
                                data = df)

tidy_aurb <- tidy(complexglm_rate_a_urb, conf.int = T)

complexglm_rate_a_sch <- svyglm(abortion ~ scheduled_group + age + state, design = design,
                                data = df)

tidy_asch <- tidy(complexglm_rate_a_sch, conf.int = T)

complexglm_rate_s <- svyglm(stillbirth ~ bpl + primary + urban + scheduled_group + age + state, design = design,
                            data = df)

complexglm_rate_s_bpl <- svyglm(stillbirth ~ bpl + age + state, design = design,
                                data = df)

tidy_sbpl <- tidy(complexglm_rate_s_bpl, conf.int = TRUE)

complexglm_rate_s_prim <- svyglm(stillbirth ~ primary + age + state, design = design,
                                 data = df)

tidy_sprim <- tidy(complexglm_rate_s_prim, conf.int = TRUE)

complexglm_rate_s_urb <- svyglm(stillbirth ~ urban + age + state, design = design,
                                data = df)

tidy_surb <- tidy(complexglm_rate_s_urb, conf.int = TRUE)

complexglm_rate_s_sch <- svyglm(stillbirth ~ scheduled_group + age + state, design = design,
                                data = df)

tidy_ssch <- tidy(complexglm_rate_s_sch, conf.int = TRUE)






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

tidy_sbpl$estimate <- tidy_sbpl$estimate*100
tidy_sbpl$conf.low <- tidy_sbpl$conf.low*100
tidy_sbpl$conf.high <- tidy_sbpl$conf.high*100

tidy_sprim$estimate <- tidy_sprim$estimate*100
tidy_sprim$conf.low <- tidy_sprim$conf.low*100
tidy_sprim$conf.high <- tidy_sprim$conf.high*100

tidy_surb$estimate <- tidy_surb$estimate*100
tidy_surb$conf.low <- tidy_surb$conf.low*100
tidy_surb$conf.high <- tidy_surb$conf.high*100

tidy_ssch$estimate <- tidy_ssch$estimate*100
tidy_ssch$conf.low <- tidy_ssch$conf.low*100
tidy_ssch$conf.high <- tidy_ssch$conf.high*100


tm <- tidy(complexglm_rate_m, conf.int = TRUE)
tm$estimate <- tm$estimate*100
tm$conf.low <- tm$conf.low*100
tm$conf.high <- tm$conf.high*100

tidy_mbpl$estimate <- tidy_mbpl$estimate*100
tidy_mbpl$conf.low <- tidy_mbpl$conf.low*100
tidy_mbpl$conf.high <- tidy_mbpl$conf.high*100

tidy_mprim$estimate <- tidy_mprim$estimate*100
tidy_mprim$conf.low <- tidy_mprim$conf.low*100
tidy_mprim$conf.high <- tidy_mprim$conf.high*100

tidy_murb$estimate <- tidy_murb$estimate*100
tidy_murb$conf.low <- tidy_murb$conf.low*100
tidy_murb$conf.high <- tidy_murb$conf.high*100

tidy_msch$estimate <- tidy_msch$estimate*100
tidy_msch$conf.low <- tidy_msch$conf.low*100
tidy_msch$conf.high <- tidy_msch$conf.high*100

ta <- tidy(complexglm_rate_a, conf.int = TRUE)
ta$estimate <- ta$estimate*100
ta$conf.low <- ta$conf.low*100
ta$conf.high <- ta$conf.high*100

tidy_abpl$estimate <- tidy_abpl$estimate*100
tidy_abpl$conf.low <- tidy_abpl$conf.low*100
tidy_abpl$conf.high <- tidy_abpl$conf.high*100

tidy_aprim$estimate <- tidy_aprim$estimate*100
tidy_aprim$conf.low <- tidy_aprim$conf.low*100
tidy_aprim$conf.high <- tidy_aprim$conf.high*100

tidy_aurb$estimate <- tidy_aurb$estimate*100
tidy_aurb$conf.low <- tidy_aurb$conf.low*100
tidy_aurb$conf.high <- tidy_aurb$conf.high*100

tidy_asch$estimate <- tidy_asch$estimate*100
tidy_asch$conf.low <- tidy_asch$conf.low*100
tidy_asch$conf.high <- tidy_asch$conf.high*100

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
       y = "Percentage of all pregnancy terminations") +
  scale_fill_brewer(palette = "Paired") +
  #  theme_minimal() +
  theme_cowplot()


# SEPTEMBER 30 PLOT WITH YEAR ON X-AXIS
d <- df_plot %>%
  count(years = year_last_terminated, Outcome)  %>%
  group_by(years) %>%
  mutate(n = prop.table(n) * 100) %>%
  ggplot(aes(years, n, fill = Outcome)) +
  geom_col(position = 'dodge', na.rm = TRUE) + 
  ylim(0,100) +
  #  scale_fill_discrete(na.translate=FALSE) +
  labs(x = "Year", y = "Percentage of all pregnancy terminations") +
  scale_fill_brewer(palette = "Paired") +
  theme_cowplot(10)

d + scale_x_continuous(breaks=seq(2004,2021,1))


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




# Archive -----------------------------------------------------------------

#make DLHS districts into state_district match

range(df$district)

df$district_match <- df$district

# Using ICPSR district match code from STATA

df <- df %>% mutate(district_match = case_when(survey == "NFHS4" & district == 300 ~ 1000,
                                               survey == "NFHS4" & district == 303 ~ 1000,
                                               survey == "NFHS4" & district == 319 ~ 1000,
                                               survey == "NFHS4" & district == 320 ~ 1000,
                                               survey == "NFHS4" & district == 321 ~ 1000,
                                               survey == "NFHS4" & district == 322 ~ 1000,
                                               survey == "NFHS4" & district == 323 ~ 1000,
                                               survey == "NFHS4" & district == 324 ~ 1000,
                                               survey == "NFHS4" & district == 306 ~ 1001,
                                               survey == "NFHS4" & district == 325 ~ 1001,
                                               survey == "NFHS4" & district == 326 ~ 1001,
                                               survey == "NFHS4" & district == 239 ~ 1002,
                                               survey == "NFHS4" & district == 240 ~ 1002,
                                               survey == "NFHS4" & district == 414 ~ 1003,
                                               survey == "NFHS4" & district == 415 ~ 1003,
                                               survey == "NFHS4" & district == 416 ~ 1004,
                                               survey == "NFHS4" & district == 417 ~ 1004,
                                               survey == "NFHS4" & district == 358 ~ 1005,
                                               survey == "NFHS4" & district == 359 ~ 1005,
                                               survey == "NFHS4" & district == 360 ~ 1006,
                                               survey == "NFHS4" & district == 361 ~ 1006,
                                               survey == "NFHS4" & district == 362 ~ 1007,
                                               survey == "NFHS4" & district == 363 ~ 1007,
                                               survey == "NFHS4" & district == 364 ~ 1008,
                                               survey == "NFHS4" & district == 365 ~ 1008,
                                               survey == "NFHS4" & district == 366 ~ 1009,
                                               survey == "NFHS4" & district == 367 ~ 1009,
                                               survey == "NFHS4" & district == 368 ~ 1010,
                                               survey == "NFHS4" & district == 369 ~ 1010,
                                               survey == "NFHS4" & district == 458 ~ 1011,
                                               survey == "NFHS4" & district == 459 ~ 1011,
                                               survey == "NFHS4" & district == 460 ~ 1012,
                                               survey == "NFHS4" & district == 461 ~ 1012,
                                               survey == "NFHS4" & district == 462 ~ 1013,
                                               survey == "NFHS4" & district == 463 ~ 1013,
                                               survey == "NFHS4" & district == 464 ~ 1014,
                                               survey == "NFHS4" & district == 465 ~ 1014,
                                               survey == "NFHS4" & district == 466 ~ 1015,
                                               survey == "NFHS4" & district == 467 ~ 1015,
                                               survey == "NFHS4" & district == 125 ~ 1016,
                                               survey == "NFHS4" & district == 126 ~ 1016,
                                               survey == "NFHS4" & district == 130 ~ 1016,
                                               survey == "NFHS4" & district == 131 ~ 1016,
                                               survey == "NFHS4" & district == 201 ~ 1017,
                                               survey == "NFHS4" & district == 202 ~ 1017,
                                               TRUE ~ NA_real_))


#                                               survey == "NFHS5" & district == 300 ~ 1000,
#                                               survey == "NFHS5" & district == 303 ~ 1000,
#                                               survey == "NFHS5" & district == 319 ~ 1000,
#                                               survey == "NFHS5" & district == 320 ~ 1000,
#                                               survey == "NFHS5" & district == 321 ~ 1000,
#                                               survey == "NFHS5" & district == 322 ~ 1000,
#                                               survey == "NFHS5" & district == 323 ~ 1000,
#                                               survey == "NFHS5" & district == 324 ~ 1000,
#                                               survey == "NFHS5" & district == 306 ~ 1001,
#                                               survey == "NFHS5" & district == 325 ~ 1001,
#                                               survey == "NFHS5" & district == 326 ~ 1001,
#                                               survey == "NFHS5" & district == 239 ~ 1002,
#                                               survey == "NFHS5" & district == 240 ~ 1002,
#                                               survey == "NFHS5" & district == 414 ~ 1003,
#                                               survey == "NFHS5" & district == 415 ~ 1003,
#                                               survey == "NFHS5" & district == 416 ~ 1004,
#                                               survey == "NFHS5" & district == 417 ~ 1004,
#                                               survey == "NFHS5" & district == 358 ~ 1005,
#                                               survey == "NFHS5" & district == 359 ~ 1005,
#                                               survey == "NFHS5" & district == 360 ~ 1006,
#                                               survey == "NFHS5" & district == 361 ~ 1006,
#                                               survey == "NFHS5" & district == 362 ~ 1007,
#                                               survey == "NFHS5" & district == 363 ~ 1007,
#                                               survey == "NFHS5" & district == 364 ~ 1008,
#                                               survey == "NFHS5" & district == 365 ~ 1008,
#                                               survey == "NFHS5" & district == 366 ~ 1009,
#                                               survey == "NFHS5" & district == 367 ~ 1009,
#                                               survey == "NFHS5" & district == 368 ~ 1010,
#                                               survey == "NFHS5" & district == 369 ~ 1010,
#                                               survey == "NFHS5" & district == 458 ~ 1011,
#                                               survey == "NFHS5" & district == 459 ~ 1011,
#                                               survey == "NFHS5" & district == 460 ~ 1012,
#                                               survey == "NFHS5" & district == 461 ~ 1012,
#                                               survey == "NFHS5" & district == 462 ~ 1013,
#                                               survey == "NFHS5" & district == 463 ~ 1013,
#                                               survey == "NFHS5" & district == 464 ~ 1014,
#                                               survey == "NFHS5" & district == 465 ~ 1014,
#                                               survey == "NFHS5" & district == 466 ~ 1015,
#                                               survey == "NFHS5" & district == 467 ~ 1015,
#                                               survey == "NFHS5" & district == 125 ~ 1016,
#                                               survey == "NFHS5" & district == 126 ~ 1016,
#                                               survey == "NFHS5" & district == 130 ~ 1016,
#                                               survey == "NFHS5" & district == 131 ~ 1016,
#                                               survey == "NFHS5" & district == 201 ~ 1017,
#                                               survey == "NFHS5" & district == 202 ~ 1017,
#                                               ))

#adding in other ahs district variable. Right now the district variable is the nfhs4_census2011_district_id

#ahs_preg_match was read in from the ahs link file. 
ahs_preg_match <- ahs_preg_match %>% rename(dist_org = district)

ahs_preg_match <- ahs_preg_match %>% rename(district = nfhs4_census2011_district_id)

ahs_dist_link <- ahs_preg_match %>% select(c(caseid, state, dist_org, district,psu, outcome, yob, wt, survey, prev_stillbirth, survey))

#trying to see if leftjoin will work for just ahs
dftry <- left_join(
  df,
  ahs_preg_match
)


df$sdist <- ifelse(df$survey != "NFHS4" | df$survey != "NFHS5", (str_c(as.character(df$state), as.character(df$district), sep="")), df$district)


df <- df %>% mutate(district_id = case_when(survey == "DLHS3" & state_dist == dlhsnfhsahs$DLHS3_id ~ dlhsnfhsahs$dist_id,
                                            survey == "DLHS4" & state_dist == dlhsnfhsahs$DLHS4_id ~ dlhsnfhsahs$dist_id,
                                            survey == "NFHS4" & district == dlhsnfhsahs$NFHS_AHS_id ~ dlhsnfhsahs$dist_id,
                                            survey == "AHS" & district == dlhsnfhsahs$NFHS_AHS_id ~ dlhsnfhsahs$dist_id,
                                            survey == "NFHS5" & district == dlhsnfhsahs$NFHS5_id ~ dlhsnfhsahs$dist_id,
                                            TRUE ~ NA_real_))


#dlhsnfhs4ahs <- stringdist_join(nfhsahs, dlhs,
#                                by='namefix', #match based on fixed district name
#                                mode='left', #use left join
#                                method = "jw", #use jw distance metric
#                                max_dist=99,
#                                distance_col='dist') %>%
#  group_by(namefix.x) %>%
#  slice_min(order_by=dist, n=1)


#names <- stringdist_join(nfhsahs, dlhs3,
#                         by='district', #match based on district name
#                         mode='left', #use left join
#                         method = "jw", #use jw distance metric
#                         max_dist=99,
#                         distance_col='dist') %>%
#  group_by(district.x) %>%
#  slice_min(order_by=dist, n=1)

#nfhsnames <- stringdist_join(nfhs5, nfhsahs,
#                         by='district', #match based on district name
#                         mode='left', #use left join
#                         method = "jw", #use jw distance metric
#                         max_dist=99,
#                         distance_col='dist') %>%
#  group_by(district.x) %>%
#  slice_min(order_by=dist, n=1)
