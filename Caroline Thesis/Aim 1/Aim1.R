# Aim 1 import ------------------------------------------------------
library(tidyverse)
setwd("./Caroline Thesis/Aim 1")

df <- read.csv("harmonized_variables.csv")


# Limiting to districts that are represented in every survey --------------

table(df$survey, df$dist_id)

# Creating asset index ----------------------------------------------------


#The second way will be the polychoric 2-factor recommendation from Martel et al. 2021

#asset index variables:
# radio, mobile phone, land line, fridge, bike, motorcycle, animal cart, car, tv, type of water filter, 
# water source, toilet type, toilet shared, cooking fuel, has computer, owns land

#based on DHS guidance not using owning agricultural land as that would be less likely in rural areas

#making telephone land line 7 (not a dejure resident) into 0s.
df$telephone_land_line <- ifelse(df$telephone_land_line == 1, 1, 0)

#fixing coding for mobile phone. In all surveys 1 == yes
df$mobile_phone <- ifelse(df$mobile_phone == 1, 1, 0)

#making 96s into NAs
df$cooking_fuel <- ifelse(df$cooking_fuel == 96, NA, df$cooking_fuel)

#making type of toilet current 9s (other) as NA, and then recoding 0s (no toilet/field/outside) as 9s.

table(df$toilet_type)
df$toilet_type <- ifelse(df$toilet_type == 9, NA, df$toilet_type)
df$toilet_type <- ifelse(df$toilet_type == 0, 9, df$toilet_type)

df <- df %>% mutate(
  toilet_rev = case_when(
    toilet_type == 9 ~ 1,
    toilet_type == 8 ~ 2,
    toilet_type == 7 ~ 3,
    toilet_type == 6 ~ 4,
    toilet_type == 5 ~ 5,
    toilet_type == 4 ~ 6,
    toilet_type == 3 ~ 7,
    toilet_type == 2 ~ 8,
    toilet_type == 1 ~ 9
  )
)


asset <- df %>% select(c(caseid, rural_urban, water_treat, radio, mobile_phone, telephone_land_line, fridge, bike, motorcycle, 
                         animal_cart, car, #type_water_filter, 
                         water_source, toilet_rev, toilet_share, 
                         cooking_fuel, has_computer))

#examining for squared mmultiple correlations
smc <- psych::smc(asset[,3:16])
smc

#drop variables with less than 0.05 -- explain less than 5% of variance. In this case bike, radio and animal car

asset_smc <- asset %>% select(c(caseid, rural_urban, water_treat, mobile_phone, #telephone_land_line, 
                                fridge, motorcycle, 
                         car,
                         water_source, toilet_rev, toilet_share, 
                         cooking_fuel, has_computer))


#not enough responses for type of water filter

asset$type_water_filter <- as.factor(asset$type_water_filter)
asset$water_source <- as.factor(asset$water_source)
asset$toilet_type <- as.factor(asset$toilet_type)
asset$cooking_fuel <- as.factor(asset$cooking_fuel)

#loading package for PCA
library(factoextra)
library(psych)

asset.pca <- prcomp(na.omit(asset), scale = TRUE)
asset.pca

asset.pca.factors <- factoextra::get_pca(asset.pca, "var")

#make 7s in land line into NAs

prn<-psych::principal(asset_smc[,3:12], rotate="none", nfactors=2, cor = "mixed", 
                      covar=T, scores=TRUE, missing = TRUE)

index <- prn$scores[,2]

Assets.indexed<-mutate(asset_smc,wi_quintile=as.factor(cut(index,breaks=5,labels= c(1,2,3,4,5))),
                       wi_continuous = index)

length(which(is.na(Assets.indexed$quintile)))

ggplot(na.omit(Assets.indexed), aes(as.factor(rural_urban))) + geom_bar(aes(fill = quintile), position = "fill")+ xlab("state")+
  ylab("Percentage")+ggtitle("Wealth by Rural/Urban")

#adding continuous and quintile household wealth variables into full 

df_w_wi <- left_join(df, Assets.indexed)

#renaming

df <- df_w_wi

# Making binary pregnancy outcome variables -------------------------------

#creating binary outcomes for the following coding scheme: 
#1 LIVE BIRTH
#2 STILLBIRTH
#3 INDUCED ABORTION
#4 SPONTANEOUS ABORTION

table(df$outcome)

df$lb <- ifelse(df$outcome == 1, 1, 0)
df$sb <- ifelse(df$outcome == 2, 1, 0)
df$abort <- ifelse(df$outcome == 3, 1, 0)
df$miscarriage <- ifelse(df$outcome == 4, 1, 0)


# creating any insurance variable -----------------------------------------

# First harmonizing

df <- df %>% mutate(insurance = case_when(esis == 1 ~ 1,
                                          rsby == 1 ~ 1,
                                          cghsshis == 1 ~ 1,
                                          reimburse == 1 ~ 1, 
                                          chip == 1 ~ 1,
                                          other_private ==1 ~ 1, 
                                          other_insurance ==1 ~ 1,
                                          TRUE ~ 0))


# analyses ----------------------------------------------------------------

#calculating weights. For NFHS surveys weights must be divided by 1000000

#dropping 48 observations that do not have weights. making new dataset.
df <- df %>% filter(complete.cases(wt))

df <- df %>% mutate(weight = case_when(survey == "NFHS4" ~ wt/1000000,
                                       survey == "NFHS5" ~ wt/1000000,
                                       survey == "DLHS3" ~ wt,
                                       survey == "DLHS4" ~ wt,
                                       survey == "AHS" ~ wt,
                                       TRUE ~ NA_real_))

#weight adjusted. Adjusting weight by medium sample (NFHS4) 

df <- df %>% mutate(weight_adj = case_when(survey == "NFHS4" ~ weight * ((length(which(survey == "NFHS5"))) / 
                                                                            (length(which(survey == "NFHS4")))),
                                           survey == "DLHS3" ~ weight * ((length(which(survey == "NFHS5"))) / 
                                                                           (length(which(survey == "DLHS3")))),
                                           survey == "DLHS4" ~ weight * ((length(which(survey == "NFHS5"))) / 
                                                                           (length(which(survey == "DLHS3")))),
                                           survey == "NFHS5" ~ weight * ((length(which(survey == "NFHS5"))) / 
                                                                           (length(which(survey == "NFHS5")))),
                                           survey == "AHS" ~ weight * ((length(which(survey == "NFHS5"))) / 
                                                                         (length(which(survey == "AHS")))),
                                           TRUE ~ NA_real_))


# calculating jackknife and bootstrap weights in survey ---------------------------------------------------------

library(survey)

##### Run on cluster ######

weightdesign <- svydesign(data = df, ids = ~psu, strata = ~rural_urban, weights = ~weight_adj, nest = TRUE)

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

#making strata variable from rural/urban
df$strat_rurb <- ifelse(df$rural_urban == 0, 2, 1)

design <- svydesign(data = df, ids = ~psu, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)

#PSUs are repeated between NFHSs and DLHSs, have set nest = TRUE to account for that. Will look into this more.


complexglm_rate_glm_m <- svyglm(miscarriage ~ insurance + primary + caste_group + age + dist_id, design = design,
                            data = df)

complexglm_rate_glm_pred <- svyglm(miscarriage ~ insurance + caste_group + age + dist_id, design = design,
                                data = df)

pred_means_primary_m <- svypredmeans(complexglm_rate_glm_pred, ~factor(primary))

complexglm_rate_m_insurance <- svyglm(miscarriage ~ insurance + age + dist_id, design = design,
                                data = df)

tidy_m_insurance <- tidy(complexglm_rate_m_insurance, conf.int = T)

complexglm_rate_m_prim <- svyglm(miscarriage ~ primary + age + dist_id, design = design,
                                 data = df)

tidy_mprim <- tidy(complexglm_rate_m_prim, conf.int = T)

#complexglm_rate_m_urb <- svyglm(miscarriage ~  urban + age + state, design = design,
#                                data = df)

#tidy_murb <- tidy(complexglm_rate_m_urb, conf.int = T)

complexglm_rate_m_sch <- svyglm(miscarriage ~ caste_group + age + dist_id, design = design,
                                data = df)

tidy_msch <- tidy(complexglm_rate_m_sch, conf.int = T)


#library(margins)
#devtools::install_github("tzoltak/margins")
#complexglm_rate_m_log <- svyglm(miscarriage ~ age + state, design = design,
#                            family = quasibinomial(), data = df)


#complexglm_rate_a <- svyglm(abortion ~ bpl + primary + urban + scheduled_group + age + state, design = design,
#                            data = df)

complexglm_rate_a_insurance <- svyglm(abort ~ insurance + age + dist_id, design = design,
                                data = df)

tidy_a_insurance <- tidy(complexglm_rate_a_insurance, conf.int = TRUE)

complexglm_rate_a_prim <- svyglm(abort ~ primary + age + dist_id, design = design,
                                 data = df)

tidy_aprim <- tidy(complexglm_rate_a_prim, conf.int = T)

#complexglm_rate_a_urb <- svyglm(abortion ~ urban  + age + state, design = design,
#                                data = df)

#tidy_aurb <- tidy(complexglm_rate_a_urb, conf.int = T)

complexglm_rate_a_sch <- svyglm(abort ~ caste_group + age + dist_id, design = design,
                                data = df)

tidy_asch <- tidy(complexglm_rate_a_sch, conf.int = T)

#complexglm_rate_s <- svyglm(stillbirth ~ bpl + primary + urban + scheduled_group + age + state, design = design,
#                            data = df)

complexglm_rate_s_insurance <- svyglm(sb ~ insurance + age + dist_id, design = design,
                                data = df)

tidy_sinsurance <- tidy(complexglm_rate_s_insurance, conf.int = TRUE)

complexglm_rate_s_prim <- svyglm(sb ~ primary + age + dist_id, design = design,
                                 data = df)

tidy_sprim <- tidy(complexglm_rate_s_prim, conf.int = TRUE)

#complexglm_rate_s_urb <- svyglm(stillbirth ~ urban + age + state, design = design,
#                                data = df)

#tidy_surb <- tidy(complexglm_rate_s_urb, conf.int = TRUE)

complexglm_rate_s_sch <- svyglm(sb ~ caste_group + age + dist_id, design = design,
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

#ts <- tidy(complexglm_rate_s, conf.int = TRUE)
#ts$estimate <- ts$estimate*100
#ts$conf.low <- ts$conf.low*100
#ts$conf.high <- ts$conf.high*100

tidy_sinsurance$estimate <- tidy_sinsurance$estimate*100
tidy_sinsurance$conf.low <- tidy_sinsurance$conf.low*100
tidy_sinsurance$conf.high <- tidy_sinsurance$conf.high*100

tidy_sprim$estimate <- tidy_sprim$estimate*100
tidy_sprim$conf.low <- tidy_sprim$conf.low*100
tidy_sprim$conf.high <- tidy_sprim$conf.high*100

tidy_ssch$estimate <- tidy_ssch$estimate*100
tidy_ssch$conf.low <- tidy_ssch$conf.low*100
tidy_ssch$conf.high <- tidy_ssch$conf.high*100


#tm <- tidy(complexglm_rate_m, conf.int = TRUE)
#tm$estimate <- tm$estimate*100
#tm$conf.low <- tm$conf.low*100
#tm$conf.high <- tm$conf.high*100

tidy_m_insurance$estimate <- tidy_m_insurance$estimate*100
tidy_m_insurance$conf.low <- tidy_m_insurance$conf.low*100
tidy_m_insurance$conf.high <- tidy_m_insurance$conf.high*100

tidy_mprim$estimate <- tidy_mprim$estimate*100
tidy_mprim$conf.low <- tidy_mprim$conf.low*100
tidy_mprim$conf.high <- tidy_mprim$conf.high*100

tidy_msch$estimate <- tidy_msch$estimate*100
tidy_msch$conf.low <- tidy_msch$conf.low*100
tidy_msch$conf.high <- tidy_msch$conf.high*100

#ta <- tidy(complexglm_rate_a, conf.int = TRUE)
#ta$estimate <- ta$estimate*100
#ta$conf.low <- ta$conf.low*100
#ta$conf.high <- ta$conf.high*100

tidy_a_insurance$estimate <- tidy_a_insurance$estimate*100
tidy_a_insurance$conf.low <- tidy_a_insurance$conf.low*100
tidy_a_insurance$conf.high <- tidy_a_insurance$conf.high*100

tidy_aprim$estimate <- tidy_aprim$estimate*100
tidy_aprim$conf.low <- tidy_aprim$conf.low*100
tidy_aprim$conf.high <- tidy_aprim$conf.high*100

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
df_plot <- df %>% drop_na(outcome)

#making factor for labeling
df_plot$Outcome <- factor(df_plot$outcome, 
                          levels = c(1, 2, 3, 4),
                          labels = c("Live Birth", "Stillbirth", "Miscarriage", "Abortion"))




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
  count(years = outcome_year, Outcome)  %>%
  group_by(years) %>%
  mutate(n = prop.table(n) * 100) %>%
  ggplot(aes(years, n, fill = Outcome)) +
  geom_col(position = 'dodge', na.rm = TRUE) + 
  ylim(0,100) +
  #  scale_fill_discrete(na.translate=FALSE) +
  labs(x = "Year", y = "Percentage of all pregnancy outcomes") +
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
