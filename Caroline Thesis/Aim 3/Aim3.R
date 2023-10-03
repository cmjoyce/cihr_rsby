# Begin -------------------------------------------------------------------


library(tidyverse)
library(survey)
library(ggpubr)
library(flextable)
library(janitor)
library(AER)
library(broom)
library(gtsummary)
library(etwfe)
library(modelsummary)
library(fixest)
library(did)
library(data.table)
#library(kableExtra)
library(boot)
library(fwildclusterboot)
library(marginaleffects)



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
#df <- df_fixed

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
table(df_rsby$treat)
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

#creating leads and lags variable
df_rsby$timing <- df_rsby$outcome_year - df_rsby$g ## doesn't work for never-treated. ignoring for now.

#making separate dataset
#write.csv(df_rsby, "df_rsby.csv")

df_rsby <- read.csv("df_rsby.csv")

#redoing design
design_rsby <- svydesign(data = df_rsby, ids = ~psu2, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)




# Tables & Figures ------------------------------------------------------------------

df_rsby$ruralurban <- factor(df_rsby$strat_rurb, 
                        levels = c(1, 2),
                        labels = c("Urban", "Rural"))

df_rsby$scheduled_c_t <- factor(df_rsby$caste_group,
                           levels = c(0,1,2),
                           labels = c("None", "Scheduled Caste", "Scheduled Tribe"))
table(df_rsby$treat)
df_rsby$treated <- ifelse(df_rsby$treat > 0, 1, 0)
table(df_rsby$treated)

df_rsby$treated <- factor(df_rsby$treated,
                     levels = c(0,1),
                     labels = c("No", "Yes"))


t1_strat <- df_rsby %>% 
  select(treat_iv, age, ruralurban, scheduled_c_t, rsby_iv, primary_school) %>% 
  mutate(treat_iv = case_when(treated == "No" ~ "Never Treated",
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
                      rsby_iv ~ "Reported enrollment in RSBY"),
                  statistic = list(all_continuous() ~ "{mean} ({sd})"),
                  missing_text =  "Missing"
      )
  ) %>% modify_header(label = "**Variable**")  %>%  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman")

library(webshot)
gt::gtsave(t1_strat, "tab_1.png", expand = 10)

df_rsby$rsby_match_fact <- factor(df_rsby$rsby_iv,
                             levels = c(0,1),
                             labels = c("No", "Yes"))


df_rsby %>% 
  select(age, ruralurban, scheduled_c_t, rsby_match_fact, primary_school, g) %>% 
  tbl_summary(
    label = list(
      age ~ "Age",
      ruralurban ~ "Rural / Urban",
      scheduled_c_t ~ "Member of Scheduled Caste or Scheduled Tribe",
      primary_school ~ "Completed Primary School",
      rsby_match_fact ~ "Enrolled in RSBY at Survey Time",
      g ~ "District-level access to RSBY"),
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing_text =  "Missing") %>% modify_header(label = "**Variable**")  %>%  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman", #table.font.size = 22, 
                  column_labels.border.top.color = "black", column_labels.border.bottom.color = "black", table.border.bottom.color = "black", 
                  table_body.border.bottom.color = "black", table_body.hlines.color = "black" )

stage_2_full$rsby_match_fact <- factor(stage_2_full$rsby_iv,
                                  levels = c(0,1),
                                  labels = c("No", "Yes"))


stage_2_full %>% 
  select(age, ruralurban, scheduled_c_t, rsby_match_fact, primary_school, g) %>% 
  tbl_summary(
    label = list(
      age ~ "Age",
      ruralurban ~ "Rural / Urban",
      scheduled_c_t ~ "Member of Scheduled Caste or Scheduled Tribe",
      primary_school ~ "Completed Primary School",
      rsby_match_fact ~ "Enrolled in RSBY at Survey Time",
      g ~ "District-level access to RSBY"),
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing_text =  "Missing") %>% modify_header(label = "**Variable**")  %>%  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman", #table.font.size = 22, 
                  column_labels.border.top.color = "black", column_labels.border.bottom.color = "black", table.border.bottom.color = "black", 
                  table_body.border.bottom.color = "black", table_body.hlines.color = "black" )



#creating weighted counts by enrollment group
df_rsby$poorest <- ifelse(df_rsby$wi_quintile == 1, 1, 0)

df_rsby <- df_rsby %>% mutate(treat_group = case_when(treat_iv == 0 ~ "None",
                            treat_iv == 1 ~ "2010 - Early",
                            treat_iv == 2 ~ "2012 - Mid",
                            treat_iv == 3 ~ "2014 - Late"))

design_rsby <- svydesign(data = df_rsby, ids = ~psu2, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)

design_rsby %>%
  tbl_svysummary(
    by = treat_group,
    include = c(age, ruralurban, scheduled_c_t, primary_school, poorest),
    label = list(
      age ~ "Age",
      ruralurban ~ "Rural / Urban",
      scheduled_c_t ~ "Member of Scheduled Caste or Scheduled Tribe",
      primary_school ~ "Completed Primary School",
      poorest ~ "Lowest wealth quintile"),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n_unweighted} ({p}%)"),
    missing =  "no"
  ) %>% add_overall()%>%  
  modify_header(all_stat_cols() ~ "**{level}**  \n (N = {n_unweighted})") %>%  
  modify_spanning_header(all_stat_cols() ~ "**District-level Access**") %>%
  modify_footnote(all_stat_cols() ~ "Mean (SD); n (%)") %>%
  #modify_caption("**Table 1**. Socio-demographic information on respondents") %>%
  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman", #table.font.size = 22, 
                  column_labels.border.top.color = "black", column_labels.border.bottom.color = "black", table.border.bottom.color = "black", 
                  table_body.border.bottom.color = "black", table_body.hlines.color = "black" )
                  



# plotting percentage reporting enrollment in RSBY for each outcome year

year_enrollment <- df_rsby %>% group_by(outcome_year) %>% summarise(enrolled = mean(rsby_iv))

year_enrollment$percent_enrolled <- year_enrollment$enrolled*100

library(cowplot)

ggplot(data = year_enrollment, aes(x = outcome_year, y = percent_enrolled)) + geom_line() +
  scale_y_continuous(limits = c(0,100), n.breaks = 10) +
  scale_x_continuous(breaks = c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017,
                                2018, 2019), labels = c("2004", "2005", "2006", "2007", "2008", "2009", "2010",
                                                        "2011", "2012", "2013", "2014", "2015", "2016", "2017",
                                                        "2018", "2019"))+
  ylab("% Households enrolled in RSBY")+
  xlab("Year Pregnancy Occurred")+
  theme_cowplot()+
  theme(axis.text.x=element_text(angle=60, hjust=1))


# Wooldridge estimator for first-stage IV ---------------------------------

#July 6 2023 using ETWFE package for estimates

#step1_etwfe <- etwfe(
#  fml = rsby_iv ~ wi_perc_rank + strat_rurb, # outcome ~ controls
#  tvar = outcome_year, #time vars
#  gvar = g, #treatment group 
#  cgroup = "notyet",
#  vcov = ~ dist_id,
#  weights = ~ weight_adj,
#  data = df_rsby
#)

#trying with controls for scheduled caste/tribe + primary school. Not controlling for rural/urban
df_rsby <- df_rsby %>% filter(!is.na(caste_group)) %>% filter(!is.na(primary_school))
step1_etwfe <- etwfe(
  fml = rsby_iv ~ caste_group + wi_perc_rank,
    #caste_group + wi_perc_rank + primary_school, # outcome ~ controls
  tvar = outcome_year, #time vars
  gvar = g, #treatment group 
  cgroup = "notyet",
  vcov = ~ dist_id,
  weights = ~ weight_adj,
  data = df_rsby
)

#wald(step1_etwfe)
#hypotheses(step1_etwfe, joint = "g")

fitstat(step1_etwfe, "wf")

#linearHypothesis(step1_etwfe)

#looking at enrollment

rates_enrolled <- df_rsby %>% group_by(outcome_year, treat_iv) %>% 
  summarise(enrolled_prop = (mean(rsby_iv)*100))

#adding in 0s for groups
group_add <- data.frame(outcome_year = c(2004, 2005, 2006, 2007, 
                                         2004, 2005, 2006, 2007, 2008, 2009,
                                         2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011),
                        treat_iv = c(1,1,1,1,
                                     2,2,2,2,2,2,
                                     3,3,3,3,3,3,3,3),
                        enrolled_prop = c(0,0,0,0,
                                          0,0,0,0,0,0,
                                          0,0,0,0,0,0,0,0))

rates_figure <- rbind(rates_enrolled, group_add)

library(NatParksPalettes)

mycolors_groups <- c("#024b7a", "#44b7c2", "#458e48", "#e67e00")

ggplot(data = rates_figure, aes(x = outcome_year, y = enrolled_prop, color = as.factor(treat_iv))) + 
  geom_line() +
  scale_y_continuous(limits = c(0,100), n.breaks = 10) +
  scale_x_continuous(breaks = c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017,
                                2018, 2019), labels = c("2004", "2005", "2006", "2007", "2008", "2009", "2010",
                                                        "2011", "2012", "2013", "2014", "2015", "2016", "2017",
                                                        "2018", "2019"))+
  scale_color_manual(values=mycolors_groups,name = "District-Level Access", breaks = c(0, 1, 2,3), 
                     labels = c("None", "Early (2008-2010)", "Mid (2011-2012)", "Late (2013-2014)"))+
  ylab("% Households enrolled in RSBY")+
  xlab("Year Pregnancy Occurred")+
  theme_cowplot()+
  theme(axis.text.x=element_text(angle=60, hjust=1))



rates_enrolled <- rates_enrolled %>% pivot_wider(names_from = treat_iv, values_from = enrolled_prop)
rates_enrolled <- rates_enrolled %>% rename("Never-Adopter Districts" = "0", "Early-Adopter Districts" = "1", "Mid-Adopter Districts" = "2",
                                            "Late-Adopter Districts" = "3", "Year" = outcome_year)

rates_enrolled$`Never-Adopter Districts` <- ifelse(is.na(rates_enrolled$`Never-Adopter Districts`), 0, 
                                                  rates_enrolled$`Never-Adopter Districts`)
rates_enrolled$`Early-Adopter Districts` <- ifelse(is.na(rates_enrolled$`Early-Adopter Districts`), 0, 
                                                   rates_enrolled$`Early-Adopter Districts`)
rates_enrolled$`Mid-Adopter Districts` <- ifelse(is.na(rates_enrolled$`Mid-Adopter Districts`), 0, 
                                                 rates_enrolled$`Mid-Adopter Districts`)
rates_enrolled$`Late-Adopter Districts` <- ifelse(is.na(rates_enrolled$`Late-Adopter Districts`), 0, 
                                                  rates_enrolled$`Late-Adopter Districts`)

rates_enrolled <- rates_enrolled %>% round_half_up(digits = 1)



flextable(rates_enrolled) %>% colformat_num(j = 1, big.mark = "") %>% font(fontname = "Times New Roman", part = "all") %>%
  bold(bold = TRUE, part = "header")


#checking f-test with iv_robust
#library(estimatr)
#checkingfsb <- iv_robust(sb ~ rsby_iv + caste_group + wi_perc_rank + primary_school | caste_group + wi_perc_rank + primary_school + treat_iv, 
#                       data = df_rsby, diagnostics = TRUE)

#summary(checkingfsb)

#checkingfabort <- iv_robust(abort ~ rsby_iv + caste_group + wi_perc_rank + primary_school | caste_group + wi_perc_rank + primary_school + treat_iv, 
#                            data = df_rsby, diagnostics = TRUE)

#summary(checkingfabort)

#checkingfms <- iv_robust(miscarriage ~ rsby_iv + caste_group + wi_perc_rank + primary_school | caste_group + wi_perc_rank + primary_school + treat_iv, 
#                       data = df_rsby, diagnostics = TRUE)

#summary(checkingfms)

#check <- lm(rsby_iv ~ treat_iv+ caste_group + wi_perc_rank + primary_school, data = df_rsby)
#linearHypothesis(check, 
#                 "treat_iv = 0",
#                 vcov = vcovHC, type = "HC1")

#need to remove the 42 obs dropped from the feols before adding back in residuals

rows_to_delete <- step1_etwfe$obs_selection


df_rsby_full <- df_rsby %>% rowid_to_column()
rows_to_delete <- as.data.frame(step1_etwfe$obs_selection)
rows_to_delete$obsRemoved <- rows_to_delete$obsRemoved * -1
colnames(rows_to_delete) <- c("rowid")

cleaned_full_df_rsby_etwfe <- anti_join(df_rsby_full,rows_to_delete,by="rowid")


df_rsby_step1_etwfe <- (augment_columns(step1_etwfe, cleaned_full_df_rsby_etwfe)) %>% rename(pred_step1 = .fitted)

#writing csv
#write.csv(df_rsby_step1_etwfe, "df_rsby_step1_etwfe.csv")

df_rsby_step1_etwfe <- read.csv("df_rsby_step1_etwfe.csv")

#making factor for comparisons
dqrng::dqset.seed(781234)
set.seed(514)

df_rsby_step1_etwfe$treat_iv_fact <- factor(df_rsby_step1_etwfe$treat_iv)

#redoing design
design_rsby_step1_etwfe <- svydesign(data = df_rsby_step1_etwfe, ids = ~psu2, 
                                     strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)


stage_2_sb <- feols(sb ~ pred_step1 + strat_rurb + age
                    | treat_iv + outcome_year, 
                    cluster = ~dist_id, 
                    weights = ~weight_adj, data = df_rsby_step1_etwfe)

summary(stage_2_sb)

#getting F-statistic
wald(stage_2_sb)

#comparing to wooldridge did estimates

#have to remove caseid character column and use new rowid as ids
df_rsby_etwfe <- df_rsby_step1_etwfe %>% select(-c(rowid))
df_rsby_etwfe <- df_rsby_etwfe %>% rowid_to_column()
df_rsby_etwfe <- df_rsby_etwfe %>% select(-c(X, X.1, caseid))

#have to remove all character variables
df_rsby_etwfe <- df_rsby_etwfe %>% select(-c(survey, state_labeled, wiquint_labeled, namefix, District, State, treated, scheduled_c_t, 
                                             rsby_match_fact, rural_urban))

woold_sb <- etwfe(
  fml = sb ~ strat_rurb + age, # outcome ~ controls
  tvar = outcome_year, #time vars
  gvar = g, #treatment group 
  cgroup = "notyet",
  vcov = ~ dist_id,
  weights = ~ weight_adj,
  data = df_rsby_etwfe
)

emfx(woold_sb, type = "simple", collapse = FALSE)

woold_abort <- etwfe(
  fml = abort ~ strat_rurb + age, # outcome ~ controls
  tvar = outcome_year, #time vars
  gvar = g, #treatment group 
  cgroup = "notyet",
  vcov = ~ dist_id,
  weights = ~ weight_adj,
  data = df_rsby_etwfe
)

emfx(woold_abort, type = "simple", collapse = FALSE)

woold_miscarriage <- etwfe(
  fml = miscarriage ~ strat_rurb + age, # outcome ~ controls
  tvar = outcome_year, #time vars
  gvar = g, #treatment group 
  cgroup = "notyet",
  vcov = ~ dist_id,
  weights = ~ weight_adj,
  data = df_rsby_etwfe
)

emfx(woold_miscarriage, type = "simple", collapse = FALSE)

#need to remove 96,293 rows
#rows_to_delete_2 <- stage_2_sb$obs_selection

#df_rsby_step1_etwfe <- df_rsby_step1_etwfe %>% select(-c(rowid))
#df_rsby_step1_full <- df_rsby_step1_etwfe %>% rowid_to_column()
#rows_to_delete_2 <- as.data.frame(stage_2_sb$obs_selection)
#rows_to_delete_2$obsRemoved <- rows_to_delete_2$obsRemoved * -1
#colnames(rows_to_delete_2) <- c("rowid")

#stage_2_full <- anti_join(df_rsby_step1_full,rows_to_delete_2,by="rowid")
#write.csv(stage_2_full, "stage_2_full.csv")


stage_2_sb <- feols(sb ~ pred_step1 + strat_rurb + age  
                    | treat_iv + outcome_year, 
                    data = df_rsby_step1_etwfe,
                    cluster = ~dist_id, 
                    weights = ~weight_adj)

stage_2_sb_nocontr <- feols(sb ~ pred_step1
                    | treat_iv + outcome_year, 
                    data = df_rsby_step1_etwfe,
                    cluster = ~dist_id, 
                    weights = ~weight_adj)


#check <- svyglm(sb ~ pred_step1*treat_iv_fact + age + outcome_year + rural_urban, design = design_rsby_step1_etwfe)

#basic_check <- lm(sb ~ pred_step1*treat_iv_fact, data = stage_2_full)

sb_bootstrapped <- avg_comparisons(stage_2_sb, variables = "treat_iv") %>% inferences(method = "boot", R = 100)

summary(stage_2_sb)
summary(stage_2_sb_nocontr)

sb_iv <- tidy(stage_2_sb, conf.int = T)
sb_iv_nocontr <- tidy(stage_2_sb_nocontr, conf.int = T)

sb_iv_pp <- sb_iv %>% mutate(estimate_pp = estimate*100, std.error.pp = std.error*100, conf.low.pp = conf.low*100,
                             conf.high.pp = conf.high*100)

sb_iv_pp_nocontr <- sb_iv_nocontr %>% mutate(estimate_pp = estimate*100, std.error.pp = std.error*100, conf.low.pp = conf.low*100,
                             conf.high.pp = conf.high*100)


sb_iv_pp <- sb_iv_pp %>% select(c(term, estimate_pp, std.error.pp, conf.low.pp, conf.high.pp))
sb_iv_pp[2:5] <- sb_iv_pp[2:5] %>% round_half_up(digits = 2)
sb_iv_pp$outcome <- c("Stillbirth")
sb_iv_pp <- sb_iv_pp %>% filter(term == "pred_step1")

sb_iv_pp_nocontr <- sb_iv_pp_nocontr %>% select(c(term, estimate_pp, std.error.pp, conf.low.pp, conf.high.pp))
sb_iv_pp_nocontr[2:5] <- sb_iv_pp_nocontr[2:5] %>% round_half_up(digits = 2)
sb_iv_pp_nocontr$outcome <- c("Stillbirth")
sb_iv_pp_nocontr <- sb_iv_pp_nocontr %>% filter(term == "pred_step1")

#bootstrapped CI from cluster 0.0155 0.0274
sb_iv_pp$conf.low.pp.bootstrap <- 0.0038*100
sb_iv_pp$conf.high.pp.bootstrap <- 0.0229*100
sb_iv_pp <- sb_iv_pp %>% select(-c(conf.low.pp, conf.high.pp))

flextable(sb_iv_pp)

sb_iv[2:7] <- sb_iv[2:7] %>% round_half_up(digits = 5)
sb_iv$outcome <- c("Stillbirth")
flextable(sb_iv)

sb_iv_groups <- tidy(avg_comparisons(stage_2_sb, 
                                     variables = list(treat_iv_fact= "reference")), conf.int = T)

sb_iv_groups[3:9] <- sb_iv_groups[3:9] %>% round_half_up(digits = 4)
sb_iv_groups$outcome <- c("Stillbirth")

#all bootstrapping will be done on cluster

boot_sb <- boottest(
  stage_2_sb, 
  clustid = "dist_id", 
  param = "pred_step1", 
  B = 9999
)

stage2_sb <- avg_comparisons(stage_2_sb,  by = "treat_iv_fact", variables = "pred_step1") %>% 
  inferences(method = "boot", R = 100)

stage_2_abort <- feols(abort ~ pred_step1 + age + strat_rurb
                       | treat_iv + outcome_year, 
                       cluster = ~dist_id, 
                       weights = ~weight_adj, data = df_rsby_step1_etwfe)

wald(stage_2_abort)
summary(stage_2_abort)

stage_2_abort_nocontr <- feols(abort ~ pred_step1
                       | treat_iv + outcome_year, 
                       cluster = ~dist_id, 
                       weights = ~weight_adj, data = df_rsby_step1_etwfe)


summary(stage_2_abort)

avg_comparisons(stage_2_abort, 
                variables = list(treat_iv_fact= "reference")
) #%>% inferences(method = "boot", R = 10)


stage2_abort <- avg_predictions(stage_2, by = "g")

abort_iv <- tidy(stage_2_abort, conf.int = T)
abort_iv_nocontr <- tidy(stage_2_abort_nocontr, conf.int = T)

abort_iv_pp <- abort_iv %>% mutate(estimate_pp = estimate*100, std.error.pp = std.error*100, conf.low.pp = conf.low*100,
                             conf.high.pp = conf.high*100)

abort_iv_pp_nocontr <- abort_iv_nocontr %>% mutate(estimate_pp = estimate*100, std.error.pp = std.error*100, conf.low.pp = conf.low*100,
                                   conf.high.pp = conf.high*100)


abort_iv_pp <- abort_iv_pp %>% select(c(term, estimate_pp, std.error.pp, conf.low.pp, conf.high.pp))
abort_iv_pp[2:5] <- abort_iv_pp[2:5] %>% round_half_up(digits = 3)
abort_iv_pp$outcome <- c("Abortion")

abort_iv_pp <- abort_iv_pp %>% filter(term == "pred_step1")

#bootstrapped CI from cluster  0.0239 0.0633
abort_iv_pp$conf.low.pp.bootstrap <- 0.0239*100
abort_iv_pp$conf.high.pp.bootstrap <- 0.0633*100
abort_iv_pp <- abort_iv_pp %>% select(-c(conf.low.pp, conf.high.pp))


flextable(abort_iv_pp)


abort_iv[2:7] <- abort_iv[2:7] %>% round_half_up(digits = 4)
abort_iv$outcome <- c("Abortion")
flextable(abort_iv)

abort_iv_groups <- tidy(avg_comparisons(stage_2_abort, 
                                     variables = list(treat_iv_fact= "reference")), conf.int = T)

abort_iv_groups[3:9] <- abort_iv_groups[3:9] %>% round_half_up(digits = 5)
abort_iv_groups$outcome <- c("Abortion")

stage_2_miscarriage <- feols(miscarriage ~ pred_step1 + strat_rurb + age
                             | treat_iv + outcome_year, 
                             cluster = ~dist_id, 
                             weights = ~weight_adj, data = df_rsby_step1_etwfe)
summary(stage_2_miscarriage)

wald(stage_2_miscarriage)

stage_2_miscarriage_nocontr <- feols(miscarriage ~ pred_step1
                             | treat_iv + outcome_year, 
                             cluster = ~dist_id, 
                             weights = ~weight_adj, data = df_rsby_step1_etwfe)


summary(stage_2_miscarriage)
avg_comparisons(stage_2_miscarriage, 
                variables = list(treat_iv_fact= "reference")
) #%>% inferences(method = "boot", R = 10)

miscarriage_iv <- tidy(stage_2_miscarriage, conf.int = T)
miscarriage_iv_nocontr <- tidy(stage_2_miscarriage_nocontr, conf.int = T)


miscarriage_iv_pp <- miscarriage_iv %>% mutate(estimate_pp = estimate*100, std.error.pp = std.error*100, conf.low.pp = conf.low*100,
                                   conf.high.pp = conf.high*100)
miscarriage_iv_pp_nocontr <- miscarriage_iv_nocontr %>% mutate(estimate_pp = estimate*100, std.error.pp = std.error*100, conf.low.pp = conf.low*100,
                                               conf.high.pp = conf.high*100)



miscarriage_iv_pp <- miscarriage_iv_pp %>% select(c(term, estimate_pp, std.error.pp, conf.low.pp, conf.high.pp))
miscarriage_iv_pp_nocontr <- miscarriage_iv_pp_nocontr %>% select(c(term, estimate_pp, std.error.pp, conf.low.pp, conf.high.pp))
miscarriage_iv_pp[2:5] <- miscarriage_iv_pp[2:5] %>% round_half_up(digits = 3)
miscarriage_iv_pp$outcome <- c("Miscarriage")

miscarriage_iv_pp <- miscarriage_iv_pp %>% filter(term == "pred_step1")

#bootstrapped CI from cluster  0.0574 0.1111
miscarriage_iv_pp$conf.low.pp.bootstrap <- 0.0574*100
miscarriage_iv_pp$conf.high.pp.bootstrap <- 0.1111*100
miscarriage_iv_pp <- miscarriage_iv_pp %>% select(-c(conf.low.pp, conf.high.pp))


flextable(miscarriage_iv_pp)


miscarriage_iv[2:7] <- miscarriage_iv[2:7] %>% round_half_up(digits = 4)
miscarriage_iv$outcome <- c("Miscarriage")
flextable(miscarriage_iv)

miscarriage_iv_groups <- tidy(avg_comparisons(stage_2_miscarriage, 
                                        variables = list(treat_iv_fact= "reference")), conf.int = T)

miscarriage_iv_groups[3:9] <- miscarriage_iv_groups[3:9] %>% round_half_up(digits = 4)
miscarriage_iv_groups$outcome <- c("Miscarriage")


stage2_miscarriage <- avg_predictions(stage_2, by = "g")

#grouping contrasts for outcomes
contrasts_treatmentgroups <- rbind(sb_iv_groups, abort_iv_groups, miscarriage_iv_groups)
contrasts_treatmentgroups <- contrasts_treatmentgroups %>% select(-c(term))
contrasts_treatmentgroups <- contrasts_treatmentgroups %>% mutate(contrastgroup = case_when(
  contrast == "1 - 0" ~ "Early vs. Not",
  contrast == "2 - 0" ~ "Mid vs. Not",
  contrast == "3 - 0" ~ "Late vs. Not"
))

contrasts_treatmentgroups <- contrasts_treatmentgroups %>% select(-c(contrast))
contrasts_treatmentgroups <- contrasts_treatmentgroups %>% relocate(contrastgroup, .before = estimate)
flextable(contrasts_treatmentgroups)

#making forest plot of iv_pp estimates with bootrapped standard errors
iv_pp <- rbind(sb_iv_pp, abort_iv_pp, miscarriage_iv_pp)

ggplot(data = iv_pp, mapping = aes(x = outcome, y = estimate_pp#, color = outcome
                                   )) + 
  geom_point(size = 1.5#, position=position_dodge(width=0.5)
             ) + 
  geom_errorbar(aes(ymin = conf.low.pp.bootstrap, ymax = conf.high.pp.bootstrap), 
                width = 0.3, position=position_dodge(width=0.5)) + 
  #geom_hline(yintercept = 0, color = I("black"), linetype = 2)+
  xlab("Outcome")+
  ylab("Change in percentage points") +
  ylim(0,15)+
 # coord_flip()+
  theme_cowplot()


##### sensitivity analyses #####

table(df_rsby$treat_iv, df_rsby$rsby_iv)

df_rsby_sens <- df_rsby

df_rsby_sens <- df_rsby_sens %>% filter(!is.na(caste_group)) %>% filter(!is.na(primary_school))

#dropping 6,730 report enrolled in RSBY in not-treated districts

df_rsby_sens$rsby_iv <- ifelse(df_rsby_sens$treat_iv == 0 & df_rsby_sens$rsby_iv == 1, 0, df_rsby_sens$rsby_iv)

#trying with controls for scheduled caste/tribe + primary school. Not controlling for rural/urban

step1_etwfe_sens <- etwfe(
  fml = rsby_iv ~ caste_group + wi_perc_rank + primary_school, # outcome ~ controls
  tvar = outcome_year, #time vars
  gvar = g, #treatment group 
  cgroup = "notyet",
  vcov = ~ dist_id,
  weights = ~ weight_adj,
  data = df_rsby_sens
)

#need to remove the 42 obs dropped from the feols before adding back in residuals

rows_to_delete_sens <- step1_etwfe_sens$obs_selection


df_rsby_full_sens <- df_rsby_sens %>% rowid_to_column()
rows_to_delete_sens <- as.data.frame(step1_etwfe_sens$obs_selection)
rows_to_delete_sens$obsRemoved <- rows_to_delete_sens$obsRemoved * -1
colnames(rows_to_delete_sens) <- c("rowid")

cleaned_full_df_rsby_etwfe_sens <- anti_join(df_rsby_full_sens,rows_to_delete_sens,by="rowid")


df_rsby_step1_etwfe_sens <- (augment_columns(step1_etwfe_sens, cleaned_full_df_rsby_etwfe_sens)) %>% 
  rename(pred_step1 = .fitted)

#writing csv
#write.csv(df_rsby_step1_etwfe_sens, "df_rsby_step1_etwfe_sens.csv")

df_rsby_step1_etwfe_sens <- read.csv("df_rsby_step1_etwfe_sens.csv")

#making factor for comparisons
dqrng::dqset.seed(781234)
set.seed(514)

df_rsby_step1_etwfe_sens$treat_iv_fact <- factor(df_rsby_step1_etwfe_sens$treat_iv)

#redoing design
design_rsby_step1_etwfe_sens <- svydesign(data = df_rsby_step1_etwfe_sens, ids = ~psu2, 
                                     strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)


stage_2_sb_sens <- feols(sb ~ pred_step1 + strat_rurb + age
                    | treat_iv + outcome_year, 
                    cluster = ~dist_id, 
                    weights = ~weight_adj, data = df_rsby_step1_etwfe_sens)

#getting F-statistic
wald(stage_2_sb_sens)

#comparing to wooldridge did estimates

#have to remove caseid character column and use new rowid as ids
df_rsby_etwfe <- df_rsby_step1_etwfe %>% select(-c(rowid))
df_rsby_etwfe <- df_rsby_etwfe %>% rowid_to_column()
df_rsby_etwfe <- df_rsby_etwfe %>% select(-c(X, X.1, caseid))

#have to remove all character variables
df_rsby_etwfe <- df_rsby_etwfe %>% select(-c(survey, state_labeled, wiquint_labeled, namefix, District, State, treated, scheduled_c_t, 
                                             rsby_match_fact, rural_urban))

woold_sb <- etwfe(
  fml = sb ~ strat_rurb + age, # outcome ~ controls
  tvar = outcome_year, #time vars
  gvar = g, #treatment group 
  cgroup = "notyet",
  vcov = ~ dist_id,
  weights = ~ weight_adj,
  data = df_rsby_etwfe
)

emfx(woold_sb, type = "simple", collapse = FALSE)

woold_abort <- etwfe(
  fml = abort ~ strat_rurb + age, # outcome ~ controls
  tvar = outcome_year, #time vars
  gvar = g, #treatment group 
  cgroup = "notyet",
  vcov = ~ dist_id,
  weights = ~ weight_adj,
  data = df_rsby_etwfe
)

emfx(woold_abort, type = "simple", collapse = FALSE)

woold_miscarriage <- etwfe(
  fml = miscarriage ~ strat_rurb + age, # outcome ~ controls
  tvar = outcome_year, #time vars
  gvar = g, #treatment group 
  cgroup = "notyet",
  vcov = ~ dist_id,
  weights = ~ weight_adj,
  data = df_rsby_etwfe
)

emfx(woold_miscarriage, type = "simple", collapse = FALSE)

#need to remove 96,293 rows
#rows_to_delete_2 <- stage_2_sb$obs_selection

#df_rsby_step1_etwfe <- df_rsby_step1_etwfe %>% select(-c(rowid))
#df_rsby_step1_full <- df_rsby_step1_etwfe %>% rowid_to_column()
#rows_to_delete_2 <- as.data.frame(stage_2_sb$obs_selection)
#rows_to_delete_2$obsRemoved <- rows_to_delete_2$obsRemoved * -1
#colnames(rows_to_delete_2) <- c("rowid")

#stage_2_full <- anti_join(df_rsby_step1_full,rows_to_delete_2,by="rowid")
#write.csv(stage_2_full, "stage_2_full.csv")


stage_2_sb_sens <- feols(sb ~ pred_step1 + strat_rurb + age  
                    | treat_iv + outcome_year, 
                    data = df_rsby_step1_etwfe_sens,
                    cluster = ~dist_id, 
                    weights = ~weight_adj)

stage_2_sb_nocontr_sens <- feols(sb ~ pred_step1
                            | treat_iv + outcome_year, 
                            data = df_rsby_step1_etwfe_sens,
                            cluster = ~dist_id, 
                            weights = ~weight_adj)


#check <- svyglm(sb ~ pred_step1*treat_iv_fact + age + outcome_year + rural_urban, design = design_rsby_step1_etwfe)

#basic_check <- lm(sb ~ pred_step1*treat_iv_fact, data = stage_2_full)

#sb_bootstrapped <- avg_comparisons(stage_2_sb, variables = "treat_iv") %>% inferences(method = "boot", R = 100)

summary(stage_2_sb)
summary(stage_2_sb_nocontr)

sb_iv <- tidy(stage_2_sb, conf.int = T)
sb_iv_nocontr <- tidy(stage_2_sb_nocontr, conf.int = T)

sb_iv_pp <- sb_iv %>% mutate(estimate_pp = estimate*100, std.error.pp = std.error*100, conf.low.pp = conf.low*100,
                             conf.high.pp = conf.high*100)

sb_iv_pp_nocontr <- sb_iv_nocontr %>% mutate(estimate_pp = estimate*100, std.error.pp = std.error*100, conf.low.pp = conf.low*100,
                                             conf.high.pp = conf.high*100)


sb_iv_pp <- sb_iv_pp %>% select(c(term, estimate_pp, std.error.pp, conf.low.pp, conf.high.pp))
sb_iv_pp[2:5] <- sb_iv_pp[2:5] %>% round_half_up(digits = 2)
sb_iv_pp$outcome <- c("Stillbirth")
sb_iv_pp <- sb_iv_pp %>% filter(term == "pred_step1")

sb_iv_pp_nocontr <- sb_iv_pp_nocontr %>% select(c(term, estimate_pp, std.error.pp, conf.low.pp, conf.high.pp))
sb_iv_pp_nocontr[2:5] <- sb_iv_pp_nocontr[2:5] %>% round_half_up(digits = 2)
sb_iv_pp_nocontr$outcome <- c("Stillbirth")
sb_iv_pp_nocontr <- sb_iv_pp_nocontr %>% filter(term == "pred_step1")

#bootstrapped CI from cluster 0.0155 0.0274
sb_iv_pp$conf.low.pp.bootstrap <- 0.0038*100
sb_iv_pp$conf.high.pp.bootstrap <- 0.0229*100
sb_iv_pp <- sb_iv_pp %>% select(-c(conf.low.pp, conf.high.pp))

flextable(sb_iv_pp)

sb_iv[2:7] <- sb_iv[2:7] %>% round_half_up(digits = 5)
sb_iv$outcome <- c("Stillbirth")
flextable(sb_iv)

sb_iv_groups <- tidy(avg_comparisons(stage_2_sb, 
                                     variables = list(treat_iv_fact= "reference")), conf.int = T)

sb_iv_groups[3:9] <- sb_iv_groups[3:9] %>% round_half_up(digits = 4)
sb_iv_groups$outcome <- c("Stillbirth")

#all bootstrapping will be done on cluster

boot_sb <- boottest(
  stage_2_sb, 
  clustid = "dist_id", 
  param = "pred_step1", 
  B = 9999
)

stage2_sb <- avg_comparisons(stage_2_sb,  by = "treat_iv_fact", variables = "pred_step1") %>% 
  inferences(method = "boot", R = 100)

stage_2_abort_sens <- feols(abort ~ pred_step1 + age + strat_rurb
                       | treat_iv + outcome_year, 
                       cluster = ~dist_id, 
                       weights = ~weight_adj, data = df_rsby_step1_etwfe_sens)

wald(stage_2_abort_sens)

stage_2_abort_nocontr_sens <- feols(abort ~ pred_step1
                               | treat_iv + outcome_year, 
                               cluster = ~dist_id, 
                               weights = ~weight_adj, data = df_rsby_step1_etwfe_sens)


summary(stage_2_abort_sens)

avg_comparisons(stage_2_abort, 
                variables = list(treat_iv_fact= "reference")
) #%>% inferences(method = "boot", R = 10)


stage2_abort <- avg_predictions(stage_2, by = "g")

abort_iv <- tidy(stage_2_abort, conf.int = T)
abort_iv_nocontr <- tidy(stage_2_abort_nocontr, conf.int = T)

abort_iv_pp <- abort_iv %>% mutate(estimate_pp = estimate*100, std.error.pp = std.error*100, conf.low.pp = conf.low*100,
                                   conf.high.pp = conf.high*100)

abort_iv_pp_nocontr <- abort_iv_nocontr %>% mutate(estimate_pp = estimate*100, std.error.pp = std.error*100, conf.low.pp = conf.low*100,
                                                   conf.high.pp = conf.high*100)


abort_iv_pp <- abort_iv_pp %>% select(c(term, estimate_pp, std.error.pp, conf.low.pp, conf.high.pp))
abort_iv_pp[2:5] <- abort_iv_pp[2:5] %>% round_half_up(digits = 3)
abort_iv_pp$outcome <- c("Abortion")

abort_iv_pp <- abort_iv_pp %>% filter(term == "pred_step1")

#bootstrapped CI from cluster  0.0239 0.0633
abort_iv_pp$conf.low.pp.bootstrap <- 0.0239*100
abort_iv_pp$conf.high.pp.bootstrap <- 0.0633*100
abort_iv_pp <- abort_iv_pp %>% select(-c(conf.low.pp, conf.high.pp))


flextable(abort_iv_pp)


abort_iv[2:7] <- abort_iv[2:7] %>% round_half_up(digits = 4)
abort_iv$outcome <- c("Abortion")
flextable(abort_iv)

abort_iv_groups <- tidy(avg_comparisons(stage_2_abort, 
                                        variables = list(treat_iv_fact= "reference")), conf.int = T)

abort_iv_groups[3:9] <- abort_iv_groups[3:9] %>% round_half_up(digits = 5)
abort_iv_groups$outcome <- c("Abortion")

stage_2_miscarriage_sens <- feols(miscarriage ~ pred_step1 + strat_rurb + age
                             | treat_iv + outcome_year, 
                             cluster = ~dist_id, 
                             weights = ~weight_adj, data = df_rsby_step1_etwfe_sens)


stage_2_miscarriage_nocontr <- feols(miscarriage ~ pred_step1
                                     | treat_iv + outcome_year, 
                                     cluster = ~dist_id, 
                                     weights = ~weight_adj, data = df_rsby_step1_etwfe_sens)


summary(stage_2_miscarriage_sens)
avg_comparisons(stage_2_miscarriage, 
                variables = list(treat_iv_fact= "reference")
) #%>% inferences(method = "boot", R = 10)

miscarriage_iv <- tidy(stage_2_miscarriage, conf.int = T)
miscarriage_iv_nocontr <- tidy(stage_2_miscarriage_nocontr, conf.int = T)


miscarriage_iv_pp <- miscarriage_iv %>% mutate(estimate_pp = estimate*100, std.error.pp = std.error*100, conf.low.pp = conf.low*100,
                                               conf.high.pp = conf.high*100)
miscarriage_iv_pp_nocontr <- miscarriage_iv_nocontr %>% mutate(estimate_pp = estimate*100, std.error.pp = std.error*100, conf.low.pp = conf.low*100,
                                                               conf.high.pp = conf.high*100)



miscarriage_iv_pp <- miscarriage_iv_pp %>% select(c(term, estimate_pp, std.error.pp, conf.low.pp, conf.high.pp))
miscarriage_iv_pp_nocontr <- miscarriage_iv_pp_nocontr %>% select(c(term, estimate_pp, std.error.pp, conf.low.pp, conf.high.pp))
miscarriage_iv_pp[2:5] <- miscarriage_iv_pp[2:5] %>% round_half_up(digits = 3)
miscarriage_iv_pp$outcome <- c("Miscarriage")

miscarriage_iv_pp <- miscarriage_iv_pp %>% filter(term == "pred_step1")

#bootstrapped CI from cluster  0.0574 0.1111
miscarriage_iv_pp$conf.low.pp.bootstrap <- 0.0574*100
miscarriage_iv_pp$conf.high.pp.bootstrap <- 0.1111*100
miscarriage_iv_pp <- miscarriage_iv_pp %>% select(-c(conf.low.pp, conf.high.pp))


flextable(miscarriage_iv_pp)


miscarriage_iv[2:7] <- miscarriage_iv[2:7] %>% round_half_up(digits = 4)
miscarriage_iv$outcome <- c("Miscarriage")
flextable(miscarriage_iv)

miscarriage_iv_groups <- tidy(avg_comparisons(stage_2_miscarriage, 
                                              variables = list(treat_iv_fact= "reference")), conf.int = T)

miscarriage_iv_groups[3:9] <- miscarriage_iv_groups[3:9] %>% round_half_up(digits = 4)
miscarriage_iv_groups$outcome <- c("Miscarriage")


stage2_miscarriage <- avg_predictions(stage_2, by = "g")


#### bootstrapping estimates and standard error ####


library(boot)
set.seed(123)

bootfun <- function(data, indices, formula) {
  d <- data[indices, ]
  mod <- feols(sb ~ pred_step1*treat_iv_fact + primary_school + wi_perc_rank + strat_rurb + age  
               | dist_id + outcome_year, 
               cluster = ~dist_id, 
               weights = ~weight_adj, data = d)
  cmp <- comparisons(mod, newdata = d, vcov = FALSE, variables = "treat_iv_fact")
  tidy(cmp)$estimate
}

b <- boot(data = stage_2_full, statistic = bootfun, R = 10)


# Comparing CS and Wooldridge ---------------------------------------------

#C/S
#from Aim 2 file
dists_df <- read.csv("dist_rates_weighted.csv")
names(dists_df)

table(dists_df$dist_id, dists_df$outcome_year)
dists_df <- dists_df %>% select(-c(X))

dists_df$sb_rate <- (dists_df$sb / dists_df$pregs)*1000
dists_df$abort_rate <- (dists_df$abort / dists_df$pregs)*1000
dists_df$ms_rate <- (dists_df$ms / dists_df$pregs)*1000


#using etwfe package
mod_package  <- etwfe(
  fml  = sb_rate ~ 0, # outcome ~ controls
  tvar = outcome_year,        # time variable
  gvar = enrollgroup, # group variable
  ivar = dist_id,
  data = dists_df,       # dataset
  vcov = ~dist_id,  # vcov adjustment (here: clustered)
  cgroup = "notyet"
)

emfx(mod_package, type = "simple")

csa_sb_check <- att_gt(yname = "sb_rate",
                 gname = "enrollgroup",
                 idname = "dist_id",
                 tname = "outcome_year",
                 control_group = "notyettreated",
                 panel = TRUE,
                 clustervars = "dist_id",
                 data = dists_df
)

aggte(csa_sb_check, type = "simple")


#now comparing using sample data from did package

#data("mpdta", package = "did")
mod =
  etwfe(
    fml  = lemp ~ 1, # outcome ~ controls
    tvar = year,        # time variable
    gvar = first.treat, # group variable
    data = mpdta,       # dataset
    vcov = ~countyreal,  # vcov adjustment (here: clustered)
    cgroup = "notyet"
  )

mod_att <- emfx(mod)

mw.attgt <- att_gt(yname = "lemp",
                   gname = "first.treat",
                   idname = "countyreal",
                   tname = "year",
                   #xformla = ~1,
                   clustervars = "countyreal",
                   data = mpdta,
                   panel = TRUE,
                   control_group = "notyettreated"
)

mw_att <- aggte(mw.attgt, type = "simple")

mod_sb <- etwfe(
  fml  = sb_rate ~ 0, # outcome ~ controls
  tvar = outcome_year,        # time variable
  gvar = enrollgroup, # group variable
  #ivar = dist_id,
  data = dists_df,       # dataset
  #vcov = ~dist_id,  # vcov adjustment (here: clustered)
  cgroup = "notyet"
)

#mod_sb = as.data.table(tidy(mod_sb))
#mod_sb = mod_sb[
#  , .(term = term, mod_sb = estimate)][
#    , group := as.numeric(gsub(".*enrollgroup.", "", term))][
#      , year := as.numeric(gsub(".*outcome_year.(\\d+).*", "\\1", term)) + group][
#        , .(group, year, mod_sb)]

mod_att_sb <- emfx(mod_sb)
mod_att_sb <- tidy(mod_att_sb)
mod_att_sb <- mod_att_sb %>% select(c(estimate, std.error, conf.low, conf.high)) %>% rename(att = estimate)
mod_att_sb$outcome <- c("stillbirth")
mod_att_sb$type <- c("etwfe")


mw.attgt_sb <- att_gt(yname = "sb_rate",
                   gname = "enrollgroup",
                   idname = "dist_id",
                   tname = "outcome_year",
                   #xformla = ~1,
                   panel = TRUE,
                   #clustervars = "dist_id",
                   data = dists_df,
                   control_group = "notyettreated"
)

#mw.attgt_sb = data.table(group = mw.attgt_sb$group, year = mw.attgt_sb$t, mw.attgt_sb = mw.attgt_sb$att)

mw_att_sb <- aggte(mw.attgt_sb, type = "simple")
mw_att_sb <- tidy(mw_att_sb) 
mw_att_sb <- mw_att_sb %>% select(c(estimate, std.error, conf.low, conf.high)) %>% rename(att = estimate)
mw_att_sb$outcome <- c("stillbirth")
mw_att_sb$type <- c("c/s'a")



#now trying with 3 year bins to see if we get less noisy estimates

dists_df <- dists_df %>% mutate(year_bin_big = (case_when(outcome_year < 2008 ~ 1,
                                          outcome_year > 2007 & outcome_year < 2011 ~ 2,
                                          outcome_year > 2010 & outcome_year < 2013 ~ 3,
                                          outcome_year > 2012 & outcome_year < 2015 ~ 4,
                                          outcome_year > 2014 & outcome_year < 2017 ~ 5,
                                          outcome_year > 2016 ~ 6,
                                          TRUE ~ NA_real_)))

#putting treatment / enrollment groups in same format as year_bin for dummies
dists_df <- dists_df %>% mutate(treat_bin_big = case_when(enrollgroup == 0 ~ 0,
                                                      enrollgroup == 2010 ~ 2,
                                                      enrollgroup == 2012 ~ 3,
                                                      enrollgroup == 2014 ~ 4))

mod_sb_bins <- etwfe(
  fml  = sb_rate ~ 0, # outcome ~ controls
  tvar = year_bin_big,        # time variable
  gvar = treat_bin_big, # group variable
  #ivar = dist_id,
  data = dists_df,       # dataset
  #vcov = ~dist_id,  # vcov adjustment (here: clustered)
  cgroup = "notyet"
)

mod_att_sb_bins <- emfx(mod_sb_bins)

mod_att_sb_binnedyear <- emfx(mod_sb_bins)
mod_att_sb_binnedyear <- tidy(mod_att_sb_binnedyear)
mod_att_sb_binnedyear <- mod_att_sb_binnedyear %>% select(c(estimate, std.error, conf.low, conf.high)) %>% 
  rename(att = estimate)
mod_att_sb_binnedyear$outcome <- c("stillbirth 2-year bins")
mod_att_sb_binnedyear$type <- c("etwfe 2-year bins")

mw.attgt_sb_bins <- att_gt(yname = "sb_rate",
                      gname = "treat_bin_big",
                      idname = "dist_id",
                      tname = "year_bin_big",
                      #xformla = ~1,
                      panel = FALSE,
                      #clustervars = "dist_id",
                      data = dists_df,
                      control_group = "notyettreated"
)

mw_att_sb_bins <- aggte(mw.attgt_sb_bins, type = "simple")

mw_att_sb_binnedyear <- tidy(mw_att_sb_bins)
mw_att_sb_binnedyear <- mw_att_sb_binnedyear %>% select(c(estimate, std.error, conf.low, conf.high)) %>% 
  rename(att = estimate)
mw_att_sb_binnedyear$outcome <- c("stillbirth 2-year bins")
mw_att_sb_binnedyear$type <- c("c/s'a 2-year bins")

mod_abort <- etwfe(
  fml  = abort_rate ~ 1, # outcome ~ controls
  tvar = outcome_year,        # time variable
  gvar = enrollgroup, # group variable
  #ivar = dist_id,
  data = dists_df,       # dataset
  #vcov = ~dist_id,  # vcov adjustment (here: clustered)
  cgroup = "notyet"
)

mod_att_abort <- emfx(mod_abort)

mod_att_abort <- tidy(mod_att_abort)
mod_att_abort <- mod_att_abort %>% select(c(estimate, std.error, conf.low, conf.high)) %>% rename(att = estimate)
mod_att_abort$outcome <- c("abortion")
mod_att_abort$type <- c("etwfe")


mw.attgt_abort <- att_gt(yname = "abort_rate",
                      gname = "enrollgroup",
                      idname = "dist_id",
                      tname = "outcome_year",
                      #xformla = ~1,
                      panel = TRUE,
                      #clustervars = "dist_id",
                      data = dists_df,
                      control_group = "notyettreated"
)

mw_att_abort <- aggte(mw.attgt_abort, type = "simple")

mw_att_abort <- tidy(mw_att_abort)
mw_att_abort <- mw_att_abort %>% select(c(estimate, std.error, conf.low, conf.high)) %>% rename(att = estimate)
mw_att_abort$outcome <- c("abortion")
mw_att_abort$type <- c("c/s'a")


mod_ms <- etwfe(
  fml  = ms_rate ~ 1, # outcome ~ controls
  tvar = outcome_year,        # time variable
  gvar = enrollgroup, # group variable
  #ivar = dist_id,
  data = dists_df,       # dataset
  #vcov = ~dist_id,  # vcov adjustment (here: clustered)
  cgroup = "notyet"
)


mod_att_ms <- emfx(mod_ms)

mod_att_ms <- tidy(mod_att_ms)
mod_att_ms <- mod_att_ms %>% select(c(estimate, std.error, conf.low, conf.high)) %>% rename(att = estimate)
mod_att_ms$outcome <- c("miscarriage")
mod_att_ms$type <- c("etwfe")

mw.attgt_ms <- att_gt(yname = "ms_rate",
                      gname = "enrollgroup",
                      idname = "dist_id",
                      tname = "outcome_year",
                      #xformla = ~1,
                      panel = TRUE,
                      #clustervars = "dist_id",
                      data = dists_df,
                      control_group = "notyettreated"
)

mw_att_ms <- aggte(mw.attgt_ms, type = "simple")

mw_att_ms <- tidy(mw_att_ms)
mw_att_ms <- mw_att_ms %>% select(c(estimate, std.error, conf.low, conf.high)) %>% rename(att = estimate)
mw_att_ms$outcome <- c("miscarriage")
mw_att_ms$type <- c("c/s'a")


#now combining all estimates into table
estimates <- rbind(mod_att_sb, mw_att_sb, mod_att_sb_binnedyear, mw_att_sb_binnedyear,
                  mod_att_abort, mw_att_abort, mod_att_ms, mw_att_ms)

estimates[1:4] <- estimates[1:4] %>% round_half_up(digits = 2)


flextable(estimates)

#checking and doing by hand

mod_ms$fml_all

dists_df2 = dists_df |>
  transform(
    .Dtreat = outcome_year >= enrollgroup & enrollgroup != 0
  )


# Then estimate the manual version of etwfe
mod2 = fixest::feols(
  ms_rate ~ .Dtreat:i(enrollgroup, i.outcome_year, ref = 0, ref2 = 2004) |
    dist_id + outcome_year,
  data = dists_df2,
  vcov = ~dist_id
)

library(modelsummary)
modelsummary(
  list("etwfe" = mod_ms, "manual" = mod2),
  gof_map = NA # drop all goodness-of-fit info for brevity
)

#now create comparisons for etwfe vs. att_gt for each outcome

modelsummary(
  list("etwfe stillbirth" = mod_sb, "c/s'a stillbirth" = mw.attgt_sb, 
       "etwfe stillbirth 2-year bins" = mod_att_sb_bins, "c/s'a stillbirth 2-year bins" = mw.attgt_sb_bins,
       "etwfe abortion" = mod_abort, "c/s'a abortion" = mw.attgt_abort,
       "etwfe miscarriage" = mod_ms, "c/s'a miscarriage" = mw.attgt_ms),
  estimate  = "{estimate} [{conf.low}, {conf.high}]",
  gof_map = NA
)

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

#### Survey IV Reg ####
#design_rsby <- svydesign(data = df_rsby, ids = ~psu2, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)

sb_iv <- svyivreg(sb ~ rsby_iv + age + rural_urban + wi_perc_rank + as.factor(outcome_year) + dist_id
                  | . - rsby_iv + treat_iv, design = design_rsby)

abort_iv <- svyivreg(abort ~ rsby_iv + age + rural_urban + wi_perc_rank + as.factor(outcome_year) + dist_id
                     | . - rsby_iv + treat_iv, design = design_rsby)

ms_iv <- svyivreg(miscarriage ~ rsby_iv + age + rural_urban + wi_perc_rank + as.factor(outcome_year) + dist_id
                  | . - rsby_iv + treat_iv, design = design_rsby)

summary(sb_iv)
summary(abort_iv)
summary(ms_iv)

sb_iv <- tidy(sb_iv, conf.int = 0.95)
abort_iv <- tidy(abort_iv, conf.int = 0.95)
ms_iv <- tidy(ms_iv, conf.int = 0.95)


sb_iv$est_per_1000 <- (sb_iv$estimate * 1000) 
sb_iv$std.error_per1000 <- sb_iv$std.error*1000 
sb_iv$conf.low_per1000 <- sb_iv$conf.low*1000
sb_iv$conf.high_per1000 <- sb_iv$conf.high*1000
sb_iv[8:11] <- sb_iv[8:11] %>% round_half_up(digits = 2)

sb_iv_tab <- sb_iv %>% select(c(term, est_per_1000, std.error_per1000, conf.low_per1000, conf.high_per1000))
flextable(sb_iv_tab)

abort_iv$est_per_1000 <- (abort_iv$estimate * 1000) 
abort_iv$std.error_per1000 <- abort_iv$std.error*1000 
abort_iv$conf.low_per1000 <- abort_iv$conf.low*1000
abort_iv$conf.high_per1000 <- abort_iv$conf.high*1000
abort_iv[8:11] <- abort_iv[8:11] %>% round_half_up(digits = 2)

abort_iv_tab <- abort_iv %>% select(c(term, est_per_1000, std.error_per1000, conf.low_per1000, conf.high_per1000))
flextable(abort_iv_tab)


ms_iv$est_per_1000 <- (ms_iv$estimate * 1000) 
ms_iv$std.error_per1000 <- ms_iv$std.error*1000 
ms_iv$conf.low_per1000 <- ms_iv$conf.low*1000
ms_iv$conf.high_per1000 <- ms_iv$conf.high*1000
ms_iv[8:11] <- ms_iv[8:11] %>% round_half_up(digits = 2)

ms_iv_tab <- ms_iv %>% select(c(term, est_per_1000, std.error_per1000, conf.low_per1000, conf.high_per1000))
flextable(ms_iv_tab)


#### Wooldridge IV estimators ####
#creating leads and lags variable
df_rsby$timing <- df_rsby$outcome_year - df_rsby$g 

df_rsby$timing <- ifelse(df_rsby$g == 0, -999, df_rsby$timing)

df_rsby$timing_fact <- as.factor(df_rsby$timing)


#update Treat_IV 
df_rsby <- df_rsby %>% mutate(dist_acces_int = case_when(outcome_year < 2008 ~ 0,
                                                         outcome_year > 2007 & g == 2010 ~ 1,
                                                         outcome_year < 2011 & g > 2010 ~ 0,
                                                         outcome_year > 2010 & g == 2012 ~ 1,
                                                         outcome_year > 2012 & g == 2012 ~ 0,
                                                         outcome_year > 2012 & g == 2014 ~ 1,
                                                         TRUE ~ NA_real_))

df_rsby$dist_acces_fact <- as.factor(df_rsby$dist_acces_int)

df_rsby$group_fact <- as.factor(df_rsby$treat)
df_rsby$year_fact <- as.factor(df_rsby$outcome_year)

#redoing design
design_rsby <- svydesign(data = df_rsby, ids = ~psu2, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)

stage1 <- svyglm(rsby_iv ~ (dist_acces_int*timing_fact*treat) + outcome_year + dist_id + 
                   age + rural_urban + wi_perc_rank, design = design_rsby, family = quasibinomial())

library(fixest)

#factor(timing) 
# factor(outcome_year)*factor(group) just during + after

#making new year timing variable for interactions. Only implementation year + after
df_rsby$year_wooldridge <- ifelse(df_rsby$outcome_year < df_rsby$g, 0, df_rsby$outcome_year)
#dropping 0s for factor variable
df_rsby$year_wooldridge <- ifelse(df_rsby$year_wooldridge == 0, NA, df_rsby$year_wooldridge)

#making 0 group NAs for interactions
df_rsby$group <- ifelse(df_rsby$g == 0, NA, df_rsby$g)

#hand-coding dummies to check
df_rsby$y2008 <- ifelse(df_rsby$outcome_year == 2008, 1, 0)
df_rsby$y2009 <- ifelse(df_rsby$outcome_year == 2009, 1, 0)
df_rsby$y2010 <- ifelse(df_rsby$outcome_year == 2010, 1, 0)
df_rsby$y2011 <- ifelse(df_rsby$outcome_year == 2011, 1, 0)
df_rsby$y2012 <- ifelse(df_rsby$outcome_year == 2012, 1, 0)
df_rsby$y2013 <- ifelse(df_rsby$outcome_year == 2013, 1, 0)
df_rsby$y2014 <- ifelse(df_rsby$outcome_year == 2014, 1, 0)
df_rsby$y2015 <- ifelse(df_rsby$outcome_year == 2015, 1, 0)
df_rsby$y2016 <- ifelse(df_rsby$outcome_year == 2016, 1, 0)
df_rsby$y2017 <- ifelse(df_rsby$outcome_year == 2017, 1, 0)
df_rsby$y2018 <- ifelse(df_rsby$outcome_year == 2018, 1, 0)
df_rsby$y2019 <- ifelse(df_rsby$outcome_year == 2019, 1, 0)

df_rsby$group1 <- ifelse(df_rsby$g == 2010, 1, 0)
df_rsby$group2 <- ifelse(df_rsby$g == 2012, 1, 0)
df_rsby$group3 <- ifelse(df_rsby$g == 2014, 1, 0)

step_1 <- feols(rsby_iv ~ (dist_acces_int : factor(timing) : factor(g)) | outcome_year + g,
                cluster = ~dist_id,
                weights = ~weight_adj, data = df_rsby)

step_1_att <- avg_predictions(step_1)

summary(step_1)
glance(step_1)
wald(step_1)

step1_hand_code <- feols(rsby_iv ~ (group1:y2010) +
                           (group1:y2011) + (group1:y2012) + (group1:y2013) + (group1:y2014) + (group1:y2015)+
                           (group1:y2016) + (group1:y2017) + (group1:y2018) + (group1:y2019) +
                           (group2:y2012) + (group2:y2013) + (group2:y2014) + (group2:y2015)+
                           (group2:y2016) + (group2:y2017) + (group2:y2018) + (group2:y2019) +
                           (group3:y2012) + (group3:y2013) + (group3:y2014) + (group3:y2015)+
                           (group3:y2016) + (group3:y2017) + (group3:y2018) + (group3:y2019) |
                           outcome_year + g, cluster = ~ dist_id, weights = ~weight_adj, data = df_rsby)
#need to remove the 215,978 obs dropped from the feols before adding back in residuals
rows_to_delete <- step_1$obs_selection

df_rsby_full <- df_rsby %>% rowid_to_column()
rows_to_delete <- as.data.frame(step_1$obs_selection)
rows_to_delete$obsRemoved <- rows_to_delete$obsRemoved * -1

colnames(rows_to_delete) <- c("rowid")

cleaned_full_df_rsby <- anti_join(df_rsby_full,rows_to_delete,by="rowid")

with_pred <- augment_columns(step_1, cleaned_full_df_rsby) %>% rename(pred_marg = .fitted)
head(with_pred)

#design_stage1 <- svydesign(data = with_pred, ids = ~psu2, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)
#hand-coding dummies to check
dists_df$y2008 <- ifelse(dists_df$outcome_year == 2008, 1, 0)
dists_df$y2009 <- ifelse(dists_df$outcome_year == 2009, 1, 0)
dists_df$y2010 <- ifelse(dists_df$outcome_year == 2010, 1, 0)
dists_df$y2011 <- ifelse(dists_df$outcome_year == 2011, 1, 0)
dists_df$y2012 <- ifelse(dists_df$outcome_year == 2012, 1, 0)
dists_df$y2013 <- ifelse(dists_df$outcome_year == 2013, 1, 0)
dists_df$y2014 <- ifelse(dists_df$outcome_year == 2014, 1, 0)
dists_df$y2015 <- ifelse(dists_df$outcome_year == 2015, 1, 0)
dists_df$y2016 <- ifelse(dists_df$outcome_year == 2016, 1, 0)
dists_df$y2017 <- ifelse(dists_df$outcome_year == 2017, 1, 0)
dists_df$y2018 <- ifelse(dists_df$outcome_year == 2018, 1, 0)
dists_df$y2019 <- ifelse(dists_df$outcome_year == 2019, 1, 0)

dists_df$group1 <- ifelse(dists_df$enrollgroup == 2010, 1, 0)
dists_df$group2 <- ifelse(dists_df$enrollgroup == 2012, 1, 0)
dists_df$group3 <- ifelse(dists_df$enrollgroup == 2014, 1, 0)

csa_sb <- att_gt(yname = "sb_rate",
                 gname = "cohort",
                 idname = "dist_id",
                 tname = "outcome_year",
                 control_group = "notyettreated",
                 data = dists_df_alltreated
)

csa = att_gt(
  yname = "y",
  gname = "cohort",
  idname = "firm",
  tname = "year",
  control_group = "notyettreated",
  data = dat)

csa_sb = data.table(group = csa_sb$group, year = csa_sb$t, csa = csa_sb$att)

dist_sb_dynamic <- aggte(dist_sb, type = "dynamic")

#Wooldridge

dists_df_alltreated <- dists_df %>% filter(enrollgroup >0)


dists_df <- dists_df %>% mutate(treat = case_when(enrollgroup == 0 ~ 0,
                                                  enrollgroup > 0 & outcome_year >= enrollgroup ~ 1,
                                                  enrollgroup > 0 & outcome_year < enrollgroup ~ 0,
                                                  TRUE ~ NA_real_))

#dists_df$treat <- ifelse(dists_df$outcome_year >= dists_df$enrollgroup, 1, 0)

dists_df <- dists_df %>% mutate(time_to_treat = case_when(enrollgroup == 0 ~ NA,
                                                          enrollgroup > 0 ~ outcome_year - enrollgroup,
                                                          TRUE ~ NA_real_))

dists_df$cohort <- ifelse(dists_df$enrollgroup == 0, NA, dists_df$enrollgroup)

#dists_df$time_to_treat <- dists_df$outcome_year - dists_df$enrollgroup
etwfe_sb <- feols(sb_rate ~ (treat : factor(time_to_treat) : factor(cohort)) | dist_id + outcome_year,
                  data = dists_df_alltreated)

etwfe_sb = as.data.table(tidy(etwfe_sb))
etwfe_sb = etwfe_sb[
  , .(term = term, etwfe_sb = estimate)][
    , group := as.numeric(gsub(".*cohort.", "", term))][
      , year := as.numeric(gsub(".*time_to_treat.(\\d+).*", "\\1", term)) + group][
        , .(group, year, etwfe_sb)]

results = merge(etwfe_sb, csa_sb, by = c("group", "year"))
colnames(results) = c("Group", "Year", "TWFE w/ interactions", "CSA (2021)")
results[, Group := factor(Group)]

dat_plot = melt(results, id.vars = c("Group", "Year"))
ggplot(dat_plot, aes(Year, value, color = variable, linetype = Group)) +
  geom_line(size = 1.4) +
  theme_minimal() +
  labs(x = "Year", y = "ATT", color = "Estimator", linetype = "Group")

stage_2_sb_lm <- svyglm(sb ~ pred_step1 + primary_school + wi_perc_rank + strat_rurb + age + as.factor(g) +
                          as.factor(outcome_year) + rural_urban, design = design_rsby_step1_etwfe)

nfhs <- df_rsby_step1_etwfe %>% filter(survey == "NFHS4" | survey == "NFHS5")

stage_2_nfhs <- feols(sb ~ pred_step1 + primary_school + wi_perc_rank + strat_rurb + age
                      | dist_id + outcome_year, 
                      cluster = ~ dist_id, weights = ~ weight_adj, data = nfhs)

# Same for RD
RD <- function(data, indices, formula) { 
  dta <- data[indices,] 
  # Model
  mod <- feols(formula, data=dta)
  # Get predicted probabilities (ie risks) for a 50 and 60 yr old based on this model
  pp <- predict(mod, newdata=transform(dta,age0=50), type="response") 
  return(predict) 
}

library(boot) 
boot.RD <- boot(data = nfhs, statistic=RD, R=1000, formula=sb ~ pred_step1 + primary_school + 
                  wi_perc_rank + strat_rurb + age+ factor(dist_id) + factor(outcome_year)); boot.RD 
