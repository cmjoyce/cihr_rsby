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

#reverse coding toilet for comprehension
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
                        improved_water, toilet_rev, toilet_share, 
                         cooking_fuel, has_computer))

#examining for squared multiple correlations
smc <- psych::smc(asset[,3:16])
smc

#drop variables with less than 0.05 -- explain less than 5% of variance. In this case bike, radio and animal car, and improved water

asset_smc <- asset %>% select(c(caseid, rural_urban, water_treat, mobile_phone, #telephone_land_line, 
                                fridge, motorcycle, 
                         car,
                         toilet_rev, toilet_share, 
                         cooking_fuel, has_computer))


#not enough responses for type of water filter

#asset$type_water_filter <- as.factor(asset$type_water_filter)
#asset$water_source <- as.factor(asset$water_source)
#asset$toilet_type <- as.factor(asset$toilet_type)
#asset$cooking_fuel <- as.factor(asset$cooking_fuel)

#loading package for PCA
library(factoextra)
library(psych)

#asset.pca <- prcomp(na.omit(asset), scale = TRUE)
#asset.pca

#asset.pca.factors <- factoextra::get_pca(asset.pca, "var")


prn<-psych::principal(asset_smc[,3:11], rotate="none", nfactors=2, cor = "mixed", 
                      covar=T, scores=TRUE, missing = TRUE)

index <- prn$scores[,2]


Assets.indexed<-mutate(asset,wi_quintile=as.factor(ntile(index,5)),
                       wi_continuous = index)


ggplot(na.omit(Assets.indexed), aes(as.factor(rural_urban))) + geom_bar(aes(fill = wi_quintile), position = "fill")+ xlab("Rural (0) & Urban (1)")+
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

#weight adjusted. De-normalizing weights from "sampled dataset.
#using closest census to outcome year. Only DLHS3 will use census 2001. Will calculate proportion for both 2001 and 2011, outcome year
#will determine which proportion to use. If outcome year is closer 2004 or 2005 we will use 2001. 2006-2008 will use 2011.

#load in sampled from weight denormalization file

df <- left_join(df, sampled, by = "state")

df <- df %>% select(-c(EM_2001, EM_2011, nfhs4_sampled, nfhs5_sampled, dlhs3_sampled, dlhs4_sampled, ahs_sampled))

#dropping sampled and census 

df <- df %>% mutate(weight_adj = case_when(survey == "NFHS4" ~ (weight * nfhs4_prop),
                                           survey == "DLHS3" & outcome_year < 2006 ~ (weight * dlhs3_prop_2001),
                                           survey == "DLHS3" & outcome_year > 2005 ~ (weight * dlhs3_prop_2011),
                                           survey == "DLHS4" ~ (weight * dlhs4_prop),
                                           survey == "NFHS5" ~ (weight * nfhs5_prop),
                                           survey == "AHS" ~ (weight * ahs_prop),
                                           TRUE ~ NA_real_))


#dropping nas from outcomes
df <- df %>% filter(!is.na(sb))

#dropping unknown outcome years
df <- df %>% filter(!is.na(outcome_year))

#dropping pregnancies that completed in 2021 or 2020 as many states not sampled
df <- df %>% filter(outcome_year < 2020)

#now making sure every state is represented in every year.
#dropping states: 1 jammu & kashmir + ladakh (missing 2009 + 2010)
#7 Delhi (missing 2009 & 2010)
#13 nagaland (missing 2004 - 2007)
#24 gujarat (missing 2009 & 2010)
#25 dadra & nagar haveli and daman & diu (missing 2009 & 2010)
#30 lakshadweep (missing 2009 & 2010)
table(df$state, df$outcome_year)

df <- df %>% filter(state != 1, state != 7, state != 13, state != 24, state != 25, state != 30)


#writing new csv
#write.csv(df, "df_socioeconomic.csv")


# calculating rates per year ----------------------------------------------

#making strata variable from rural/urban
df$strat_rurb <- ifelse(df$rural_urban == 0, 2, 1)

design <- svydesign(data = df, ids = ~psu, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)

sb_year_rate <- svyby(~sb, ~outcome_year, design, svymean, vartype=c("se","ci"), na.rm.all = TRUE)
ms_year_rate <- svyby(~miscarriage, ~outcome_year, design, svymean, vartype=c("se","ci"), na.rm.all = TRUE)
abort_year_rate <- svyby(~abort, ~outcome_year, design, svymean, vartype = c("se", "ci"))

sb_wi_rate <- svyby(~sb, ~wi_quintile, design, svymean, vartype=c("se","ci"), na.rm.all = TRUE)
ms_wi_rate <- svyby(~miscarriage, ~wi_quintile, design, svymean, vartype=c("se","ci"), na.rm.all = TRUE)
abort_wi_rate <- svyby(~abort, ~wi_quintile, design, svymean, vartype = c("se", "ci"))


sb_wi_rate <- svyby(~sb, ~wi_quintile, design, svymean, vartype=c("se","ci"), na.rm.all = TRUE)
sb_wi_rate$sb_per1000 <- sb_wi_rate$sb*1000
sb_wi_rate$ci_l_per1000 <- sb_wi_rate$ci_l*1000
sb_wi_rate$ci_u_per1000 <- sb_wi_rate$ci_u*1000




sb_wi_year_rate <- svyby(~sb, ~outcome_year*~wi_quintile, design, svymean, vartype=c("se","ci"), na.rm.all = TRUE)

sb_wi_year_rate$sb_per1000 <- sb_wi_year_rate$sb*1000
sb_wi_year_rate$ci_l_per1000 <- sb_wi_year_rate$ci_l*1000
sb_wi_year_rate$ci_u_per1000 <- sb_wi_year_rate$ci_u*1000
sb_wi_year_rate$se_per1000 <- sb_wi_year_rate$se*1000

library(cowplot)
stillbirth_wi <- ggplot(data = sb_wi_year_rate, mapping = aes(x= outcome_year, y = sb_per1000, color = wi_quintile)) + geom_point() + 
  #geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+
  geom_line() + ylim(0,25)+
  scale_color_brewer(palette = "Paired", name = "Wealth Quintile", breaks =c("1", "2", "3", "4", "5"), 
                     labels = c("0.2", "0.4", "0.6", "0.8", "1.0"))+
  labs(y = "Rate of stillbirths per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot()


#with ribbon showing confidence interval
ggplot(data=sb_wi_year_rate, aes(x=outcome_year, y=sb_per1000, ymin=ci_l_per1000, ymax=ci_u_per1000, fill=wi_quintile, linetype=wi_quintile)) + 
  #geom_point() +     
  geom_line() + 
  geom_ribbon(alpha=0.25) + 
  scale_color_brewer(palette = "Paired", name = "Wealth Quintile", breaks =c("1", "2", "3", "4", "5"), 
                     labels = c("0.2", "0.4", "0.6", "0.8", "1.0"))+
  labs(y = "Rate of stillbirths per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot()

#with shapes instead of colors
ggplot(data = sb_wi_year_rate, mapping = aes(x= outcome_year, y = sb_per1000)) + geom_point(aes(shape = wi_quintile)) + 
  #geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+ 
  geom_line() + ylim(0,25)+
  scale_color_brewer(palette = "Paired")+
  theme_cowplot()

ggplot(data = sb_wi_rate, mapping = aes(x= wi_quintile, y = sb_per1000)) + 
  geom_point() +
  geom_line() + 
  ylim(0,15) +
  scale_color_brewer(palette = "Paired", name = "Wealth Quintile", breaks =c("1", "2", "3", "4", "5"), 
                     labels = c("0.2", "0.4", "0.6", "0.8", "1.0"))+
  labs(y = "Rate of stillbirths per 1000 pregnancies") +
  labs(x = "Wealth Quintile")+
  theme_cowplot()

# now looking at primary school
sb_prim_year_rate <- svyby(~sb, ~outcome_year*~primary, design, svymean, vartype=c("se","ci"), na.rm.all = TRUE)

sb_prim_year_rate$sb_per1000 <- sb_prim_year_rate$sb*1000
sb_prim_year_rate$ci_l_per1000 <- sb_prim_year_rate$ci_l*1000
sb_prim_year_rate$ci_u_per1000 <- sb_prim_year_rate$ci_u*1000


ggplot(data = sb_prim_year_rate, mapping = aes(x= outcome_year, y = sb_per1000, color = as.factor(primary))) + geom_point() + 
  #geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+
  geom_line() + ylim(0,25)+
  scale_color_brewer(palette = "Paired",  name = "Completed Primary School", breaks =c("0", "1"), 
                     labels = c("No", "Yes"))+
  labs(y = "Rate of stillbirths per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot()


ggplot(data=sb_prim_year_rate, aes(x=outcome_year, y=sb_per1000, ymin=ci_l_per1000, ymax=ci_u_per1000, 
                                   fill=as.factor(primary), linetype=as.factor(primary))) + 
  #geom_point() +     
  geom_line() + 
  geom_ribbon(alpha=0.25) + 
  #scale_color_brewer(palette = "Paired", name = "Wealth Quintile", breaks =c("1", "2", "3", "4", "5"), 
  #                   labels = c("0.2", "0.4", "0.6", "0.8", "1.0"))+
  labs(y = "Rate of stillbirths per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot()

#now looking at abortion
abort_prim_year_rate <- svyby(~abort, ~outcome_year*~primary, design, svymean, vartype=c("se","ci"), na.rm.all = TRUE)

abort_prim_year_rate$abort_per1000 <- abort_prim_year_rate$abort*1000
abort_prim_year_rate$ci_l_per1000 <- abort_prim_year_rate$ci_l*1000
abort_prim_year_rate$ci_u_per1000 <- abort_prim_year_rate$ci_u*1000

abort_wi_year_rate <- svyby(~abort, ~outcome_year*~wi_quintile, design, svymean, vartype = c("se", "ci"), na.rm.all = TRUE)

abort_wi_year_rate$abort_per1000 <- abort_wi_year_rate$abort*1000
abort_wi_year_rate$ci_l_per1000 <- abort_wi_year_rate$ci_l*1000
abort_wi_year_rate$ci_u_per1000 <- abort_wi_year_rate$ci_u*1000

ggplot(data = abort_wi_year_rate, mapping = aes(x= outcome_year, y = abort_per1000, color = wi_quintile)) + geom_point() + 
  #geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+
  geom_line() +
  scale_color_brewer(palette = "Paired", name = "Wealth Quintile", breaks =c("1", "2", "3", "4", "5"), 
                     labels = c("0.2", "0.4", "0.6", "0.8", "1.0"))+
  labs(y = "Rate of abortions per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot()

ggplot(data=abort_wi_year_rate, aes(x=outcome_year, y=abort_per1000, ymin=ci_l_per1000, ymax=ci_u_per1000, fill=wi_quintile, linetype=wi_quintile)) + 
  #geom_point() +     
  geom_line() + 
  geom_ribbon(alpha=0.25) + 
  scale_color_brewer(palette = "Paired", name = "Wealth Quintile", breaks =c("1", "2", "3", "4", "5"), 
                     labels = c("0.2", "0.4", "0.6", "0.8", "1.0"))+
  labs(y = "Rate of abortion per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot()

ggplot(data = abort_prim_year_rate, mapping = aes(x= outcome_year, y = abort_per1000, color = as.factor(primary))) + geom_point() + 
  #geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+
  geom_line() +
  scale_color_brewer(palette = "Paired", name = "Completed Primary School", breaks =c("0", "1"), 
                     labels = c("No", "Yes"))+
  labs(y = "Rate of abortions per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot()


#now looking at miscarriage
miscarriage_prim_year_rate <- svyby(~miscarriage, ~outcome_year*~primary, design, svymean, vartype=c("se","ci"), na.rm.all = TRUE)

miscarriage_prim_year_rate$miscarriage_per1000 <- miscarriage_prim_year_rate$miscarriage*1000
miscarriage_prim_year_rate$ci_l_per1000 <- miscarriage_prim_year_rate$ci_l*1000
miscarriage_prim_year_rate$ci_u_per1000 <- miscarriage_prim_year_rate$ci_u*1000

miscarriage_wi_year_rate <- svyby(~miscarriage, ~outcome_year*~wi_quintile, design, svymean, vartype = c("se", "ci"), na.rm.all = TRUE)
miscarriage_wi_year_rate$miscarriage_per1000 <- miscarriage_wi_year_rate$miscarriage*1000
miscarriage_wi_year_rate$ci_l_per1000 <- miscarriage_wi_year_rate$ci_l*1000
miscarriage_wi_year_rate$ci_u_per1000 <- miscarriage_wi_year_rate$ci_u*1000

ggplot(data = miscarriage_wi_year_rate, mapping = aes(x= outcome_year, y = miscarriage_per1000, color = wi_quintile)) + geom_point() + 
  #geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+
  geom_line() +
  scale_color_brewer(palette = "Paired", name = "Wealth Quintile", breaks =c("1", "2", "3", "4", "5"), 
                     labels = c("0.2", "0.4", "0.6", "0.8", "1.0"))+
  labs(y = "Rate of miscarriage per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot()

ggplot(data = miscarriage_prim_year_rate, mapping = aes(x= outcome_year, y = miscarriage_per1000, color = as.factor(primary))) + geom_point() + 
  #geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+
  geom_line() + 
  scale_color_brewer(palette = "Paired", name = "Completed Primary School", breaks =c("0", "1"), 
                     labels = c("No", "Yes"))+
  labs(y = "Rate of miscarriage per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot()

#complex model run on cluster. Code below copied over

library(survey)
library(broom)

#calculating midpoint
table(df$wi_quintile)

#highest wealth index midpoint
highest_range <- (length(which(df$wi_quintile == 5)) / sum(table(df$wi_quintile)))
highest_midpoint <- highest_range / 2

quint4_range <- (length(which(df$wi_quintile == 4)) / sum(table(df$wi_quintile)))
quint4_midpoint <- (quint4_range / 2) + highest_range

quint3_range <- (length(which(df$wi_quintile == 3)) / sum(table(df$wi_quintile)))
quint3_midpoint <- (quint3_range / 2) + quint4_range

quint2_range <- (length(which(df$wi_quintile == 2)) / sum(table(df$wi_quintile)))
quint2_midpoint <- (quint2_range / 2) + quint3_range

lowest_range <- (length(which(df$wi_quintile == 1)) / sum(table(df$wi_quintile)))
lowest_midpoint <- (lowest_range / 2) + quint2_range


df <- df %>% mutate(wi_midpoint = case_when(wi_quintile == 5 ~ highest_midpoint,
                                                                          wi_quintile == 4 ~ quint4_midpoint,
                                                                          wi_quintile == 3 ~ quint3_midpoint,
                                                                          wi_quintile == 2 ~ quint2_midpoint,
                                                                          wi_quintile == 1 ~ lowest_midpoint,
                                                                          TRUE ~ NA_real_))
#redoing design to account for midpoint variable
design <- svydesign(data = df, ids = ~psu, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)

design_noweight <- svydesign(data = df, ids = ~psu, strata = ~strat_rurb, nest = TRUE)


#PSUs are repeated between NFHSs and DLHSs, have set nest = TRUE to account for that. Will look into this more.

#rii with midpoint
sb_wi_rii <- svyglm(sb ~ wi_midpoint + age + outcome_year + as.factor(state), design = design, family = quasibinomial(link = "log"))

sb_wi_rii <- sb_wi_rii %>% tidy(conf.int = T) %>%
  mutate(across(where(is.numeric), round, digits = 2))

rii <- sb_rii$coefficients[2]



#SII check using following equation SII = 2 X M X (RII−1) / RII + 1

m <- svymean(~sb, design_completeyears)

sb_sii <- (m*2*(rii - 1)) / (rii + 1)

sb_wi_sii <- svyglm(sb ~ wi_midpoint + age + outcome_year + (1 | state), design = design, family = quasibinomial(link = "identity"))

sb_wi_sii <- sb_wi_sii %>% 
  tidy(conf.int = TRUE)


sb_wi_sii <- sb_wi_sii %>%  mutate(across(where(is.numeric), round, digits = 2))


# now abortion
#rii with midpoint
abort_rii <- svyglm(abort ~ wi_midpoint + (1 | age) + outcome_year + (1 | state), design = design, family = quasibinomial(link = "log"))
abort_rii <- abort_rii %>% tidy(conf.int = TRUE) %>% mutate(across(where(is.numeric), round, digits = 2))

abort_sii <- svyglm(abort ~ wi_midpoint + outcome_year + (1|age) + (1|state), design = design, family = quasibinomial(link = "identity"))
abort_sii <- abort_sii %>% tidy(conf.int = TRUE) %>% mutate(across(where(is.numeric), round, digits = 2))



#now miscarriage
miscarriage_rii <- svyglm(miscarriage ~ wi_midpoint + outcome_year -1 + age + state -1, design = design, family = quasibinomial(link = "log"))
miscarriage_rii <- miscarriage_rii %>% tidy(conf.int = TRUE) %>% mutate(across(where(is.numeric), round, digits = 2))

miscarriage_sii <- svyglm(miscarriage ~ wi_midpoint + (1 | outcome_year) + (1 | age) + (1|state), design = design, family = quasibinomial(link = "identity"))
miscarriage_sii <- miscarriage_sii %>% tidy(conf.int = TRUE) %>% mutate(across(where(is.numeric), round, digits = 2))








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


# Table 1 creation --------------------------------------------------------

df %>% 
  select(age, age_first_birth, tot_live_births, primary, outcome_year, insurance) %>%
  tbl_summary()   


#rural == 0, 1s as they must be urbans.

df$rural_urban <- factor(df$rural_urban, 
                         levels = c(0, 1),
                         labels = c("Rural", "Urban"))


df %>% 
  select(age, rural_urban, tot_live_births, primary, insurance) %>% 
  tbl_summary(
  label = list(
    age ~ "Age",
    rural_urban ~ "Rural / Urban",
    #age_first_birth ~ "Age at first birth",
    tot_live_births ~ "Total live births",
    primary ~ "Completed Primary School",
    #outcome_year ~ "Year of Pregnancy Outcome",
    insurance ~ "Household has any insurance"),
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


df_plot %>% 
  ggplot(aes(x = outcome_year, y = abort)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
#  geom_vline(xintercept = 0, linetype = 3, size = 0.3) +
#  ylim(0, 0.1)+
#  labs(y = "Paid Job") +
#  labs(x = "Birth month relative to policy change")+
  theme_cowplot(12)

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

# calculating jackknife and bootstrap weights in survey ---------------------------------------------------------

library(survey)

##### Run on cluster ######

#weightdesign <- svydesign(data = df, ids = ~psu, strata = ~rural_urban, weights = ~weight_adj, nest = TRUE)

#dfboot <- as.svrepdesign(weightdesign, type="bootstrap")

#write.csv(dfboot, "dfboot.csv")

#dfboot <- read.csv("dfboot.csv")





# using new covariates of state, any primary, urban location, and if part of scheduled group

#lm <- lm(any_nonbirth ~ state + primary + urban + scheduled_group, data = df)

#summary(lm)

#lm_msb <- lm(miscarriage_sb ~ state + primary + urban + scheduled_group, data = df)
#summary(lm_msb)


#lm_abort <- lm(abortion ~ state + primary + urban + scheduled_group, data = df)
#summary(lm_abort)

#summary(glm(miscarriage_sb ~ state + primary + urban + scheduled_group, family = binomial(link = "logit"), data = df))


