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

df <- read.csv("df_socioeconomic.csv")

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
my_colors <- RColorBrewer::brewer.pal(5, "Reds")[4:9]

stillbirth_wi <- ggplot(data = sb_wi_year_rate, mapping = aes(x= outcome_year, y = sb_per1000, color = as.factor(wi_quintile))) + geom_point() + 
  #geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+
  geom_line() + ylim(0,25)+
  scale_color_grey(name = "Wealth Quintile", breaks =c("1", "2", "3", "4", "5"), 
                     labels = c("0.2", "0.4", "0.6", "0.8", "1.0"))+
  labs(y = "Rate of stillbirths per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot(11)

#Jan 11 using stillbirth_wi_rate below + forest plot
sbwirate <- ggplot(data = sb_wi_year_rate, mapping = aes(x= outcome_year, y = sb_per1000, color = as.factor(wi_quintile))) + 
  #geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+
  geom_smooth(se = F) + ylim(0,25)+
  scale_color_brewer(palette = "Paired", name = "Wealth Quintile", breaks =c("1", "2", "3", "4", "5"), 
                     labels = c("0.2", "0.4", "0.6", "0.8", "1.0"))+
  labs(y = "Rate of stillbirths per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot(11)

#plot just lowest and highest
highlow_quints <- df %>% filter(wi_quintile == 1 | wi_quintile == 5)
design_highlow <- svydesign(data = highlow_quints, ids = ~psu, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)

sb_wi_year_rate_highlow <- svyby(~sb, ~outcome_year*~wi_quintile, design_highlow, svymean, vartype=c("se","ci"), na.rm.all = TRUE)

sb_wi_year_rate_highlow$sb_per1000 <- sb_wi_year_rate_highlow$sb*1000
sb_wi_year_rate_highlow$ci_l_per1000 <- sb_wi_year_rate_highlow$ci_l*1000
sb_wi_year_rate_highlow$ci_u_per1000 <- sb_wi_year_rate_highlow$ci_u*1000
sb_wi_year_rate_highlow$se_per1000 <- sb_wi_year_rate_highlow$se*1000

library(cowplot)
ggplot(data = sb_wi_year_rate_highlow, mapping = aes(x= outcome_year, y = sb_per1000, color = wi_quintile)) + geom_point() + 
  geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+
  geom_line() + ylim(0,25)+
  scale_color_grey(name = "Wealth Quintile", breaks =c("1", "2", "3", "4", "5"), 
                   labels = c("0.2", "0.4", "0.6", "0.8", "1.0"))+
  labs(y = "Rate of stillbirths per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot()

ggplot(data = sb_wi_year_rate_highlow, mapping = aes(x= outcome_year, y = sb_per1000, ymin=ci_l_per1000, ymax=ci_u_per1000,color = wi_quintile)) + #geom_point() + 
  #geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+
  geom_line() + 
  geom_ribbon(alpha = 0.15)+
  ylim(0,25)+
  scale_color_grey(name = "Wealth Quintile", breaks =c("1", "2", "3", "4", "5"), 
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



stillbirth_prim <- ggplot(data = sb_prim_year_rate, mapping = aes(x= outcome_year, y = sb_per1000, color = as.factor(primary))) + geom_point() + 
  #geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+
  geom_line() + ylim(0,25)+
  scale_color_brewer(palette = "Set1",  name = "Completed Primary School", breaks =c("0", "1"), 
                     labels = c("No", "Yes"))+
  labs(y = "Rate of stillbirths per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot(11)


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

#now looking at scheduled caste and tribe
sb_caste_year_rate <- svyby(~sb, ~outcome_year*caste_group, design, svymean, vartype = c("se", "ci"), na.rm.all = TRUE)

sb_caste_year_rate$sb_per1000 <- sb_caste_year_rate$sb*1000
sb_caste_year_rate$ci_l_per1000 <- sb_caste_year_rate$ci_l*1000
sb_caste_year_rate$ci_u_per1000 <- sb_caste_year_rate$ci_u*1000

stillbirth_caste <-  ggplot(data = sb_caste_year_rate, mapping = aes(x= outcome_year, y = sb_per1000, color = as.factor(caste_group))) + geom_point() + 
  #geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+
  geom_line() + ylim(0, 15)+
  scale_color_brewer(palette = "Set1",  name = "Scheduled Caste or Scheduled Tribe", breaks =c("0", "1", "2"), 
                     labels = c("None", "Scheduled Caste", "Scheduled Tribe"))+
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

abort_wi <- ggplot(data = abort_wi_year_rate, mapping = aes(x= outcome_year, y = abort_per1000, color = wi_quintile)) + geom_point() + 
  #geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+
  geom_line() +
  scale_color_grey(name = "Wealth Quintile", breaks =c("1", "2", "3", "4", "5"), 
                     labels = c("0.2", "0.4", "0.6", "0.8", "1.0"))+
  labs(y = "Rate of abortions per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot(11)

ggplot(data=abort_wi_year_rate, aes(x=outcome_year, y=abort_per1000, ymin=ci_l_per1000, ymax=ci_u_per1000, fill=wi_quintile, linetype=wi_quintile)) + 
  #geom_point() +     
  geom_line() + 
  geom_ribbon(alpha=0.25) + 
  scale_color_grey(name = "Wealth Quintile", breaks =c("1", "2", "3", "4", "5"), 
                   labels = c("0.2", "0.4", "0.6", "0.8", "1.0"))+
  labs(y = "Rate of abortions per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot()

abort_prim <- ggplot(data = abort_prim_year_rate, mapping = aes(x= outcome_year, y = abort_per1000, color = as.factor(primary))) + geom_point() + 
  #geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+
  geom_line() +
  scale_color_brewer(palette = "Set1", name = "Completed Primary School", breaks =c("0", "1"), 
                     labels = c("No", "Yes"))+
  labs(y = "Rate of abortions per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot(11)

#now looking at scheduled caste and tribe
abort_caste_year_rate <- svyby(~abort, ~outcome_year*caste_group, design, svymean, vartype = c("se", "ci"), na.rm.all = TRUE)

abort_caste_year_rate$abort_per1000 <- abort_caste_year_rate$abort*1000
abort_caste_year_rate$ci_l_per1000 <- abort_caste_year_rate$ci_l*1000
abort_caste_year_rate$ci_u_per1000 <- abort_caste_year_rate$ci_u*1000

abort_caste <-  ggplot(data = abort_caste_year_rate, mapping = aes(x= outcome_year, y = abort_per1000, color = as.factor(caste_group))) + geom_point() + 
  #geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+
  geom_line() + 
  scale_color_brewer(palette = "Set1",  name = "Scheduled Caste or Scheduled Tribe", breaks =c("0", "1", "2"), 
                     labels = c("None", "Scheduled Caste", "Scheduled Tribe"))+
  labs(y = "Rate of abortion per 1000 pregnancies") +
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

miscarriage_wi <- ggplot(data = miscarriage_wi_year_rate, mapping = aes(x= outcome_year, y = miscarriage_per1000, color = wi_quintile)) + geom_point() + 
  #geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+
  geom_line() +
  scale_color_grey(name = "Wealth Quintile", breaks =c("1", "2", "3", "4", "5"), 
                     labels = c("0.2", "0.4", "0.6", "0.8", "1.0"))+
  labs(y = "Rate of miscarriage per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot(11)

miscarriage_prim <- ggplot(data = miscarriage_prim_year_rate, mapping = aes(x= outcome_year, y = miscarriage_per1000, color = as.factor(primary))) + geom_point() + 
  #geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+
  geom_line() + 
  scale_color_brewer(palette = "Set1",name = "Completed Primary School", breaks =c("0", "1"), 
                     labels = c("No", "Yes"))+
  labs(y = "Rate of miscarriage per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot(11)

miscarriage_caste_year_rate <- svyby(~miscarriage, ~outcome_year*caste_group, design, svymean, vartype = c("se", "ci"), na.rm.all = TRUE)

miscarriage_caste_year_rate$miscarriage_per1000 <- miscarriage_caste_year_rate$miscarriage*1000
miscarriage_caste_year_rate$ci_l_per1000 <- miscarriage_caste_year_rate$ci_l*1000
miscarriage_caste_year_rate$ci_u_per1000 <- miscarriage_caste_year_rate$ci_u*1000

miscarriage_caste <-  ggplot(data = miscarriage_caste_year_rate, mapping = aes(x= outcome_year, y = miscarriage_per1000, color = as.factor(caste_group))) + geom_point() + 
  #geom_errorbar(aes(ymin = ci_l_per1000, ymax = ci_u_per1000, color="black", width=.1))+
  geom_line() + 
  scale_color_brewer(palette = "Set1",  name = "Scheduled Caste or Scheduled Tribe", breaks =c("0", "1", "2"), 
                     labels = c("None", "Scheduled Caste", "Scheduled Tribe"))+
  labs(y = "Rate of miscarriageion per 1000 pregnancies") +
  labs(x = "Year")+
  theme_cowplot()

library(ggpubr)
#grouping plots so there is one for wealth quintile, one for education, and one for primary school
fig1 <- ggarrange(stillbirth_wi, abort_wi, miscarriage_wi,
                  #labels = c("A", "B", "C"),
                  ncol = 3, nrow = 1, common.legend = TRUE, legend = "right")

fig2 <- ggarrange(stillbirth_prim, abort_prim, miscarriage_prim,
                  ncol = 3, nrow = 1,
                  common.legend = TRUE, legend = "right")

fig3 <- ggarrange(stillbirth_caste, abort_caste, miscarriage_caste,
                  ncol = 3, nrow = 1,
                  common.legend = TRUE, legend = "right")


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

#checking other wealth quintile (0.2 to 1 )
df <- df %>% mutate(wi_quint = case_when(wi_quintile == 1 ~ 0.2,
                                        wi_quintile == 2 ~ 0.4,
                                        wi_quintile == 3 ~ 0.6,
                                        wi_quintile == 4 ~ 0.8,
                                        wi_quintile == 5 ~ 1))

#redoing design to account for midpoint and wi_quint variable
library(survey)
design <- svydesign(data = df, ids = ~psu, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)

design_noweight <- svydesign(data = df, ids = ~psu, strata = ~strat_rurb, nest = TRUE)


#PSUs are repeated between NFHSs and DLHSs, have set nest = TRUE to account for that. Will look into this more.


#making two groups to compare RII and SII. 2004 - 2010 

#binarizing caste or tribe?

df$scheduled <- ifelse(df$caste_group > 0, 1, df$caste_group)

#redoing design to account for binarized variable
design <- svydesign(data = df, ids = ~psu, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)


groupa <- df %>% filter(outcome_year < 2010)
groupb <- df %>% filter(outcome_year > 2009)

design_a <- svydesign(data = groupa, ids = ~psu, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)

design_b <- svydesign(data = groupb, ids = ~psu, strata = ~strat_rurb, weights = ~weight_adj, nest = TRUE)


# Wealth index SII and RII ------------------------------------------------

#looking at it with 0.2 - 1 quints
library(janitor)
library(dotwhisker)
sb_wiquint_sii <- svyglm(sb ~ wi_quint + age + outcome_year + as.factor(state), design = design)
sb_wiquint_sii <- sb_wiquint_sii %>% tidy(conf.int = TRUE) 
sbwi_sii <- sb_wiquint_sii$estimate[2] * 1000
sbwi_sii <- round_half_up(sbwi_sii, digits = 2)
sbwi_sii_conf.low <- (sb_wiquint_sii$conf.low[2]*1000) %>% round_half_up(digits = 2)
sbwi_sii_conf.high <- (sb_wiquint_sii$conf.high[2]*1000) %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
sb_wiquint_sii <- sb_wiquint_sii %>% filter(row_number() %in% c(2))
sb_wiquint_sii$term[sb_wiquint_sii$term=="wi_quint"] <- "Stillbirth All Years"


#now in group a
sb_wiquint_sii_group_a <- svyglm(sb ~ wi_quint + age + outcome_year + as.factor(state), design = design_a)
sb_wiquint_sii_group_a <- sb_wiquint_sii_group_a %>% tidy(conf.int = TRUE) 
sbwi_sii_group_a <- sb_wiquint_sii_group_a$estimate[2] * 1000
sbwi_sii_group_a <- round_half_up(sbwi_sii_group_a, digits = 2)
sbwi_sii_group_a_conf.low <- (sb_wiquint_sii_group_a$conf.low[2]*1000) %>% round_half_up(digits = 2)
sbwi_sii_group_a_conf.high <- (sb_wiquint_sii_group_a$conf.high[2]*1000) %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
sb_wiquint_sii_group_a <- sb_wiquint_sii_group_a %>% filter(row_number() %in% c(2))
sb_wiquint_sii_group_a$term[sb_wiquint_sii_group_a$term=="wi_quint"] <- "Stillbirth 2004 - 2009"

#now in group b
sb_wiquint_sii_group_b <- svyglm(sb ~ wi_quint + age + outcome_year + as.factor(state), design = design_b)
sb_wiquint_sii_group_b <- sb_wiquint_sii_group_b %>% tidy(conf.int = TRUE) 
sbwi_sii_group_b <- sb_wiquint_sii_group_b$estimate[2] * 1000
sbwi_sii_group_b <- round_half_up(sbwi_sii_group_b, digits = 2)
sbwi_sii_group_b_conf.low <- (sb_wiquint_sii_group_b$conf.low[2]*1000) %>% round_half_up(digits = 2)
sbwi_sii_group_b_conf.high <- (sb_wiquint_sii_group_b$conf.high[2]*1000) %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
sb_wiquint_sii_group_b <- sb_wiquint_sii_group_b %>% filter(row_number() %in% c(2))
sb_wiquint_sii_group_b$term[sb_wiquint_sii_group_b$term=="wi_quint"] <- "Stillbirth 2010 - 2019"


#calculating RII. quasipoisson glm
sb_wiquint_rii <- svyglm(sb ~ wi_quint + age + outcome_year + as.factor(state), design = design, family = quasipoisson())
sb_wiquint_rii <- sb_wiquint_rii %>% tidy(conf.int = TRUE)
sbwi_rii <- sb_wiquint_rii$estimate[2] %>% round_half_up(digits = 2)
sbwi_rii_conflow <- sb_wiquint_rii$conf.low[2] %>% round_half_up(digits = 2)
sbwi_rii_confhigh <- sb_wiquint_rii$conf.high[2]  %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
sb_wiquint_rii <- sb_wiquint_rii %>% filter(row_number() %in% c(2))
sb_wiquint_rii$term[sb_wiquint_rii$term=="wi_quint"] <- "Stillbirth All Years"


#now group a 
sb_wiquint_rii_group_a <- svyglm(sb ~ wi_quint + age + outcome_year + as.factor(state), design = design_a, family = quasipoisson())
sb_wiquint_rii_group_a <- sb_wiquint_rii_group_a %>% tidy(conf.int = TRUE)
sbwi_rii_group_a <- sb_wiquint_rii_group_a$estimate[2] %>% round_half_up(digits = 2)
sbwi_rii_group_a_conflow <- sb_wiquint_rii_group_a$conf.low[2] %>% round_half_up(digits = 2)
sbwi_rii_group_a_confhigh <- sb_wiquint_rii_group_a$conf.high[2]  %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
sb_wiquint_rii_group_a <- sb_wiquint_rii_group_a %>% filter(row_number() %in% c(2))
sb_wiquint_rii_group_a$term[sb_wiquint_rii_group_a$term=="wi_quint"] <- "Stillbirth 2004 - 2009"


#now group b 
sb_wiquint_rii_group_b <- svyglm(sb ~ wi_quint + age + outcome_year + as.factor(state), design = design_b, family = quasipoisson())
sb_wiquint_rii_group_b <- sb_wiquint_rii_group_b %>% tidy(conf.int = TRUE)
sbwi_rii_group_b <- sb_wiquint_rii_group_b$estimate[2] %>% round_half_up(digits = 2)
sbwi_rii_group_b_conflow <- sb_wiquint_rii_group_b$conf.low[2] %>% round_half_up(digits = 2)
sbwi_rii_group_b_confhigh <- sb_wiquint_rii_group_b$conf.high[2]  %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
sb_wiquint_rii_group_b <- sb_wiquint_rii_group_b %>% filter(row_number() %in% c(2))
sb_wiquint_rii_group_b$term[sb_wiquint_rii_group_b$term=="wi_quint"] <- "Stillbirth 2010 - 2019"


abort_wiquint_sii <- svyglm(abort ~ wi_quint + age + outcome_year + as.factor(state), design = design)
abort_wiquint_sii <- abort_wiquint_sii %>% tidy(conf.int = TRUE) 
abortwi_sii <- abort_wiquint_sii$estimate[2] * 1000
abortwi_sii <- round_half_up(abortwi_sii, digits = 2)
abortwi_sii_conf.low <- (abort_wiquint_sii$conf.low[2]*1000) %>% round_half_up(digits = 2)
abortwi_sii_conf.high <- (abort_wiquint_sii$conf.high[2]*1000) %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
abort_wiquint_sii <- abort_wiquint_sii %>% filter(row_number() %in% c(2))
abort_wiquint_sii$term[abort_wiquint_sii$term=="wi_quint"] <- "Abortion All Years"


abort_wiquint_sii_group_a <- svyglm(abort ~ wi_quint + age + outcome_year + as.factor(state), design = design_a)
abort_wiquint_sii_group_a <- abort_wiquint_sii_group_a %>% tidy(conf.int = TRUE) 
abortwi_sii_group_a <- abort_wiquint_sii_group_a$estimate[2] * 1000
abortwi_sii_group_a <- round_half_up(abortwi_sii_group_a, digits = 2)
abortwi_sii_group_a_conf.low <- (abort_wiquint_sii_group_a$conf.low[2]*1000) %>% round_half_up(digits = 2)
abortwi_sii_group_a_conf.high <- (abort_wiquint_sii_group_a$conf.high[2]*1000) %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
abort_wiquint_sii_group_a <- abort_wiquint_sii_group_a %>% filter(row_number() %in% c(2))
abort_wiquint_sii_group_a$term[abort_wiquint_sii_group_a$term=="wi_quint"] <- "Abortion 2004 - 2009"


abort_wiquint_sii_group_b <- svyglm(abort ~ wi_quint + age + outcome_year + as.factor(state), design = design_b)
abort_wiquint_sii_group_b <- abort_wiquint_sii_group_b %>% tidy(conf.int = TRUE) 
abortwi_sii_group_b <- abort_wiquint_sii_group_b$estimate[2] * 1000
abortwi_sii_group_b <- round_half_up(abortwi_sii_group_b, digits = 2)
abortwi_sii_group_b_conf.low <- (abort_wiquint_sii_group_b$conf.low[2]*1000) %>% round_half_up(digits = 2)
abortwi_sii_group_b_conf.high <- (abort_wiquint_sii_group_b$conf.high[2]*1000) %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
abort_wiquint_sii_group_b <- abort_wiquint_sii_group_b %>% filter(row_number() %in% c(2))
abort_wiquint_sii_group_b$term[abort_wiquint_sii_group_b$term=="wi_quint"] <- "Abortion 2010 - 2019"


abort_wiquint_rii <- svyglm(abort ~ wi_quint + age + outcome_year + as.factor(state), design = design, family = quasipoisson())
abort_wiquint_rii <- abort_wiquint_rii %>% tidy(conf.int = TRUE)
abortwi_rii <- abort_wiquint_rii$estimate[2] %>% round_half_up(digits = 2)
abortwi_rii_conflow <- abort_wiquint_rii$conf.low[2] %>% round_half_up(digits = 2)
abortwi_rii_confhigh <- abort_wiquint_rii$conf.high[2]  %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
abort_wiquint_rii <- abort_wiquint_rii %>% filter(row_number() %in% c(2))
abort_wiquint_rii$term[abort_wiquint_rii$term=="wi_quint"] <- "Abortion All Years"


abort_wiquint_rii_group_a <- svyglm(abort ~ wi_quint + age + outcome_year + as.factor(state), design = design_a, family = quasipoisson())
abort_wiquint_rii_group_a <- abort_wiquint_rii_group_a %>% tidy(conf.int = TRUE)
abortwi_rii_group_a <- abort_wiquint_rii_group_a$estimate[2] %>% round_half_up(digits = 2)
abortwi_rii_group_a_conflow <- abort_wiquint_rii_group_a$conf.low[2] %>% round_half_up(digits = 2)
abortwi_rii_group_a_confhigh <- abort_wiquint_rii_group_a$conf.high[2]  %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
abort_wiquint_rii_group_a <- abort_wiquint_rii_group_a %>% filter(row_number() %in% c(2))
abort_wiquint_rii_group_a$term[abort_wiquint_rii_group_a$term=="wi_quint"] <- "Abortion 2004 - 2009"


abort_wiquint_rii_group_b <- svyglm(abort ~ wi_quint + age + outcome_year + as.factor(state), design = design_b, family = quasipoisson())
abort_wiquint_rii_group_b <- abort_wiquint_rii_group_b %>% tidy(conf.int = TRUE)
abortwi_rii_group_b <- abort_wiquint_rii_group_b$estimate[2] %>% round_half_up(digits = 2)
abortwi_rii_group_b_conflow <- abort_wiquint_rii_group_b$conf.low[2] %>% round_half_up(digits = 2)
abortwi_rii_group_b_confhigh <- abort_wiquint_rii_group_b$conf.high[2]  %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
abort_wiquint_rii_group_b <- abort_wiquint_rii_group_b %>% filter(row_number() %in% c(2))
abort_wiquint_rii_group_b$term[abort_wiquint_rii_group_b$term=="wi_quint"] <- "Abortion 2010 - 2019"


miscarriage_wiquint_sii <- svyglm(miscarriage ~ wi_quint + age + outcome_year + as.factor(state), design = design)
miscarriage_wiquint_sii <- miscarriage_wiquint_sii %>% tidy(conf.int = TRUE) 
miscarriagewi_sii <- miscarriage_wiquint_sii$estimate[2] * 1000
miscarriagewi_sii <- round_half_up(miscarriagewi_sii, digits = 2)
miscarriagewi_sii_conf.low <- (miscarriage_wiquint_sii$conf.low[2]*1000) %>% round_half_up(digits = 2)
miscarriagewi_sii_conf.high <- (miscarriage_wiquint_sii$conf.high[2]*1000) %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
miscarriage_wiquint_sii <- miscarriage_wiquint_sii %>% filter(row_number() %in% c(2))
miscarriage_wiquint_sii$term[miscarriage_wiquint_sii$term=="wi_quint"] <- "Miscarriage All Years"


miscarriage_wiquint_sii_group_a <- svyglm(miscarriage ~ wi_quint + age + outcome_year + as.factor(state), design = design_a)
miscarriage_wiquint_sii_group_a <- miscarriage_wiquint_sii_group_a %>% tidy(conf.int = TRUE) 
miscarriagewi_sii_group_a <- miscarriage_wiquint_sii_group_a$estimate[2] * 1000
miscarriagewi_sii_group_a <- round_half_up(miscarriagewi_sii_group_a, digits = 2)
miscarriagewi_sii_group_a_conf.low <- (miscarriage_wiquint_sii_group_a$conf.low[2]*1000) %>% round_half_up(digits = 2)
miscarriagewi_sii_group_a_conf.high <- (miscarriage_wiquint_sii_group_a$conf.high[2]*1000) %>% round_half_up(digits = 2)

miscarriage_wiquint_sii_group_a <- miscarriage_wiquint_sii_group_a %>% filter(row_number() %in% c(2))
miscarriage_wiquint_sii_group_a$term[miscarriage_wiquint_sii_group_a$term=="wi_quint"] <- "Miscarriage 2004 - 2009"


miscarriage_wiquint_sii_group_b <- svyglm(miscarriage ~ wi_quint + age + outcome_year + as.factor(state), design = design_b)
miscarriage_wiquint_sii_group_b <- miscarriage_wiquint_sii_group_b %>% tidy(conf.int = TRUE) 
miscarriagewi_sii_group_b <- miscarriage_wiquint_sii_group_b$estimate[2] * 1000
miscarriagewi_sii_group_b <- round_half_up(miscarriagewi_sii_group_b, digits = 2)
miscarriagewi_sii_group_b_conf.low <- (miscarriage_wiquint_sii_group_b$conf.low[2]*1000) %>% round_half_up(digits = 2)
miscarriagewi_sii_group_b_conf.high <- (miscarriage_wiquint_sii_group_b$conf.high[2]*1000) %>% round_half_up(digits = 2)

miscarriage_wiquint_sii_group_b <- miscarriage_wiquint_sii_group_b %>% filter(row_number() %in% c(2))
miscarriage_wiquint_sii_group_b$term[miscarriage_wiquint_sii_group_b$term=="wi_quint"] <- "Miscarriage 2010 - 2019"



#calculating RR. 
miscarriage_wiquint_rii <- svyglm(miscarriage ~ wi_quint + age + outcome_year + as.factor(state), design = design, family = quasipoisson())
miscarriage_wiquint_rii <- miscarriage_wiquint_rii %>% tidy(conf.int = TRUE)
miscarriagewi_rii <- miscarriage_wiquint_rii$estimate[2] %>% round_half_up(digits = 2)
miscarriagewi_rii_conflow <- miscarriage_wiquint_rii$conf.low[2] %>% round_half_up(digits = 2)
miscarriagewi_rii_confhigh <- miscarriage_wiquint_rii$conf.high[2]  %>% round_half_up(digits = 2)

miscarriage_wiquint_rii <- miscarriage_wiquint_rii %>% filter(row_number() %in% c(2))
miscarriage_wiquint_rii$term[miscarriage_wiquint_rii$term=="wi_quint"] <- "Miscarriage All Years"


miscarriage_wiquint_rii_group_a <- svyglm(miscarriage ~ wi_quint + age + outcome_year + as.factor(state), design = design_a, family = quasipoisson())
miscarriage_wiquint_rii_group_a <- miscarriage_wiquint_rii_group_a %>% tidy(conf.int = TRUE)
miscarriagewi_rii_group_a <- miscarriage_wiquint_rii_group_a$estimate[2] %>% round_half_up(digits = 2)
miscarriagewi_rii_group_a_conflow <- miscarriage_wiquint_rii_group_a$conf.low[2] %>% round_half_up(digits = 2)
miscarriagewi_rii_group_a_confhigh <- miscarriage_wiquint_rii_group_a$conf.high[2]  %>% round_half_up(digits = 2)

miscarriage_wiquint_rii_group_a <- miscarriage_wiquint_rii_group_a %>% filter(row_number() %in% c(2))
miscarriage_wiquint_rii_group_a$term[miscarriage_wiquint_rii_group_a$term=="wi_quint"] <- "Miscarriage 2004 - 2009"


miscarriage_wiquint_rii_group_b <- svyglm(miscarriage ~ wi_quint + age + outcome_year + as.factor(state), design = design_b, family = quasipoisson())
miscarriage_wiquint_rii_group_b <- miscarriage_wiquint_rii_group_b %>% tidy(conf.int = TRUE)
miscarriagewi_rii_group_b <- miscarriage_wiquint_rii_group_b$estimate[2] %>% round_half_up(digits = 2)
miscarriagewi_rii_group_b_conflow <- miscarriage_wiquint_rii_group_b$conf.low[2] %>% round_half_up(digits = 2)
miscarriagewi_rii_group_b_confhigh <- miscarriage_wiquint_rii_group_b$conf.high[2]  %>% round_half_up(digits = 2)

miscarriage_wiquint_rii_group_b <- miscarriage_wiquint_rii_group_b %>% filter(row_number() %in% c(2))
miscarriage_wiquint_rii_group_b$term[miscarriage_wiquint_rii_group_b$term=="wi_quint"] <- "Miscarriage 2010 - 2019"


sii <- rbind(sb_wiquint_sii, sb_wiquint_sii_group_a, sb_wiquint_sii_group_b, 
             abort_wiquint_sii, abort_wiquint_sii_group_a, abort_wiquint_sii_group_b,
             miscarriage_wiquint_sii, miscarriage_wiquint_sii_group_a, miscarriage_wiquint_sii_group_b)

#putting on scale of thousand pregnancies
sii$estimate <- sii$estimate*1000
sii$std.error <- sii$std.error*1000
sii$conf.low <- sii$conf.low*1000
sii$conf.high <- sii$conf.high*1000

dwsii <- dwplot(sii) +
  theme_bw() + xlab("Slope Index of Inequality per 1,000 pregnancies") + ylab("") + 
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  #ggtitle("Impact of Extended Leave on Probability of Holding a Paid Job") +
  theme_cowplot(12)+
  theme(legend.position = "none")

rii <- rbind(sb_wiquint_rii, sb_wiquint_rii_group_a, sb_wiquint_rii_group_b,
             abort_wiquint_rii, abort_wiquint_rii_group_a, abort_wiquint_rii_group_b,
             miscarriage_wiquint_rii, miscarriage_wiquint_rii_group_a, miscarriage_wiquint_rii_group_b)

dwrii <- dwplot(rii) +
  theme_bw() + xlab("Relative Index of Inequality") + ylab("") + 
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme_cowplot(12)+
  theme(legend.position = "none")

sii_estimates <- rbind(sbwi_sii, sbwi_sii_group_a, sbwi_sii_group_b,
                       abortwi_sii, abortwi_sii_group_a, abortwi_sii_group_b,
                       miscarriagewi_sii, miscarriagewi_sii_group_a, miscarriagewi_sii_group_b)

sii_conf.low <- rbind(sbwi_sii_conf.low, sbwi_sii_group_a_conf.low, sbwi_sii_group_b_conf.low,
                      abortwi_sii_conf.low, abortwi_sii_group_a_conf.low, abortwi_sii_group_b_conf.low,
                      miscarriagewi_sii_conf.low, miscarriagewi_sii_group_a_conf.low, miscarriagewi_sii_group_b_conf.low)

sii_conf.high <- rbind(sbwi_sii_conf.high, sbwi_sii_group_a_conf.high, sbwi_sii_group_b_conf.high,
                       abortwi_sii_conf.high, abortwi_sii_group_a_conf.high, abortwi_sii_group_b_conf.high,
                       miscarriagewi_sii_conf.high, miscarriagewi_sii_group_a_conf.high, miscarriagewi_sii_group_b_conf.high)

rii_estimates <- rbind(sbwi_rii, sbwi_rii_group_a, sbwi_rii_group_b,
                       abortwi_rii, abortwi_rii_group_a, abortwi_rii_group_b,
                       miscarriagewi_rii, miscarriagewi_rii_group_a, miscarriagewi_rii_group_b)

rii_conf.low <- rbind(sbwi_rii_conflow, sbwi_rii_group_a_conflow, sbwi_rii_group_b_conflow,
                      abortwi_rii_conflow, abortwi_rii_group_a_conflow, abortwi_rii_group_b_conflow,
                      miscarriagewi_rii_conflow, miscarriagewi_rii_group_a_conflow, miscarriagewi_rii_group_b_conflow)

rii_conf.high <- rbind(sbwi_rii_confhigh, sbwi_rii_group_a_confhigh, sbwi_rii_group_b_confhigh,
                       abortwi_rii_confhigh, abortwi_rii_group_a_confhigh, abortwi_rii_group_b_confhigh,
                       miscarriagewi_rii_confhigh, miscarriagewi_rii_group_a_confhigh, miscarriagewi_rii_group_b_confhigh)

sii <- cbind(sii_estimates, sii_conf.low, sii_conf.high)
sii <- as.data.frame(sii)
sii <- sii %>% rename(SII = V1, Conf.Low = V2, Conf.High = V3)
sii$Outcome <- c("Stillbirth All Years", "Stillbirth 2004-2009", "Stillbirth 2010-2019",
                 "Abortion All Years", "Abortion 2004-2009", "Abortion 2010-2019",
                 "Miscarriage All Years", "Miscarriage 2004-2009", "Miscarriage 2010-2019")
sii <- sii %>% relocate(Outcome, .before = SII)

rii <- cbind(rii_estimates, rii_conf.low, rii_conf.high)
rii <- as.data.frame(rii)
rii <- rii %>% rename(RII = V1, RIIConf.Low = V2, RIIConf.High = V3)

table2 <- cbind(sii, rii)

library(flextable)
require(gdtools)
fontname <- "Times New Roman"
border_style = officer::fp_border(color="black", width=1)

theme_vanilla()  %>% 
  
flextable(sii) %>% vline(part = "all", j = 1, border = border_style)  #%>% theme_vanilla()
flexsii  <- flextable(sii)
flexsii %>% vline(part = "all", j = 1, border = border_style)
flexsii %>% bold(i = 1, bold = TRUE, part = "header")

tab2 <- flextable(table2) %>% vline(part="all", j = 1, border = border_style) %>% vline(part="all", j = 4, border = border_style)
print(tab2, preview = "docx")



# Education RD and RR ---------------------------------------------------

#Using Naimi and Whitcomb (2020) in AJE as evidence for gaussian distribution

#RR: family = quasipoisson(link = "log") MUST TAKE NATURAL EXPONENT OF COEFFICIENT

#RD: family = quasibinomial(link = "identity") OR family = gaussian(link = "identity")

sb_prim_rr <- svyglm(sb ~ primary + age + outcome_year + as.factor(state), design = design, family = quasipoisson(link = "log"))
sb_prim_rr <- sb_prim_rr %>% tidy(conf.int = TRUE) 
sb_prim_rr 

#tidying for dot and whisker plot
sb_prim_rr <- sb_prim_rr %>% filter(row_number() %in% c(2))
sb_prim_rr$term[sb_prim_rr$term=="primary"] <- "Stillbirth All Years"



#NOW EXPONENTIATE THE COEFFICIENT

sb_prim_exp_rr <- exp(sb_prim_rr$estimate[2]) %>% round_half_up(digits = 2)
sb_prim_exp_conflow <- exp(sb_prim_rr$conf.low[2]) %>% round_half_up(digits = 2)
sb_prim_exp_confhigh <- exp(sb_prim_rr$conf.high[2]) %>% round_half_up(digits = 2)



#now pre and post
sb_prim_rr_group_a <- svyglm(sb ~ primary + age + outcome_year + as.factor(state), design = design_a, family = quasipoisson(link = "log"))
sb_prim_rr_group_a <- sb_prim_rr_group_a %>% tidy(conf.int = TRUE) 
sb_prim_rr_group_a 

#tidying for dot and whisker plot
sb_prim_rr_group_a <- sb_prim_rr_group_a %>% filter(row_number() %in% c(2))
sb_prim_rr_group_a$term[sb_prim_rr_group_a$term=="primary"] <- "Stillbirth 2004 - 2009"


#NOW EXPONENTIATE THE COEFFICIENT

sb_prim_exp_rr_group_a <- exp(sb_prim_rr_group_a$estimate[2]) %>% round_half_up(digits = 2)
sb_prim_exp_conflow_group_a <- exp(sb_prim_rr_group_a$conf.low[2]) %>% round_half_up(digits = 2)
sb_prim_exp_confhigh_group_a <- exp(sb_prim_rr_group_a$conf.high[2]) %>% round_half_up(digits = 2)

sb_prim_rr_group_b <- svyglm(sb ~ primary + age + outcome_year + as.factor(state), design = design_b, family = quasipoisson(link = "log"))
sb_prim_rr_group_b <- sb_prim_rr_group_b %>% tidy(conf.int = TRUE) 
sb_prim_rr_group_b 

#tidying for dot and whisker plot
sb_prim_rr_group_b <- sb_prim_rr_group_b %>% filter(row_number() %in% c(2))
sb_prim_rr_group_b$term[sb_prim_rr_group_b$term=="primary"] <- "Stillbirth 2010 - 2019"


#NOW EXPONENTIATE THE COEFFICIENT

sb_prim_exp_rr_group_b <- exp(sb_prim_rr_group_b$estimate[2]) %>% round_half_up(digits = 2)
sb_prim_exp_conflow_group_b <- exp(sb_prim_rr_group_b$conf.low[2]) %>% round_half_up(digits = 2)
sb_prim_exp_confhigh_group_b <- exp(sb_prim_rr_group_b$conf.high[2]) %>% round_half_up(digits = 2)



sb_prim_rd <- svyglm(sb ~ primary + age + outcome_year + as.factor(state), design = design, family = gaussian(link = "identity"))
sb_prim_rd <- sb_prim_rd %>% tidy(conf.int = TRUE)
sbprim_rd <- (sb_prim_rd$estimate[2]*1000) %>% round_half_up(digits = 2)
sbprim_rd_conflow <- (sb_prim_rd$conf.low[2]*1000) %>% round_half_up(digits = 2)
sbprim_rd_confhigh <- (sb_prim_rd$conf.high[2]*1000)  %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
sb_prim_rd <- sb_prim_rd %>% filter(row_number() %in% c(2))
sb_prim_rd$term[sb_prim_rd$term=="primary"] <- "Stillbirth All Years"


sb_prim_rd_group_a <- svyglm(sb ~ primary + age + outcome_year + as.factor(state), design = design_a, family = gaussian(link = "identity"))
sb_prim_rd_group_a <- sb_prim_rd_group_a %>% tidy(conf.int = TRUE)
sbprim_rd_group_a <- (sb_prim_rd_group_a$estimate[2]*1000) %>% round_half_up(digits = 2)
sbprim_rd_group_a_conflow <- (sb_prim_rd_group_a$conf.low[2]*1000) %>% round_half_up(digits = 2)
sbprim_rd_group_a_confhigh <- (sb_prim_rd_group_a$conf.high[2]*1000)  %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
sb_prim_rd_group_a <- sb_prim_rd_group_a %>% filter(row_number() %in% c(2))
sb_prim_rd_group_a$term[sb_prim_rd_group_a$term=="primary"] <- "Stillbirth 2004 - 2009"


sb_prim_rd_group_b <- svyglm(sb ~ primary + age + outcome_year + as.factor(state), design = design_b, family = gaussian(link = "identity"))
sb_prim_rd_group_b <- sb_prim_rd_group_b %>% tidy(conf.int = TRUE)
sbprim_rd_group_b <- (sb_prim_rd_group_b$estimate[2]*1000) %>% round_half_up(digits = 2)
sbprim_rd_group_b_conflow <- (sb_prim_rd_group_b$conf.low[2]*1000) %>% round_half_up(digits = 2)
sbprim_rd_group_b_confhigh <- (sb_prim_rd_group_b$conf.high[2]*1000)  %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
sb_prim_rd_group_b <- sb_prim_rd_group_b %>% filter(row_number() %in% c(2))
sb_prim_rd_group_b$term[sb_prim_rd_group_b$term=="primary"] <- "Stillbirth 2010 - 2019"


abort_prim_rr <- svyglm(abort ~ primary + age + outcome_year + as.factor(state), design = design, family = quasipoisson(link = "log"))
abort_prim_rr <- abort_prim_rr %>% tidy(conf.int = TRUE) 
abort_prim_rr 

#tidying for dot and whisker plot
abort_prim_rr <- abort_prim_rr %>% filter(row_number() %in% c(2))
abort_prim_rr$term[abort_prim_rr$term=="primary"] <- "Abortion All Years"


#NOW EXPONENTIATE THE COEFFICIENT

abort_prim_exp_rr <- exp(abort_prim_rr$estimate[2]) %>% round_half_up(digits = 2)
abort_prim_exp_conflow <- exp(abort_prim_rr$conf.low[2]) %>% round_half_up(digits = 2)
abort_prim_exp_confhigh <- exp(abort_prim_rr$conf.high[2]) %>% round_half_up(digits = 2)

abort_prim_rr_group_a <- svyglm(abort ~ primary + age + outcome_year + as.factor(state), design = design_a, family = quasipoisson(link = "log"))
abort_prim_rr_group_a <- abort_prim_rr_group_a %>% tidy(conf.int = TRUE) 
abort_prim_rr_group_a 

#tidying for dot and whisker plot
abort_prim_rr_group_a <- abort_prim_rr_group_a %>% filter(row_number() %in% c(2))
abort_prim_rr_group_a$term[abort_prim_rr_group_a$term=="primary"] <- "Abortion 2004 - 2009"


#NOW EXPONENTIATE THE COEFFICIENT

abort_prim_exp_rr_group_a <- exp(abort_prim_rr_group_a$estimate[2]) %>% round_half_up(digits = 2)
abort_prim_exp_conflow_group_a <- exp(abort_prim_rr_group_a$conf.low[2]) %>% round_half_up(digits = 2)
abort_prim_exp_confhigh_group_a <- exp(abort_prim_rr_group_a$conf.high[2]) %>% round_half_up(digits = 2)

abort_prim_rr_group_b <- svyglm(abort ~ primary + age + outcome_year + as.factor(state), design = design_b, family = quasipoisson(link = "log"))
abort_prim_rr_group_b <- abort_prim_rr_group_b %>% tidy(conf.int = TRUE) 
abort_prim_rr_group_b 

#tidying for dot and whisker plot
abort_prim_rr_group_b <- abort_prim_rr_group_b %>% filter(row_number() %in% c(2))
abort_prim_rr_group_b$term[abort_prim_rr_group_b$term=="primary"] <- "Abortion 2010 - 2019"


#NOW EXPONENTIATE THE COEFFICIENT

abort_prim_exp_rr_group_b <- exp(abort_prim_rr_group_b$estimate[2]) %>% round_half_up(digits = 2)
abort_prim_exp_conflow_group_b <- exp(abort_prim_rr_group_b$conf.low[2]) %>% round_half_up(digits = 2)
abort_prim_exp_confhigh_group_b <- exp(abort_prim_rr_group_b$conf.high[2]) %>% round_half_up(digits = 2)


abort_prim_rd <- svyglm(abort ~ primary + age + outcome_year + as.factor(state), design = design, family = gaussian(link = "identity"))
abort_prim_rd <- abort_prim_rd %>% tidy(conf.int = TRUE)
abortprim_rd <- (abort_prim_rd$estimate[2]*1000) %>% round_half_up(digits = 2)
abortprim_rd_conflow <- (abort_prim_rd$conf.low[2]*1000) %>% round_half_up(digits = 2)
abortprim_rd_confhigh <- (abort_prim_rd$conf.high[2]*1000)  %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
abort_prim_rd <- abort_prim_rd %>% filter(row_number() %in% c(2))
abort_prim_rd$term[abort_prim_rd$term=="primary"] <- "Abortion All Years"


abort_prim_rd_group_a <- svyglm(abort ~ primary + age + outcome_year + as.factor(state), design = design_a, family = gaussian(link = "identity"))
abort_prim_rd_group_a <- abort_prim_rd_group_a %>% tidy(conf.int = TRUE)
abortprim_rd_group_a <- (abort_prim_rd_group_a$estimate[2]*1000) %>% round_half_up(digits = 2)
abortprim_rd_group_a_conflow <- (abort_prim_rd_group_a$conf.low[2]*1000) %>% round_half_up(digits = 2)
abortprim_rd_group_a_confhigh <- (abort_prim_rd_group_a$conf.high[2]*1000)  %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
abort_prim_rd_group_a <- abort_prim_rd_group_a %>% filter(row_number() %in% c(2))
abort_prim_rd_group_a$term[abort_prim_rd_group_a$term=="primary"] <- "Abortion 2004 - 2009"


abort_prim_rd_group_b <- svyglm(abort ~ primary + age + outcome_year + as.factor(state), design = design_b, family = gaussian(link = "identity"))
abort_prim_rd_group_b <- abort_prim_rd_group_b %>% tidy(conf.int = TRUE)
abortprim_rd_group_b <- (abort_prim_rd_group_b$estimate[2]*1000) %>% round_half_up(digits = 2)
abortprim_rd_group_b_conflow <- (abort_prim_rd_group_b$conf.low[2]*1000) %>% round_half_up(digits = 2)
abortprim_rd_group_b_confhigh <- (abort_prim_rd_group_b$conf.high[2]*1000)  %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
abort_prim_rd_group_b <- abort_prim_rd_group_b %>% filter(row_number() %in% c(2))
abort_prim_rd_group_b$term[abort_prim_rd_group_b$term=="primary"] <- "Abortion 2010 - 2019"


miscarriage_prim_rr <- svyglm(miscarriage ~ primary + age + outcome_year + as.factor(state), design = design, family = quasipoisson(link = "log"))
miscarriage_prim_rr <- miscarriage_prim_rr %>% tidy(conf.int = TRUE) 
miscarriage_prim_rr 

#tidying for dot and whisker plot
miscarriage_prim_rr <- miscarriage_prim_rr %>% filter(row_number() %in% c(2))
miscarriage_prim_rr$term[miscarriage_prim_rr$term=="primary"] <- "Miscarriage All Years"


#NOW EXPONENTIATE THE COEFFICIENT

miscarriage_prim_exp_rr <- exp(miscarriage_prim_rr$estimate[2]) %>% round_half_up(digits = 2)
miscarriage_prim_exp_conflow <- exp(miscarriage_prim_rr$conf.low[2]) %>% round_half_up(digits = 2)
miscarriage_prim_exp_confhigh <- exp(miscarriage_prim_rr$conf.high[2]) %>% round_half_up(digits = 2)

miscarriage_prim_rr_group_a <- svyglm(miscarriage ~ primary + age + outcome_year + as.factor(state), design = design_a, family = quasipoisson(link = "log"))
miscarriage_prim_rr_group_a <- miscarriage_prim_rr_group_a %>% tidy(conf.int = TRUE) 
miscarriage_prim_rr_group_a 

#tidying for dot and whisker plot
miscarriage_prim_rr_group_a <- miscarriage_prim_rr_group_a %>% filter(row_number() %in% c(2))
miscarriage_prim_rr_group_a$term[miscarriage_prim_rr_group_a$term=="primary"] <- "Miscarriage 2004 - 2009"


#NOW EXPONENTIATE THE COEFFICIENT

miscarriage_prim_exp_rr_group_a <- exp(miscarriage_prim_rr_group_a$estimate[2]) %>% round_half_up(digits = 2)
miscarriage_prim_exp_conflow_group_a <- exp(miscarriage_prim_rr_group_a$conf.low[2]) %>% round_half_up(digits = 2)
miscarriage_prim_exp_confhigh_group_a <- exp(miscarriage_prim_rr_group_a$conf.high[2]) %>% round_half_up(digits = 2)

miscarriage_prim_rr_group_b <- svyglm(miscarriage ~ primary + age + outcome_year + as.factor(state), design = design_b, family = quasipoisson(link = "log"))
miscarriage_prim_rr_group_b <- miscarriage_prim_rr_group_b %>% tidy(conf.int = TRUE) 
miscarriage_prim_rr_group_b 

#tidying for dot and whisker plot
miscarriage_prim_rr_group_b <- miscarriage_prim_rr_group_b %>% filter(row_number() %in% c(2))
miscarriage_prim_rr_group_b$term[miscarriage_prim_rr_group_b$term=="primary"] <- "Miscarriage 2010 - 2019"




miscarriage_prim_rd <- svyglm(miscarriage ~ primary + age + outcome_year + as.factor(state), design = design, family = gaussian(link = "identity"))
miscarriage_prim_rd <- miscarriage_prim_rd %>% tidy(conf.int = TRUE)
miscarriageprim_rd <- (miscarriage_prim_rd$estimate[2]*1000) %>% round_half_up(digits = 2)
miscarriageprim_rd_conflow <- (miscarriage_prim_rd$conf.low[2]*1000) %>% round_half_up(digits = 2)
miscarriageprim_rd_confhigh <- (miscarriage_prim_rd$conf.high[2]*1000)  %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
miscarriage_prim_rd <- miscarriage_prim_rd %>% filter(row_number() %in% c(2))
miscarriage_prim_rd$term[miscarriage_prim_rd$term=="primary"] <- "Miscarriage All Years"


miscarriage_prim_rd_group_a <- svyglm(miscarriage ~ primary + age + outcome_year + as.factor(state), design = design_a, family = gaussian(link = "identity"))
miscarriage_prim_rd_group_a <- miscarriage_prim_rd_group_a %>% tidy(conf.int = TRUE)
miscarriageprim_rd_group_a <- (miscarriage_prim_rd_group_a$estimate[2]*1000) %>% round_half_up(digits = 2)
miscarriageprim_rd_group_a_conflow <- (miscarriage_prim_rd_group_a$conf.low[2]*1000) %>% round_half_up(digits = 2)
miscarriageprim_rd_group_a_confhigh <- (miscarriage_prim_rd_group_a$conf.high[2]*1000)  %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
miscarriage_prim_rd_group_a <- miscarriage_prim_rd_group_a %>% filter(row_number() %in% c(2))
miscarriage_prim_rd_group_a$term[miscarriage_prim_rd_group_a$term=="primary"] <- "Miscarriage 2004 - 2009"


miscarriage_prim_rd_group_b <- svyglm(miscarriage ~ primary + age + outcome_year + as.factor(state), design = design_b, family = gaussian(link = "identity"))
miscarriage_prim_rd_group_b <- miscarriage_prim_rd_group_b %>% tidy(conf.int = TRUE)
miscarriageprim_rd_group_b <- (miscarriage_prim_rd_group_b$estimate[2]*1000) %>% round_half_up(digits = 2)
miscarriageprim_rd_group_b_conflow <- (miscarriage_prim_rd_group_b$conf.low[2]*1000) %>% round_half_up(digits = 2)
miscarriageprim_rd_group_b_confhigh <- (miscarriage_prim_rd_group_b$conf.high[2]*1000)  %>% round_half_up(digits = 2)

#tidying for dot and whisker plot
miscarriage_prim_rd_group_b <- miscarriage_prim_rd_group_b %>% filter(row_number() %in% c(2))
miscarriage_prim_rd_group_b$term[miscarriage_prim_rd_group_b$term=="primary"] <- "Miscarriage 2010 - 2019"



rd_primary <- rbind(sb_prim_rd, sb_prim_rd_group_a, sb_prim_rd_group_b, 
             abort_prim_rd, abort_prim_rd_group_a, abort_prim_rd_group_b,
             miscarriage_prim_rd, miscarriage_prim_rd_group_a, miscarriage_prim_rd_group_b)

#putting in scale per 1,000 pregancies
rd_primary$estimate <- rd_primary$estimate*1000
rd_primary$std.error <- rd_primary$std.error*1000
rd_primary$conf.low <- rd_primary$conf.low*1000
rd_primary$conf.high <- rd_primary$conf.high*1000

dw_rd_primary <- dwplot(rd_primary) +
  theme_bw() + xlab("Risk Difference of Primary Education per 1,000 pregnancies") + ylab("") + 
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme_cowplot(12)+
  theme(legend.position = "none")


#rr_primary
rr_primary <- rbind(sb_prim_rr, sb_prim_rr_group_a, sb_prim_rr_group_b,
             abort_prim_rr, abort_prim_rr_group_a, abort_prim_rr_group_b,
             miscarriage_prim_rr, miscarriage_prim_rr_group_a, miscarriage_prim_rr_group_b)

#NOW EXPONENTIATE THE COEFFICIENT

rr_primary$estimate <- exp(rr_primary$estimate)
rr_primary$std.error <- exp(rr_primary$std.error)
rr_primary$conf.low <- exp(rr_primary$conf.low) 
rr_primary$conf.high <- exp(rr_primary$conf.high)


dw_rr_primary <- dwplot(rr_primary) +
  theme_bw() + xlab("Risk Ratio of Primary Educaiton") + ylab("") + 
  geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
  theme_cowplot(12)+
  theme(legend.position = "none")







rr_prim_estimates <- rbind(sb_prim_exp_rr, sb_prim_exp_rr_group_a, sb_prim_exp_rr_group_b,
                           abort_prim_exp_rr, abort_prim_exp_rr_group_a, abort_prim_exp_rr_group_b,
                           miscarriage_prim_exp_rr, miscarriage_prim_exp_rr_group_a, miscarriage_prim_exp_rr_group_b)



rr_prim_conf.low <- rbind(sb_prim_exp_conflow, abort_prim_exp_conflow, miscarriage_prim_exp_conflow)


rr_prim_conf.high <- rbind(sb_prim_exp_confhigh, abort_prim_exp_confhigh, miscarriage_prim_exp_confhigh)

rd_prim_estimates <- rbind(sbprim_rd, abortprim_rd, miscarriageprim_rd)
rd_prim_conf.low <- rbind(sbprim_rd_conflow, abortprim_rd_conflow, miscarriageprim_rd_conflow)
rd_prim_conf.high <- rbind(sbprim_rd_confhigh, abortprim_rd_confhigh, miscarriageprim_rd_confhigh)

rr_prim <- cbind(rr_prim_estimates, rr_prim_conf.low, rr_prim_conf.high)
rr_prim <- as.data.frame(rr_prim)
rr_prim <- rr_prim %>% rename(RR = V1, Conf.Low = V2, Conf.High = V3)
rr_prim$Outcome <- c("Stillbirth", "Abortion", "Miscarriage")
rr_prim <- rr_prim %>% relocate(Outcome, .before = RR)

rd_prim <- cbind(rd_prim_estimates, rd_prim_conf.low, rd_prim_conf.high)
rd_prim <- as.data.frame(rd_prim)
rd_prim <- rd_prim %>% rename(RD = V1, RDConf.Low = V2, RDConf.High = V3)

table3 <- cbind(rr_prim, rd_prim)

tab3 <- flextable(table3) %>% vline(part="all", j = 1, border = border_style) %>% vline(part="all", j = 4, border = border_style)
print(tab3, preview = "docx")



# Caste RD and RR -------------------------------------------------------
#RR: family = quasipoisson(link = "log") MUST TAKE NATURAL EXPONENT OF COEFFICIENT

#RD: family = quasibinomial(link = "identity") OR family = gaussian(link = "identity")

sb_caste_rr <- svyglm(sb ~ scheduled + age + outcome_year + as.factor(state), design = design, family = quasipoisson(link = "log"))
sb_caste_rr <- sb_caste_rr %>% tidy(conf.int = TRUE) 
sb_caste_rr 

#tidying for dot and whisker plot
sb_caste_rr <- sb_caste_rr %>% filter(row_number() %in% c(2))
sb_caste_rr$term[sb_caste_rr$term=="scheduled"] <- "Stillbirth All Years"


#NOW EXPONENTIATE THE COEFFICIENT

#sb_caste_exp_rr <- exp(sb_caste_rr$estimate[2]) %>% round_half_up(digits = 2)
#sb_caste_exp_conflow <- exp(sb_caste_rr$conf.low[2]) %>% round_half_up(digits = 2)
#sb_caste_exp_confhigh <- exp(sb_caste_rr$conf.high[2]) %>% round_half_up(digits = 2)

#now pre and post
sb_caste_rr_group_a <- svyglm(sb ~ scheduled + age + outcome_year + as.factor(state), design = design_a, family = quasipoisson(link = "log"))
sb_caste_rr_group_a <- sb_caste_rr_group_a %>% tidy(conf.int = TRUE) 
sb_caste_rr_group_a 

#tidying for dot and whisker plot
sb_caste_rr_group_a <- sb_caste_rr_group_a %>% filter(row_number() %in% c(2))
sb_caste_rr_group_a$term[sb_caste_rr_group_a$term=="scheduled"] <- "Stillbirth 2004 - 2009"


#now pre and post
sb_caste_rr_group_b <- svyglm(sb ~ scheduled + age + outcome_year + as.factor(state), design = design_b, family = quasipoisson(link = "log"))
sb_caste_rr_group_b <- sb_caste_rr_group_b %>% tidy(conf.int = TRUE) 
sb_caste_rr_group_b 

#tidying for dot and whisker plot
sb_caste_rr_group_b <- sb_caste_rr_group_b %>% filter(row_number() %in% c(2))
sb_caste_rr_group_b$term[sb_caste_rr_group_b$term=="scheduled"] <- "Stillbirth 2010 - 2019"


sb_caste_rd <- svyglm(sb ~ scheduled + age + outcome_year + as.factor(state), design = design, family = gaussian(link = "identity"))
sb_caste_rd <- sb_caste_rd %>% tidy(conf.int = TRUE)
#sbcaste_rd <- (sb_caste_rd$estimate[2]*1000) %>% round_half_up(digits = 2)
#sbcaste_rd_conflow <- (sb_caste_rd$conf.low[2]*1000) %>% round_half_up(digits = 2)
#sbcaste_rd_confhigh <- (sb_caste_rd$conf.high[2]*1000)  %>% round_half_up(digits = 2)


#tidying for dot and whisker plot
sb_caste_rd <- sb_caste_rd %>% filter(row_number() %in% c(2))
sb_caste_rd$term[sb_caste_rd$term=="scheduled"] <- "Stillbirth All Years"

#now pre and post
sb_caste_rd_group_a <- svyglm(sb ~ scheduled + age + outcome_year + as.factor(state), design = design_a, family = gaussian(link = "identity"))
sb_caste_rd_group_a <- sb_caste_rd_group_a %>% tidy(conf.int = TRUE) 
sb_caste_rd_group_a 

#tidying for dot and whisker plot
sb_caste_rd_group_a <- sb_caste_rd_group_a %>% filter(row_number() %in% c(2))
sb_caste_rd_group_a$term[sb_caste_rd_group_a$term=="scheduled"] <- "Stillbirth 2004 - 2009"

#now pre and post
sb_caste_rd_group_b <- svyglm(sb ~ scheduled + age + outcome_year + as.factor(state), design = design_b, family = gaussian(link = "identity"))
sb_caste_rd_group_b <- sb_caste_rd_group_b %>% tidy(conf.int = TRUE) 
sb_caste_rd_group_b 

#tidying for dot and whisker plot
sb_caste_rd_group_b <- sb_caste_rd_group_b %>% filter(row_number() %in% c(2))
sb_caste_rd_group_b$term[sb_caste_rd_group_b$term=="scheduled"] <- "Stillbirth 2010 - 2019"


abort_caste_rr <- svyglm(abort ~ scheduled + age + outcome_year + as.factor(state), design = design, family = quasipoisson(link = "log"))
abort_caste_rr <- abort_caste_rr %>% tidy(conf.int = TRUE) 
abort_caste_rr 

abort_caste_rr <- abort_caste_rr %>% filter(row_number() %in% c(2))
abort_caste_rr$term[abort_caste_rr$term == "scheduled"] <- "Abortion All Years"

#NOW EXPONENTIATE THE COEFFICIENT

abort_caste_exp_rr <- exp(abort_caste_rr$estimate[2]) %>% round_half_up(digits = 2)
abort_caste_exp_conflow <- exp(abort_caste_rr$conf.low[2]) %>% round_half_up(digits = 2)
abort_caste_exp_confhigh <- exp(abort_caste_rr$conf.high[2]) %>% round_half_up(digits = 2)

abort_caste_rr_group_a <- svyglm(abort ~ scheduled + age + outcome_year + as.factor(state), design = design_a, family = quasipoisson(link = "log"))
abort_caste_rr_group_a <- abort_caste_rr_group_a %>% tidy(conf.int = TRUE) 
abort_caste_rr_group_a 

abort_caste_rr_group_a <- abort_caste_rr_group_a %>% filter(row_number() %in% c(2))
abort_caste_rr_group_a$term[abort_caste_rr_group_a$term == "scheduled"] <- "Abortion 2004 - 2009"

abort_caste_rr_group_b <- svyglm(abort ~ scheduled + age + outcome_year + as.factor(state), design = design_b, family = quasipoisson(link = "log"))
abort_caste_rr_group_b <- abort_caste_rr_group_b %>% tidy(conf.int = TRUE) 
abort_caste_rr_group_b 

abort_caste_rr_group_b <- abort_caste_rr_group_b %>% filter(row_number() %in% c(2))
abort_caste_rr_group_b$term[abort_caste_rr_group_b$term == "scheduled"] <- "Abortion 2010 - 2019"


abort_caste_rd <- svyglm(abort ~ scheduled + age + outcome_year + as.factor(state), design = design, family = gaussian(link = "identity"))
abort_caste_rd <- abort_caste_rd %>% tidy(conf.int = TRUE)
#abortcaste_rd <- (abort_caste_rd$estimate[2]*1000) %>% round_half_up(digits = 2)
#abortcaste_rd_conflow <- (abort_caste_rd$conf.low[2]*1000) %>% round_half_up(digits = 2)
#abortcaste_rd_confhigh <- (abort_caste_rd$conf.high[2]*1000)  %>% round_half_up(digits = 2)

abort_caste_rd <- abort_caste_rd %>% filter(row_number() %in% c(2))
abort_caste_rd$term[abort_caste_rd$term == "scheduled"] <- "Abortion All Years"

abort_caste_rd_group_a <- svyglm(abort ~ scheduled + age + outcome_year + as.factor(state), design = design_a, family = gaussian(link = "identity"))
abort_caste_rd_group_a <- abort_caste_rd_group_a %>% tidy(conf.int = TRUE) 
abort_caste_rd_group_a 

abort_caste_rd_group_a <- abort_caste_rd_group_a %>% filter(row_number() %in% c(2))
abort_caste_rd_group_a$term[abort_caste_rd_group_a$term == "scheduled"] <- "Abortion 2004 - 2009"

abort_caste_rd_group_b <- svyglm(abort ~ scheduled + age + outcome_year + as.factor(state), design = design_b, family = gaussian(link = "identity"))
abort_caste_rd_group_b <- abort_caste_rd_group_b %>% tidy(conf.int = TRUE) 
abort_caste_rd_group_b 

abort_caste_rd_group_b <- abort_caste_rd_group_b %>% filter(row_number() %in% c(2))
abort_caste_rd_group_b$term[abort_caste_rd_group_b$term == "scheduled"] <- "Abortion 2010 - 2019"


miscarriage_caste_rr <- svyglm(miscarriage ~ scheduled + age + outcome_year + as.factor(state), design = design, family = quasipoisson(link = "log"))
miscarriage_caste_rr <- miscarriage_caste_rr %>% tidy(conf.int = TRUE) 
miscarriage_caste_rr 

#NOW EXPONENTIATE THE COEFFICIENT

#miscarriage_caste_exp_rr <- exp(miscarriage_caste_rr$estimate[2]) %>% round_half_up(digits = 2)
#miscarriage_caste_exp_conflow <- exp(miscarriage_caste_rr$conf.low[2]) %>% round_half_up(digits = 2)
#miscarriage_caste_exp_confhigh <- exp(miscarriage_caste_rr$conf.high[2]) %>% round_half_up(digits = 2)

miscarriage_caste_rr <- miscarriage_caste_rr %>% filter(row_number() %in% c(2))
miscarriage_caste_rr$term[miscarriage_caste_rr$term == "scheduled"] <- "Miscarriage All Years"

miscarriage_caste_rr_group_a <- svyglm(miscarriage ~ scheduled + age + outcome_year + as.factor(state), 
                                       design = design_a, family = quasipoisson(link = "log"))
miscarriage_caste_rr_group_a <- miscarriage_caste_rr_group_a %>% tidy(conf.int = TRUE) 
miscarriage_caste_rr_group_a 

miscarriage_caste_rr_group_a <- miscarriage_caste_rr_group_a %>% filter(row_number() %in% c(2))
miscarriage_caste_rr_group_a$term[miscarriage_caste_rr_group_a$term == "scheduled"] <- "Miscarriage 2004 - 2009"

miscarriage_caste_rr_group_b <- svyglm(miscarriage ~ scheduled + age + outcome_year + as.factor(state), design = design_b, 
                                       family = quasipoisson(link = "log"))
miscarriage_caste_rr_group_b <- miscarriage_caste_rr_group_b %>% tidy(conf.int = TRUE) 
miscarriage_caste_rr_group_b 

miscarriage_caste_rr_group_b <- miscarriage_caste_rr_group_b %>% filter(row_number() %in% c(2))
miscarriage_caste_rr_group_b$term[miscarriage_caste_rr_group_b$term == "scheduled"] <- "Miscarriage 2010 - 2019"


miscarriage_caste_rd <- svyglm(miscarriage ~ scheduled + age + outcome_year + as.factor(state), design = design, family = gaussian(link = "identity"))
miscarriage_caste_rd <- miscarriage_caste_rd %>% tidy(conf.int = TRUE)
#miscarriagecaste_rd <- (miscarriage_caste_rd$estimate[2]*1000) %>% round_half_up(digits = 2)
#miscarriagecaste_rd_conflow <- (miscarriage_caste_rd$conf.low[2]*1000) %>% round_half_up(digits = 2)
#miscarriagecaste_rd_confhigh <- (miscarriage_caste_rd$conf.high[2]*1000)  %>% round_half_up(digits = 2)

miscarriage_caste_rd <- miscarriage_caste_rd %>% filter(row_number() %in% c(2))
miscarriage_caste_rd$term[miscarriage_caste_rd$term == "scheduled"] <- "Miscarriage All Years"

miscarriage_caste_rd_group_a <- svyglm(miscarriage ~ scheduled + age + outcome_year + as.factor(state), design = design_a, 
                                       family = gaussian(link = "identity"))
miscarriage_caste_rd_group_a <- miscarriage_caste_rd_group_a %>% tidy(conf.int = TRUE) 
miscarriage_caste_rd_group_a 

miscarriage_caste_rd_group_a <- miscarriage_caste_rd_group_a %>% filter(row_number() %in% c(2))
miscarriage_caste_rd_group_a$term[miscarriage_caste_rd_group_a$term == "scheduled"] <- "Miscarriage 2004 - 2009"

miscarriage_caste_rd_group_b <- svyglm(miscarriage ~ scheduled + age + outcome_year + as.factor(state), design = design_b, family = gaussian(link = "identity"))
miscarriage_caste_rd_group_b <- miscarriage_caste_rd_group_b %>% tidy(conf.int = TRUE) 
miscarriage_caste_rd_group_b 

miscarriage_caste_rd_group_b <- miscarriage_caste_rd_group_b %>% filter(row_number() %in% c(2))
miscarriage_caste_rd_group_b$term[miscarriage_caste_rd_group_b$term == "scheduled"] <- "Miscarriage 2010 - 2019"

rd_caste <- rbind(sb_caste_rd, sb_caste_rd_group_a, sb_caste_rd_group_b, 
                    abort_caste_rd, abort_caste_rd_group_a, abort_caste_rd_group_b,
                    miscarriage_caste_rd, miscarriage_caste_rd_group_a, miscarriage_caste_rd_group_b)

#putting in scale per 1,000 pregancies
rd_caste$estimate <- rd_caste$estimate*1000
rd_caste$std.error <- rd_caste$std.error*1000
rd_caste$conf.low <- rd_caste$conf.low*1000
rd_caste$conf.high <- rd_caste$conf.high*1000

dw_rd_caste <- dwplot(rd_caste) +
  theme_bw() + xlab("Risk Difference by Caste") + ylab("") + 
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme_cowplot(12)+
  theme(legend.position = "none")


#rr_primary
rr_caste <- rbind(sb_caste_rr, sb_caste_rr_group_a, sb_caste_rr_group_b,
                    abort_caste_rr, abort_caste_rr_group_a, abort_caste_rr_group_b,
                    miscarriage_caste_rr, miscarriage_caste_rr_group_a, miscarriage_caste_rr_group_b)

#NOW EXPONENTIATE THE COEFFICIENT

rr_caste$estimate <- exp(rr_caste$estimate)
rr_caste$std.error <- exp(rr_caste$std.error)
rr_caste$conf.low <- exp(rr_caste$conf.low) 
rr_caste$conf.high <- exp(rr_caste$conf.high)


dw_rr_caste <- dwplot(rr_caste) +
  theme_bw() + xlab("Risk Ratio Caste") + ylab("") + 
  geom_vline(xintercept = 1, colour = "grey60", linetype = 2) +
  theme_cowplot(12)+
  theme(legend.position = "none")




rr_caste_estimates <- rbind(sb_caste_exp_rr, abort_caste_exp_rr, miscarriage_caste_exp_rr)
rr_caste_conf.low <- rbind(sb_caste_exp_conflow, abort_caste_exp_conflow, miscarriage_caste_exp_conflow)
rr_caste_conf.high <- rbind(sb_caste_exp_confhigh, abort_caste_exp_confhigh, miscarriage_caste_exp_confhigh)

rd_caste_estimates <- rbind(sbcaste_rd, abortcaste_rd, miscarriagecaste_rd)
rd_caste_conf.low <- rbind(sbcaste_rd_conflow, abortcaste_rd_conflow, miscarriagecaste_rd_conflow)
rd_caste_conf.high <- rbind(sbcaste_rd_confhigh, abortcaste_rd_confhigh, miscarriagecaste_rd_confhigh)

rr_caste <- cbind(rr_caste_estimates, rr_caste_conf.low, rr_caste_conf.high)
rr_caste <- as.data.frame(rr_caste)
rr_caste <- rr_caste %>% rename(RR = V1, Conf.Low = V2, Conf.High = V3)
rr_caste$Outcome <- c("Stillbirth", "Abortion", "Miscarriage")
rr_caste <- rr_caste %>% relocate(Outcome, .before = RR)

rd_caste <- cbind(rd_caste_estimates, rd_caste_conf.low, rd_caste_conf.high)
rd_caste <- as.data.frame(rd_caste)
rd_caste <- rd_caste %>% rename(RD = V1, RDConf.Low = V2, RDConf.High = V3)

table4 <- cbind(rr_caste, rd_caste)

tab4 <- flextable(table4) %>% vline(part="all", j = 1, border = border_style) %>% vline(part="all", j = 4, border = border_style)
print(tab4, preview = "docx")


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


# Figures code ------------------------------------------------------------

fig_rates <- ggarrange(sbwirate, dwsii,
                  #labels = c("A", "B", "C"),
                  ncol = 1, nrow = 2, common.legend = FALSE, legend = "right")


# Table 1 creation --------------------------------------------------------

df %>% 
  select(age, age_first_birth, tot_live_births, primary, outcome_year, insurance) %>%
  tbl_summary()   


#rural == 0, 1s as they must be urbans.

df$rural_urban <- factor(df$strat_rurb, 
                         levels = c(1, 2),
                         labels = c("Urban", "Rural"))

df$scheduled_c_t <- factor(df$caste_group,
                           levels = c(0,1,2),
                           labels = c("None", "Scheduled Caste", "Scheduled Tribe"))


df %>% 
  select(age, rural_urban, scheduled_c_t,tot_live_births, primary, insurance) %>% 
  tbl_summary(
  label = list(
    age ~ "Age",
    rural_urban ~ "Rural / Urban",
    scheduled_c_t ~ "Member of Scheduled Caste or Scheduled Tribe",
    #age_first_birth ~ "Age at first birth",
    tot_live_births ~ "Total live births",
    primary ~ "Completed Primary School",
    #outcome_year ~ "Year of Pregnancy Outcome",
    insurance ~ "Household has any insurance"),
  #statistic = list(all_continuous() ~ "{mean} ({sd})"),
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


