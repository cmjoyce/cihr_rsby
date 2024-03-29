
# Long District Enrollment Data -------------------------------------------

setwd("./District enrollment")

library(tidyverse)

dist <- read.csv("LongDistrictEnrollment.csv")

glimpse(dist)


# Making long -------------------------------------------------------------


df <- dist %>%
    pivot_longer(Total_2010:Enrolled_2016,
               names_to = c(".value", "year"),
               names_pattern = "(.*)_(.*)",
               values_to = c("Total, Enrolled")
  )



# Creating % Enrolled variables -------------------------------------------

dist$percent2010 <- dist$Enrolled2010/dist$Total2010
dist$percent2011 <- dist$Enrolled2011/dist$Total2011
dist$percent2012 <- dist$Enrolled2012/dist$Total2012
dist$percent2013 <- dist$Enrolled2013/dist$Total2013
dist$percent2014 <- dist$Enrolled2014/dist$Total2014
dist$percent2015 <- dist$Enrolled2015/dist$Total2015
dist$percent2016 <- dist$Enrolled2016/dist$Total2016

df$percent <- df$Enrolled/df$Total

# Creating district enrollment by year variable ---------------------------

#next steps have to create y/n enrollment in each year for policy year variable
#start with indicator variable -- 2010 == T, 2011 == T, etc.

dist$any2010 <- ifelse(is.na(dist$Enrolled2010), 0, 1)
dist$any2011 <- ifelse(is.na(dist$Enrolled2011), 0, 1)
dist$any2012 <- ifelse(is.na(dist$Enrolled2012), 0, 1)
dist$any2013 <- ifelse(is.na(dist$Enrolled2013), 0, 1)
dist$any2014 <- ifelse(is.na(dist$Enrolled2014), 0, 1) 
dist$any2015 <- ifelse(is.na(dist$Enrolled2015), 0, 1) 
dist$any2016 <- ifelse(is.na(dist$Enrolled2016), 0, 1) 

df$policy <- ifelse(is.na(df$Enrolled), 0, 1)

# Grouping by district -------------------------------------------------------

dist <- dist %>% group_by(State)

df <- df %>% group_by(District)


# Creating years of policy variable ---------------------------------------

dist$policyyears <- (dist$any2010 + dist$any2011 + dist$any2012 + dist$any2013 + dist$any2014 + dist$any2015 + 
                       dist$any2016)

df <- df %>% mutate(policyyears = length(which(policy == 1)))



# Creating region variable ------------------------------------------------

dist$region[dist$State == "Chandigarh" ] <- "Northern"
dist$region[dist$State == "Delhi" ] <- "Northern"
dist$region[dist$State == "Haryana" ] <- "Northern"
dist$region[dist$State == "Himachal Pradesh" ] <- "Northern"
dist$region[dist$State == "Jammu and Kashmir" ] <- "Northern"
dist$region[dist$State == "Punjab" ] <- "Northern"
dist$region[dist$State == "Rajasthan" ] <- "Northern"

dist$region[dist$State == "Assam"] <- "North Eastern"
dist$region[dist$State == "Arunachal Pradesh"] <- "North Eastern"
dist$region[dist$State == "Manipur"] <- "North Eastern"
dist$region[dist$State == "Meghalaya"] <- "North Eastern"
dist$region[dist$State == "Mizoram"] <- "North Eastern"
dist$region[dist$State == "Nagaland"] <- "North Eastern"
dist$region[dist$State == "Tripura"] <- "North Eastern"

dist$region[dist$State == "Chhattisgarh"] <- "Central"
dist$region[dist$State == "Madhya Pradesh"] <- "Central"
dist$region[dist$State == "Uttrakhand"] <- "Central"
dist$region[dist$State == "Uttar Pradesh"] <- "Central"

dist$region[dist$State == "Bihar"] <- "Eastern"
dist$region[dist$State == "Jharkand"] <- "Eastern"
dist$region[dist$State == "Orrisa"] <- "Eastern"
dist$region[dist$State == "West Bengal"] <- "Eastern"

dist$region[dist$State == "Goa"] <- "Western"
dist$region[dist$State == "Gujarat"] <- "Western"
dist$region[dist$State == "Maharashtra"] <- "Western"

dist$region[dist$State == "Andhra Pradesh"] <- "Southern"
dist$region[dist$State == "Karnataka"] <- "Southern"
dist$region[dist$State == "Kerala"] <- "Southern"
dist$region[dist$State == "Pondicherry"] <- "Southern"
dist$region[dist$State == "Tamil Nadu"] <- "Southern"


df$region[df$State == "Chandigarh" ] <- "Northern"
df$region[df$State == "Delhi" ] <- "Northern"
df$region[df$State == "Haryana" ] <- "Northern"
df$region[df$State == "Himachal Pradesh" ] <- "Northern"
df$region[df$State == "Jammu and Kashmir" ] <- "Northern"
df$region[df$State == "Punjab" ] <- "Northern"
df$region[df$State == "Rajasthan" ] <- "Northern"

df$region[df$State == "Assam"] <- "North Eastern"
df$region[df$State == "Arunachal Pradesh"] <- "North Eastern"
df$region[df$State == "Manipur"] <- "North Eastern"
df$region[df$State == "Meghalaya"] <- "North Eastern"
df$region[df$State == "Mizoram"] <- "North Eastern"
df$region[df$State == "Nagaland"] <- "North Eastern"
df$region[df$State == "Tripura"] <- "North Eastern"

df$region[df$State == "Chhattisgarh"] <- "Central"
df$region[df$State == "Madhya Pradesh"] <- "Central"
df$region[df$State == "Uttrakhand"] <- "Central"
df$region[df$State == "Uttar Pradesh"] <- "Central"

df$region[df$State == "Bihar"] <- "Eastern"
df$region[df$State == "Jharkand"] <- "Eastern"
df$region[df$State == "Orrisa"] <- "Eastern"
df$region[df$State == "West Bengal"] <- "Eastern"

df$region[df$State == "Goa"] <- "Western"
df$region[df$State == "Gujarat"] <- "Western"
df$region[df$State == "Maharashtra"] <- "Western"

df$region[df$State == "Andhra Pradesh"] <- "Southern"
df$region[df$State == "Karnataka"] <- "Southern"
df$region[df$State == "Kerala"] <- "Southern"
df$region[df$State == "Pondicherry"] <- "Southern"
df$region[df$State == "Tamil Nadu"] <- "Southern"


# Creating % district enrollment by state variable ------------------------


dist <- dist %>% mutate(stateenrollment2010 = ave(any2010))
dist <- dist %>% mutate(stateenrollment2011 = ave(any2011))
dist <- dist %>% mutate(stateenrollment2012 = ave(any2012))
dist <- dist %>% mutate(stateenrollment2013 = ave(any2013))
dist <- dist %>% mutate(stateenrollment2014 = ave(any2014))
dist <- dist %>% mutate(stateenrollment2015 = ave(any2015))
dist <- dist %>% mutate(stateenrollment2016 = ave(any2016))

#redoing grouping and adding in % of districts enrolled by each state by each year

df <- df %>%
  group_by(State, year) %>%
  mutate(percent_state_enrolled=ave(policy))


# subsetting states -------------------------------------------------------

state <- data.frame(df$State, df$year, df$percent_state_enrolled, df$percentstate)

state <- distinct(state)

stateplot <- ggplot(state, aes(x = df.State, y = df.percentstate))
stateplot + geom_col() + facet_grid(cols = vars(df.year)) + theme(axis.text.x = element_text(angle = 315)) 

stateplot + geom_col() + facet_wrap(vars(df.year)) #+ theme(axis.text.x = element_text(angle = 315)) 



# Saving CSV --------------------------------------------------------------

#write.csv(df, "districtenrollment.csv")

dist <- read.csv("districtenrollment.csv")

# plotting % enrolled by region -------------------------------------------

#removing % enrolled calcuatlions where there are more enrolled than total, as data error from gov website
df$percent <- ifelse(df$percent > 1, NA, df$percent)

df$percentstate <- df$percent_state_enrolled*100

p <- ggplot(df, aes(x = State, y = percent))

p + geom_point(mapping = aes(color = year))
  
p1 <- ggplot(df, aes(x = year, y = percentstate))

p1 + geom_point(mapping = aes(color = region))

p1 + geom_boxplot()

p2 <- ggplot(df, aes(x = State, y = percentstate))

p2 + geom_point(mapping = aes(color = year)) + theme(axis.text.x = element_text(angle = 315)) 

p2 + geom_point(mapping = aes(color = year)) + coord_flip()

p2 + stat_summary(fun = mean, geom = "bar") + theme(axis.text.x = element_text(angle = 315)) 

p3 <- ggplot(df, aes(x = year, y = Enrolled))

p3 + geom_point(mapping = aes(color = region))

p4 <- ggplot(df, aes(x = year, y = Enrolled))

p4 + geom_col(mapping = aes(color = region))

p5 <- ggplot(df, aes(State, policy))

p5 + geom_col(mapping = aes(color = year)) + theme(axis.text.x = element_text(angle = 315)) 

p5 + geom_col() + coord_flip()

p5 + facet_wrap(cols = vars(year)) 



# matching state to coding from codebook ----------------------------------

table(dist$State)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#trimming to remove and leading or trailing spaces
dist$State <- trim(dist$State)

dist <- dist %>% mutate(state_match = case_when(State == "Andhra Pradesh" ~ 27,
                                                State == "Arunachal Pradesh" ~ 12,
                                                State == "Assam" ~ 18,
                                                State == "Bihar" ~ 10,
                                                State == "Chandigarh" ~ 4,
                                                State == "Chhattisgarh" ~ 22,
                                                State == "Delhi" ~ 7,
                                                State == "Goa" ~ 29,
                                                State == "Gujarat" ~ 24,
                                                State == "Haryana" ~ 6,
                                                State == "Himachal Pradesh" ~ 2,
                                                State == "Jammu and Kashmir" ~ 1,
                                                State == "Jharkand" ~ 20,
                                                State == "Karnataka" ~ 28,
                                                State == "Kerala" ~ 31,
                                                State == "Madhya Pradesh" ~ 23,
                                                State == "Maharashtra" ~ 26,
                                                State == "Manipur" ~ 14,
                                                State == "Meghalaya" ~ 17,
                                                State == "Mizoram" ~ 15,
                                                State == "Nagaland" ~ 13,
                                                State == "Orrisa" ~ 21,
                                                State == "Pondicherry" ~ 33,
                                                State == "Punjab" ~ 3,
                                                State == "Rajasthan" ~ 8,
                                                State == "Tamil Nadu" ~ 32,
                                                State == "Tripura" ~ 16,
                                                State == "Uttar Pradesh" ~ 9,
                                                State == "Uttrakhand" ~ 5,
                                                State == "West Bengal" ~ 19,
                                                TRUE ~ NA_real_))
  


#now need to add in correct district code to match on name + state
#using "districts_harmonized.csv" that was created in District harmonization 

dist_names <- read.csv("districts_harmonized.csv")
dist_names <- dist_names %>% select(c(dist_id, namefix))

#using df (harmonized variables) from Aim1.R file
dist_state <- df %>% select(c(state, dist_id))

dist_names_state <- left_join(dist_names, dist_state)

#dropping districts without state values as that means they are not in our sample
dist_names_state <- dist_names_state %>% filter(!is.na(state))

dist_names_state <- dist_names_state %>% rename(District = namefix, state_match = state)

dist_check <- left_join(dist, dist_names_state, by = c("District", "state_match"))

dist_check <- unique(dist_check)

#write.csv(dist_check, "dist_check_unique.csv")

dist_check <- read.csv("dist_check_unique.csv")

check <- dist_check %>% filter(is.na(dist_id))

#updating names for consistency across all datasets

#fix district names and match to spelling in district harmonization file

#for districts that changed from 2010 onwards using changes made already in district harominization

check_dist <- dist %>% mutate(district_fix = case_when(District == "Rangareddi" ~ "Rangareddy",
                                              District == "Aurangabad" ~ "Aurangabad_bihar",
                                              District == "Kaimur" ~ "Kaimur Bhabua",
                                              District == "Balod" ~ "Durg",
                                              District ==	"Balodabazar"	~	"Raipur",
                                              District ==	"Balrampur"	~	"Surguja",
                                              District ==	"Bemetara"	~	"Durg",
                                              District ==	"Gariyaband"	~	"Raipur",
                                              District ==	"Gaurella-Pendra-Marwahi"	~	"Bilaspur",
                                              District == "Janjgir Champa" ~ " Janjgir-Champa",
                                              District ==	"Kondagaon"	~	"Bastar",
                                              District ==	"Mungeli"	~	"Bilaspur",
                                              District == "Narayanpur" ~ "Bastar",
                                              District ==	"Sukma"	~	"Dantewada",
                                              District ==	"Surajpur"	~	"Surguja",
                                              District == "Bhuj" ~ " Kachchh",
                                              District == "Dahod" ~ "Dohad",
                                              District ==	"Tapi"	~	"Surat",
                                              District == "Bilaspur" & State == "Himachal Pradesh" ~ "Bilaspur_hp",
                                              District == "Hamirpur" & State == "Himachal Pradesh" ~ "Hamirpur_hp",
                                              District == "Lahul And Spiti" ~ "Lahul Spiti",
                                              District == "Palwal" ~ "Faridabad",
                                              District == "Hazaribag" ~ "Hazaribagh",
                                              District ==	"Khunti"	~	"Ranchi",
                                              District ==	"Ramgarh"	~	"Hazaribagh",
                                              District == "Saraikela" ~ "Saraikela Kharsawan",
                                              District == "Budgam" ~ "Badgam",
                                              District == "Poonch" ~ "Punch",
                                              District == "Rajauri" ~ "Rajouri",
                                              District == "Chamrajnagar" ~ "Chamarajanagar",
                                              District == "Chikballapur" ~	"Kolar",
                                              District == "Davangere" ~ "Davanagere",
                                              District ==	"Ramanagara"	~	"Bangalore Rural",
                                              District ==	"Yadgir"	~	"Kalaburagi", 
                                              District ==	"East Jaintia Hills"	~	"Jaintia Hills", 
                                              District == "West Jaintia Hills"	~	"Jaintia Hills", 
                                              District ==	"North Garo Hills"	~	"East Garo Hills", 
                                              District ==	"South West Garo Hills"	~	"West Garo Hills", 
                                              District ==	"South West Khasi Hills"	~	"West Khasi Hills",
                                              District == "Aizwal" ~ "Aizawl",
                                              District == "Anugul" ~ "Anugul",
                                              District == "Balasore" ~ "Baleshwar",
                                              District == "Bolangir" ~ "Balangir",
                                              District == "Boudh" ~ "Baudh",
                                              District == "Deogarh" ~ "Debagarh",
                                              District == "Jagatsinghpur" ~ "Jagatsinghapur",
                                              District == "Jajpur" ~ "Jajapur",
                                              District == "Keonjhar" ~ "Kendujhar",
                                              District == "Raygada" ~ "Rayagada",
                                              District == "Sonepur" ~ "Sonapur",
                                              District == "Aurangabad" ~ "Aurangabad_maharashtra",
                                              District == "Buldhana" ~ "Buldana",
                                              District == "Gondia" ~ "Gondiya",
                                              District == "Raigarh" & State == "Maharashtra" ~ "Raigarh_mh",
                                              District == "Anuppur" ~ "Shahdol",
                                              District == "Ashoknagar" ~ "Guna",
                                              District == "Burhanpur" ~ "East Nimar",
                                              District == "Khandwa" ~ "East Nimar",
                                              District == "Khargone West Nimar" ~ "West Nimar",
                                              District == "Narsinghpur" ~ "Narsimhapur",
                                              District == "Pondicherry" ~ "Puducherry",
                                              District == "Fazilka" ~ "Firozpur",
                                              District == "Mohali" ~ "Sas Nagar",
                                              District == "Pathankot" ~ "Gurdaspur",
                                              District == "Chittorgarh" ~ "Chittaurgarh",
                                              District == "Dholpur" ~ "Dhaulpur",
                                              District == "Jhunjhunu" ~ "Jhunjhunun",
                                              District ==	"Pratapgarh"	~	"Chittaurgarh",
                                              District == "Sri Ganganagar" ~ "Ganganagar",
                                              District ==	"Gomathi"	~	"Dhalai",
                                              District ==	"Khowai"	~	"West Tripura",
                                              District ==	"Sepahijala"	~	"West Tripura",
                                              District ==	"Unakoti"	~	"North Tripura",
                                              District == "Balrampur" ~ "Balrampur_up",
                                              District == "Bhim Nagar" ~ "Moradabad",
                                              District == "Chatrapati Shahuji Maharaj Nagar" ~ "Sultanpur",
                                              District == "Kashiram Nagar" ~ "Etah",
                                              District == "Panchsheel Nagar" ~ "Ghaziabad",
                                              District == "Prabudh Nagar" ~  "Muzaffarnagar",
                                              District == "Pratapgarh" & State == "Uttar Pradesh" ~
                                                "Pratapgarh_up",
                                              District == "Sant Ravidas Nagar" ~ "Sant Ravidas Nagar Bhadohi",
                                              District == "Haridwar" ~ "Hardwar",
                                              District ==	"Alipurduar"	~	"Jalpaiguri",
                                              District == "Coochbehar" ~ "Koch Bihar",
                                              District == "Darjeeling" ~ "Darjiling",
                                              District == "Darjiling GTA" ~ "Darjiling",
                                              District == "Hoogly" ~ "Hugli",
                                              District == "Howrah" ~ "Haora",
                                              District == "North 24 Parganas" ~ "North Twenty Four Parganas",
                                              District == "Purba Midnapore" ~ "Purba Medinipur",
                                              District == "Purulia" ~ "Puruliya",
                                              District == "South 24 Parganas" ~ "South Twenty Four Parganas",
                                              TRUE ~ District))

#now doing same process again
#using "districts_harmonized.csv" that was created in District harmonization 

dist_names_state <- dist_names_state %>% rename(district_fix = District)

check_dist <- left_join(check_dist, dist_names_state, by = c("district_fix"))

check_dist <- unique(check_dist)


                                                    