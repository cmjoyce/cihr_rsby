
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



                                                    