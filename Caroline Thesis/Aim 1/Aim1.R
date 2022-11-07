# Aim 1 import ------------------------------------------------------
library(tidyverse)
setwd("./Caroline Thesis/Aim 1")

df <- read.csv("harmonized_variables.csv")


# Making binary pregnancy outcome variables -------------------------------

df$stillbirth


# Fixing districts to match -----------------------------------------------


#district names that duplicate across multiple states were relabeled as district_state, i.e. aurangabad_bihar and aurangabad_maharashtra

#trimming trailing spaces

#library(fuzzyjoin)

#Labels taken from STATA and made into new dataframe with their values. 
# All districts that changed in the study time frame were found on wikipedia. This is saved as district_changes.csv

#district names that duplicate across multiple states were relabeled as district_state, i.e. aurangabad_bihar and aurangabad_maharashtra
# If a district is a new district *and* the name is a repeat between states than it was left as just the district name and relabeled with old district name.


# In DLHS-3 both 2327 and 2328 are labeled West Nimar. I relabeled 2328 to match DLHS4 as Barwani.
# Districts labeled north/south/east/west repeat between Sikkim and Daman&Diu. For uniqueness, have renamed the Sikkim districts to be district_sikkim, 
#i.e. north_sikkim

# bilaspur and hamirpur district in himachal pradesh was relabeled as bilaspur_hp and hamirpur district.

#raigarh district in mahaarashtra was renamed as raigarh_mh

# This was done in the file "district_names" to ensure the IDs matched the correct state in district comparison

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
names <- read.csv("district_names.csv")


names$DLHS3 <- str_c(names$DLHS3a, names$DLHS3b, sep=" ")
names <- names %>% select(-c(DLHS3a, DLHS3b))

names$DLHS4 <- str_to_title(names$DLHS4)
names$DLHS3 <- str_to_title(names$DLHS3)
names$NFHS4_AHS <- str_to_title(names$NFHS4_AHS)
names$NFHS5 <- str_to_title(names$NFHS5)



names$NFHS4_AHS <- trim(names$NFHS4_AHS)
names$DLHS3 <- trim(names$DLHS3)
names$DLHS4 <- trim(names$DLHS4)
names$NFHS5 <- trim(names$NFHS5)





#now making separate dataframes so can merge them based on district name

dlhs <- names %>% select(c(DLHS3, DLHS3_id, DLHS4, DLHS4_id))
nfhsahs <- names %>% select(c(NFHS4_AHS, NFHS_AHS_id))
nfhs5 <- names %>% select(c(NFHS5, NFHS5_id))

dlhs4 <- dlhs %>% select(c(DLHS4, DLHS4_id))
dlhs3 <- dlhs %>% select(c(DLHS3, DLHS3_id))

dlhs3 <- dlhs3 %>% rename(district = DLHS3)
dlhs4 <- dlhs4 %>% rename(district = DLHS4)
nfhsahs <- nfhsahs %>% rename(district = NFHS4_AHS)
nfhs5 <- nfhs5 %>% rename(district = NFHS5)

dlhs3 <- dlhs3 %>% filter(!is.na(DLHS3_id))
dlhs4 <- dlhs4 %>% filter(!is.na(DLHS4_id))

#mutating old districts to new districts
#most changes are from wikipedia. Small changes added here i.e. Y.S.R. = old Cuddapah

nfhs5 <- nfhs5 %>% mutate(namefix = case_when(district ==	"Longding"	~	"Tirap",
                                              district ==	"Namsai"	~	"Lohit",
                                              district ==	"Kra Daadi"	~	"Kurung Kumey",
                                              district ==	"Siang"	~	"East Siang", 
                                              district ==	"Lower Siang"	~	"West Siang",
                                              district ==	"Kamle"	~	"Lower Subansiri", 
                                              district ==	"Pakke-Kesang"	~	"East Kesang", 
                                              district ==	"Lepa-Rada"	~	"Lower Siang", 
                                              district ==	"Shi-Yomi"	~	"West Siang", 
                                              district ==	"Dima Hasao"	~	"North Cachar Hills", 
                                              district ==	"Biswanath"	~	"Sonitpur", 
                                              district ==	"Charaideo"	~	"Sibsagar", 
                                              district ==	"Hojai"	~	"Nagaon", 
                                              district ==	"South Salmara-Mankachar"	~	"Dhubri", 
                                              district ==	"West Karbi Anglong"	~	"Karbi Anglong", 
                                              district ==	"Kamrup Metropolitan District"	~	"East Kamrup", 
                                              district ==	"Kamrup Rural district"	~	"South Kamrup", 
                                              district ==	"Majuli"	~	"Jorhat", 
                                              district ==	"Balod"	~	"Durg", 
                                              district ==	"Baloda Bazar"	~	"Raipur", 
                                              district ==	"Balrampur"	~	"Surguja", 
                                              district ==	"Bemetara"	~	"Durg", 
                                              district ==	"Gariaband"	~	"Raipur", 
                                              district ==	"Gaurella-Pendra-Marwahi"	~	"Bilaspur", 
                                              district ==	"Kodagaon"	~	"Bastar", 
                                              district == "Narayanpur" ~ "Bastar",
                                              district ==	"Mungeli"	~	"Bilaspur", 
                                              district ==	"Sukma"	~	"Dantewada", 
                                              district ==	"Surajpur"	~	"Surguja", 
                                              district ==	"Tapi"	~	"Surat", 
                                              district ==	"Aravali"	~	"Sabar Kantha", 
                                              district ==	"Botad"	~	"Ahmedabad", 
                                              district ==	"Chhota Udaipur"	~	"Vadodara", 
                                              district ==	"Mahisagar"	~	"Kheda", 
                                              district ==	"Morbi"	~	"Rajkot", 
                                              district ==	"Gir Somnath"	~	"Junagadh", 
                                              district ==	"Charkhi Dadri"	~	"Bhiwani", 
                                              district ==	"Khunti"	~	"Ranchi", 
                                              district ==	"Ramgarh"	~	"Hazaribagh", 
                                              district ==	"Ramanagara"	~	"Bangalore Rural", 
                                              district ==	"Chikkaballapura"	~	"Kolar", 
                                              district ==	"Yadgir"	~	"Kalaburagi", 
                                              district ==	"Vijayanagara"	~	"Ballari", 
                                              district ==	"Palghar"	~	"Thane", 
                                              district ==	"Jiribam"	~	"Imphal East", 
                                              district ==	"Kamjong"	~	"Ukhrul", 
                                              district ==	"Kangpokpi"	~	"Senapati", 
                                              district ==	"Noney"	~	"Tamenglong", 
                                              district ==	"Tengnoupal"	~	"Chandel", 
                                              district ==	"Pherzawl"	~	"Churachanpur", 
                                              district ==	"Kakching"	~	"Thoubal", 
                                              district ==	"Eastern West Khasi Hills"	~	"East Garo Hills", 
                                              district ==	"North Garo Hills"	~	"East Garo Hills", 
                                              district ==	"South West Garo Hills"	~	"West Garo Hills", 
                                              district ==	"West Jaintia Hills"	~	"Jaintia Hills", 
                                              district ==	"East Jaintia Hills"	~	"Jaintia Hills", 
                                              district ==	"South West Khasi Hills"	~	"West Khasi Hills", 
                                              district ==	"Pratapgarh"	~	"Chittaurgarh",
                                              district ==	"Tiruppur"	~	"Coimbatore",
                                              district ==	"Komaram Bheem Asifabad"	~	"Adilabad", 
                                              district ==	"Mancherial"	~	"Adilabad", 
                                              district ==	"Nirmal"	~	"Adilabad", 
                                              district ==	"Kamareddy"	~	"Nizamabad",
                                              district ==	"Jayashankar Bhupalpally"	~	"Warangal",
                                              district ==	"Jagital"	~	"Karimnagar",
                                              district ==	"Peddapalli"	~	"Karimnagar",
                                              district ==	"Rajanna Sircilla"	~	"Karimnagar",
                                              district ==	"Jangaon"	~	"Warangal",
                                              district ==	"Hanumakonda"	~	"Warangal",
                                              district ==	"Warangal Rural"	~	"Warangal",
                                              district ==	"Mahabubabad"	~	"Warangal",
                                              district ==	"Yadadri Bhuvanagiri"	~	"Nalgonda",
                                              district ==	"Vikarabad"	~	"Rangareddy",
                                              district ==	"Medchal-Malkajgiri"	~	"Rangareddy",
                                              district ==	"Nagarkurnool"	~	"Mahbubnagar",
                                              district ==	"Jogulamba Gadwal"	~	"Mahbubnagar",
                                              district ==	"Gomati"	~	"Dhalai",
                                              district ==	"Khowai"	~	"West Tripura",
                                              district ==	"Sepahijala"	~	"West Tripura",
                                              district ==	"Unakoti"	~	"North Tripura",
                                              district ==	"Paschim Barddhaman"	~	"Barddhaman",
                                              district ==	"Purba Barddhaman"	~	"Barddhaman",
                                              district ==	"Jhargram"	~	"Paschim Medinipur",
                                              district ==	"Kalimpong"	~	"Darjeeling",
                                              district ==	"Alipurduar"	~	"Jalpaiguri",
                                              district ==	"North And Middle Andaman"	~	"Andamans",
                                              district ==	"South Andaman"	~	"Andamans",
                                              district ==	"Samba"	~	"Jammu",
                                              district ==	"Reasi"	~	"Udhampur",
                                              district ==	"Ramban"	~	"Doda",
                                              district ==	"Kishtwar"	~	"Doda",
                                              district ==	"Kulgam"	~	"Anantnang",
                                              district ==	"Shupiyan"	~	"Pulwama",
                                              district ==	"Ganderbal"	~	"Srinagar",
                                              district ==	"Bandipore"	~	"Baramulla",
                                              district ==	"South East"	~	"South",
                                              district ==	"Shahdara"	~	"North East",
                                              district == "Y.s.r." ~ "Cuddapah",
                                              district == "Lahul And Spiti" ~ "Lahul Spiti",
                                              district == "Palwal" ~ "Faridabad",
                                              district == "Sivasagar" ~ "Sibsagar",
                                              district == "Kabirdham" ~ "Kawardha",
                                              district == "Uttar Bastar Kanker" ~ "Kanker",
                                              district == "Sri Potti Sriramulu Nellore" ~ "Nellore",
                                              district == "Ashoknagar" ~ "Guna",
                                              district == "Anuppur" ~ "Shahdol",
                                              district == "Burhanpur" ~ "East Nimar",
                                              district == "Shahid Bhagat Singh Nagar" ~	"Nawanshahr",
                                              district == "Mahamaya Nagar" ~ "Hathras",
                                              district == "South Salmara Mancachar" ~ "Dhubri",
                                              district == "Gariyaband" ~ "Raipur",
                                              district == "Agar Malwa" ~ "Shajapur",
                                              district == "Fazilka" ~ "Firozpur",
                                              district == "Pathankot" ~ "Gurdaspur",
                                              district == "Bhadradri Kothagudem" ~ "Khammam",
                                              district == "Jayashankar Bhupalapally" ~ "Warangal",
                                              district == "Sangareddy" ~ "Medak",
                                              district == "Siddipet" ~ "Medak",
                                              district == "Suryapet" ~ "Nalgonda",
                                              district == "Wanaparthy" ~ "Mahbubnagar",
                                              district == "Warangal Urban" ~ "Warangal",
                                              district == "Amethi" ~ "Sultanpur",
                                              district == "Hapur" ~ "Ghaziabad",
                                              district == "Sambhal" ~ "Moradabad",
                                              district == "Shamli" ~ "Muzaffarnagar",
                                              district ==	"Jagtial"	~	"Karimnagar",
                                              district ==	"Jangoan"	~	"Warangal",
                                              district == "Kanshiram Nagar" ~ "Etah",
                                              district == "Devbhumi Dwarka" ~ 	"Jamnagar",
                                              district == "Alirajpur" ~ "Jhabua",
                                              district == "Singrauli" ~ "Sidhi",
                                              TRUE ~ district))

nfhsahs <- nfhsahs %>% mutate(namefix = case_when(district ==	"Longding"	~	"Tirap",
                                                district ==	"Namsai"	~	"Lohit",
                                                district ==	"Kra Daadi"	~	"Kurung Kumey",
                                                district ==	"Siang"	~	"East Siang", 
                                                district ==	"Lower Siang"	~	"West Siang",
                                                district ==	"Kamle"	~	"Lower Subansiri", 
                                                district ==	"Pakke-Kesang"	~	"East Kesang", 
                                                district ==	"Lepa-Rada"	~	"Lower Siang", 
                                                district ==	"Shi-Yomi"	~	"West Siang", 
                                                district ==	"Dima Hasao"	~	"North Cachar Hills", 
                                                district ==	"Biswanath"	~	"Sonitpur", 
                                                district ==	"Charaideo"	~	"Sibsagar", 
                                                district ==	"Hojai"	~	"Nagaon", 
                                                district ==	"South Salmara-Mankachar"	~	"Dhubri", 
                                                district ==	"West Karbi Anglong"	~	"Karbi Anglong", 
                                                district ==	"Kamrup Metropolitan District"	~	"East Kamrup", 
                                                district ==	"Kamrup Rural district"	~	"South Kamrup", 
                                                district ==	"Majuli"	~	"Jorhat", 
                                                district ==	"Balod"	~	"Durg", 
                                                district ==	"Baloda Bazar"	~	"Raipur", 
                                                district ==	"Balrampur"	~	"Surguja", 
                                                district ==	"Bemetara"	~	"Durg", 
                                                district ==	"Gariaband"	~	"Raipur", 
                                                district ==	"Gaurella-Pendra-Marwahi"	~	"Bilaspur", 
                                                district ==	"Kodagaon"	~	"Bastar", 
                                                district ==	"Mungeli"	~	"Bilaspur", 
                                                district ==	"Sukma"	~	"Dantewada", 
                                                district ==	"Surajpur"	~	"Surguja", 
                                                district ==	"Tapi"	~	"Surat", 
                                                district ==	"Aravali"	~	"Sabar Kantha", 
                                                district ==	"Botad"	~	"Ahmedabad", 
                                                district ==	"Chhota Udaipur"	~	"Vadodara", 
                                                district ==	"Mahisagar"	~	"Kheda", 
                                                district ==	"Morbi"	~	"Rajkot", 
                                                district ==	"Gir Somnath"	~	"Junagadh", 
                                                district ==	"Charkhi Dadri"	~	"Bhiwani", 
                                                district ==	"Khunti"	~	"Ranchi", 
                                                district ==	"Ramgarh"	~	"Hazaribagh", 
                                                district ==	"Ramanagara"	~	"Bangalore Rural", 
                                                district ==	"Chikkaballapura"	~	"Kolar", 
                                                district ==	"Yadgir"	~	"Kalaburagi", 
                                                district ==	"Vijayanagara"	~	"Ballari", 
                                                district ==	"Palghar"	~	"Thane", 
                                                district ==	"Jiribam"	~	"Imphal East", 
                                                district ==	"Kamjong"	~	"Ukhrul", 
                                                district ==	"Kangpokpi"	~	"Senapati", 
                                                district ==	"Noney"	~	"Tamenglong", 
                                                district ==	"Tengnoupal"	~	"Chandel", 
                                                district ==	"Pherzawl"	~	"Churachanpur", 
                                                district ==	"Kakching"	~	"Thoubal", 
                                                district ==	"Eastern West Khasi Hills"	~	"East Garo Hills", 
                                                district ==	"North Garo Hills"	~	"East Garo Hills", 
                                                district ==	"South West Garo Hills"	~	"West Garo Hills", 
                                                district ==	"West Jaintia Hills"	~	"Jaintia Hills", 
                                                district ==	"East Jaintia Hills"	~	"Jaintia Hills", 
                                                district ==	"South West Khasi Hills"	~	"West Khasi Hills", 
                                                district ==	"Pratapgarh"	~	"Chittaurgarh",
                                                district ==	"Tiruppur"	~	"Coimbatore",
                                                district ==	"Komaram Bheem Asifabad"	~	"Adilabad", 
                                                district ==	"Mancherial"	~	"Adilabad", 
                                                district ==	"Nirmal"	~	"Adilabad", 
                                                district ==	"Kamareddy"	~	"Nizamabad",
                                                district ==	"Jayashankar Bhupalpally"	~	"Warangal",
                                                district ==	"Jagtial"	~	"Karimnagar",
                                                district ==	"Peddapalli"	~	"Karimnagar",
                                                district ==	"Rajanna Sircilla"	~	"Karimnagar",
                                                district ==	"Jangoan"	~	"Warangal",
                                                district ==	"Hanumakonda"	~	"Warangal",
                                                district ==	"Warangal Rural"	~	"Warangal",
                                                district ==	"Mahabubabad"	~	"Warangal",
                                                district ==	"Yadadri Bhuvanagiri"	~	"Nalgonda",
                                                district ==	"Vikarabad"	~	"Rangareddy",
                                                district ==	"Medchal-Malkajgiri"	~	"Rangareddy",
                                                district ==	"Nagarkurnool"	~	"Mahbubnagar",
                                                district ==	"Jogulamba Gadwal"	~	"Mahbubnagar",
                                                district ==	"Gomati"	~	"Dhalai",
                                                district ==	"Khowai"	~	"West Tripura",
                                                district ==	"Sepahijala"	~	"West Tripura",
                                                district ==	"Unakoti"	~	"North Tripura",
                                                district ==	"Paschim Barddhaman"	~	"Barddhaman",
                                                district ==	"Purba Barddhaman"	~	"Barddhaman",
                                                district ==	"Jhargram"	~	"Paschim Medinipur",
                                                district ==	"Kalimpong"	~	"Darjeeling",
                                                district ==	"Alipurduar"	~	"Jalpaiguri",
                                                district ==	"North And Middle Andaman"	~	"Andamans",
                                                district ==	"South Andaman"	~	"Andamans",
                                                district ==	"Samba"	~	"Jammu",
                                                district ==	"Reasi"	~	"Udhampur",
                                                district ==	"Ramban"	~	"Doda",
                                                district ==	"Kishtwar"	~	"Doda",
                                                district ==	"Kulgam"	~	"Anantnang",
                                                district ==	"Shupiyan"	~	"Pulwama",
                                                district ==	"Ganderbal"	~	"Srinagar",
                                                district ==	"Bandipore"	~	"Baramulla",
                                                district ==	"South East"	~	"South",
                                                district ==	"Shahdara"	~	"North East",
                                                district == "Y.s.r." ~ "Cuddapah",
                                                district == "Lahul And Spiti" ~ "Lahul Spiti",
                                                district == "Palwal" ~ "Faridabad",
                                                district == "Sivasagar" ~ "Sibsagar",
                                                district == "Kabirdham" ~ "Kawardha",
                                                district == "Uttar Bastar Kanker" ~ "Kanker",
                                                district == "Narayanpur" ~ "Bastar",
                                                district == "Sri Potti Sriramulu Nellore" ~ "Nellore",
                                                district == "Ashoknagar" ~ "Guna",
                                                district == "Anuppur" ~ "Shahdol",
                                                district == "Burhanpur" ~ "East Nimar",
                                                district == "Shahid Bhagat Singh Nagar" ~	"Nawanshahr",
                                                district == "Devbhumi Dwarka" ~ 	"Jamnagar",
                                                district == "Kanshiram Nagar" ~ "Etah",
                                                district == "Alirajpur" ~ "Jhabua",
                                                district == "Singrauli" ~ "Sidhi",
                                                TRUE ~ district))

nfhsahs <- nfhsahs %>% filter(!is.na(nfhsahs$NFHS_AHS_id))

dlhs4 <- dlhs4 %>%  mutate(namefix = case_when(district ==	"Longding"	~	"Tirap",
                                               district ==	"Namsai"	~	"Lohit",
                                               district ==	"Kra Daadi"	~	"Kurung Kumey",
                                               district ==	"Siang"	~	"East Siang",
                                               district ==	"Lower Siang"	~	"West Siang",
                                               district ==	"Kamle"	~	"Lower Subansiri",
                                               district ==	"Pakke-Kesang"	~	"East Kesang",
                                               district ==	"Lepa-Rada"	~	"Lower Siang",
                                               district ==	"Shi-Yomi"	~	"West Siang",
                                               district ==	"Dima Hasao"	~	"North Cachar Hills",
                                               district ==	"Biswanath"	~	"Sonitpur",
                                               district ==	"Charaideo"	~	"Sibsagar",
                                               district ==	"Hojai"	~	"Nagaon",
                                               district ==	"South Salmara-Mankachar"	~	"Dhubri",
                                               district ==	"West Karbi Anglong"	~	"Karbi Anglong",
                                               district ==	"Kamrup Metropolitan District"	~	"East Kamrup",
                                               district ==	"Kamrup Rural district"	~	"South Kamrup",
                                               district ==	"Majuli"	~	"Jorhat",
                                               district ==	"Balod"	~	"Durg",
                                               district ==	"Baloda Bazar"	~	"Raipur",
                                               district ==	"Balrampur"	~	"Surguja",
                                               district ==	"Bemetara"	~	"Durg",
                                               district ==	"Gariaband"	~	"Raipur",
                                               district ==	"Gaurella-Pendra-Marwahi"	~	"Bilaspur",
                                               district ==	"Kodagaon"	~	"Bastar",
                                               district ==	"Mungeli"	~	"Bilaspur",
                                               district ==	"Sukma"	~	"Dantewada",
                                               district ==	"Surajpur"	~	"Surguja",
                                               district ==	"Tapi"	~	"Surat",
                                               district ==	"Aravali"	~	"Sabar Kantha",
                                               district ==	"Botad"	~	"Ahmedabad",
                                               district ==	"Chhota Udaipur"	~	"Vadodara",
                                               district ==	"Mahisagar"	~	"Kheda",
                                               district ==	"Morbi"	~	"Rajkot",
                                               district ==	"Gir Somnath"	~	"Junagadh",
                                               district ==	"Charkhi Dadri"	~	"Bhiwani",
                                               district ==	"Khunti"	~	"Ranchi",
                                               district ==	"Ramgarh"	~	"Hazaribagh",
                                               district ==	"Ramanagara"	~	"Bangalore Rural",
                                               district ==	"Chikkaballapura"	~	"Kolar",
                                               district ==	"Yadgir"	~	"Kalaburagi",
                                               district ==	"Vijayanagara"	~	"Ballari",
                                               district ==	"Palghar"	~	"Thane",
                                               district ==	"Jiribam"	~	"Imphal East",
                                               district ==	"Kamjong"	~	"Ukhrul",
                                               district ==	"Kangpokpi"	~	"Senapati",
                                               district ==	"Noney"	~	"Tamenglong",
                                               district ==	"Tengnoupal"	~	"Chandel",
                                               district ==	"Pherzawl"	~	"Churachanpur",
                                               district ==	"Kakching"	~	"Thoubal",
                                               district ==	"Eastern West Khasi Hills"	~	"East Garo Hills",
                                               district ==	"North Garo Hills"	~	"East Garo Hills",
                                               district ==	"South West Garo Hills"	~	"West Garo Hills",
                                               district ==	"West Jaintia Hills"	~	"Jaintia Hills",
                                               district ==	"East Jaintia Hills"	~	"Jaintia Hills",
                                               district ==	"South West Khasi Hills"	~	"West Khasi Hills",
                                               district ==	"Pratapgarh"	~	"Chittaurgarh",
                                               district ==	"Tiruppur"	~	"Coimbatore",
                                               district ==	"Komaram Bheem Asifabad"	~	"Adilabad",
                                               district ==	"Mancherial"	~	"Adilabad",
                                               district ==	"Nirmal"	~	"Adilabad",
                                               district ==	"Kamareddy"	~	"Nizamabad",
                                               district ==	"Jayashankar Bhupalpally"	~	"Warangal",
                                               district ==	"Jagital"	~	"Karimnagar",
                                               district ==	"Peddapalli"	~	"Karimnagar",
                                               district ==	"Rajanna Sircilla"	~	"Karimnagar",
                                               district ==	"Jangoan"	~	"Warangal",
                                               district ==	"Hanumakonda"	~	"Warangal",
                                               district ==	"Warangal Rural"	~	"Warangal",
                                               district ==	"Mahabubabad"	~	"Warangal",
                                               district ==	"Yadadri Bhuvanagiri"	~	"Nalgonda",
                                               district ==	"Vikarabad"	~	"Rangareddy",
                                               district ==	"Medchal-Malkajgiri"	~	"Rangareddy",
                                               district ==	"Nagarkurnool"	~	"Mahbubnagar",
                                               district ==	"Jogulamba Gadwal"	~	"Mahbubnagar",
                                               district ==	"Gomati"	~	"Dhalai",
                                               district ==	"Khowai"	~	"West Tripura",
                                               district ==	"Sepahijala"	~	"West Tripura",
                                               district ==	"Unakoti"	~	"North Tripura",
                                               district ==	"Paschim Barddhaman"	~	"Barddhaman",
                                               district ==	"Purba Barddhaman"	~	"Barddhaman",
                                               district ==	"Jhargram"	~	"Paschim Medinipur",
                                               district ==	"Kalimpong"	~	"Darjeeling",
                                               district ==	"Alipurduar"	~	"Jalpaiguri",
                                               district ==	"North And Middle Andaman"	~	"Andamans",
                                               district ==	"South Andaman"	~	"Andamans",
                                               district ==	"Samba"	~	"Jammu",
                                               district ==	"Reasi"	~	"Udhampur",
                                               district ==	"Ramban"	~	"Doda",
                                               district ==	"Kishtwar"	~	"Doda",
                                               district ==	"Kulgam"	~	"Anantnang",
                                               district ==	"Shupiyan"	~	"Pulwama",
                                               district ==	"Ganderbal"	~	"Srinagar",
                                               district ==	"Bandipore"	~	"Baramulla",
                                               district ==	"South East"	~	"South",
                                               district ==	"Shahdara"	~	"North East",
                                               district == "Lahul And Spiti" ~ "Lahul Spiti",
                                               district == "Palwal" ~ "Faridabad",
                                               district == "Sri Potti Sriramulu Nellore" ~ "Nellore",
                                               district == "Ashoknagar" ~ "Guna",
                                               district == "Anuppur" ~ "Shahdol",
                                               district == "Burhanpur" ~ "East Nimar",
                                               district == "Shahid Bhagat Singh Nagar" ~	"Nawanshahr",
                                               TRUE ~ district))

dlhs4 <- dlhs4 %>% filter(!is.na(DLHS4_id))

#All DLHS3 district names should stay the same as they are the base. This is a check. 

dlhs3 <- dlhs3 %>%  mutate(namefix = case_when(district ==	"Longding"	~	"Tirap",
                                               district ==	"Namsai"	~	"Lohit",
                                               district ==	"Kra Daadi"	~	"Kurung Kumey",
                                               district ==	"Siang"	~	"East Siang", 
                                               district ==	"Lower Siang"	~	"West Siang",
                                               district ==	"Kamle"	~	"Lower Subansiri", 
                                               district ==	"Pakke-Kesang"	~	"East Kesang", 
                                               district ==	"Lepa-Rada"	~	"Lower Siang", 
                                               district ==	"Shi-Yomi"	~	"West Siang", 
                                               district ==	"Dima Hasao"	~	"North Cachar Hills", 
                                               district ==	"Biswanath"	~	"Sonitpur", 
                                               district ==	"Charaideo"	~	"Sibsagar", 
                                               district ==	"Hojai"	~	"Nagaon", 
                                               district ==	"South Salmara-Mankachar"	~	"Dhubri", 
                                               district ==	"West Karbi Anglong"	~	"Karbi Anglong", 
                                               district ==	"Kamrup Metropolitan District"	~	"East Kamrup", 
                                               district ==	"Kamrup Rural district"	~	"South Kamrup", 
                                               district ==	"Majuli"	~	"Jorhat", 
                                               district ==	"Balod"	~	"Durg", 
                                               district ==	"Baloda Bazar"	~	"Raipur", 
                                               district ==	"Balrampur"	~	"Surguja", 
                                               district ==	"Bemetara"	~	"Durg", 
                                               district ==	"Gariaband"	~	"Raipur", 
                                               district ==	"Gaurella-Pendra-Marwahi"	~	"Bilaspur", 
                                               district ==	"Kodagaon"	~	"Bastar", 
                                               district ==	"Mungeli"	~	"Bilaspur", 
                                               district ==	"Sukma"	~	"Dantewada", 
                                               district ==	"Surajpur"	~	"Surguja", 
                                               district ==	"Tapi"	~	"Surat", 
                                               district ==	"Aravali"	~	"Sabar Kantha", 
                                               district ==	"Botad"	~	"Ahmedabad", 
                                               district ==	"Chhota Udaipur"	~	"Vadodara", 
                                               district ==	"Mahisagar"	~	"Kheda", 
                                               district ==	"Morbi"	~	"Rajkot", 
                                               district ==	"Gir Somnath"	~	"Junagadh", 
                                               district ==	"Charkhi Dadri"	~	"Bhiwani", 
                                               district ==	"Khunti"	~	"Ranchi", 
                                               district ==	"Ramgarh"	~	"Hazaribagh", 
                                               district ==	"Ramanagara"	~	"Bangalore Rural", 
                                               district ==	"Chikkaballapura"	~	"Kolar", 
                                               district ==	"Yadgir"	~	"Kalaburagi", 
                                               district ==	"Vijayanagara"	~	"Ballari", 
                                               district ==	"Palghar"	~	"Thane", 
                                               district ==	"Jiribam"	~	"Imphal East", 
                                               district ==	"Kamjong"	~	"Ukhrul", 
                                               district ==	"Kangpokpi"	~	"Senapati", 
                                               district ==	"Noney"	~	"Tamenglong", 
                                               district ==	"Tengnoupal"	~	"Chandel", 
                                               district ==	"Pherzawl"	~	"Churachanpur", 
                                               district ==	"Kakching"	~	"Thoubal", 
                                               district ==	"Eastern West Khasi Hills"	~	"East Garo Hills", 
                                               district ==	"North Garo Hills"	~	"East Garo Hills", 
                                               district ==	"South West Garo Hills"	~	"West Garo Hills", 
                                               district ==	"West Jaintia Hills"	~	"Jaintia Hills", 
                                               district ==	"East Jaintia Hills"	~	"Jaintia Hills", 
                                               district ==	"South West Khasi Hills"	~	"West Khasi Hills", 
                                               district ==	"Pratapgarh"	~	"Chittaurgarh",
                                               district ==	"Tiruppur"	~	"Coimbatore",
                                               district ==	"Komaram Bheem Asifabad"	~	"Adilabad", 
                                               district ==	"Mancherial"	~	"Adilabad", 
                                               district ==	"Nirmal"	~	"Adilabad", 
                                               district ==	"Kamareddy"	~	"Nizamabad",
                                               district ==	"Jayashankar Bhupalpally"	~	"Warangal",
                                               district ==	"Jagital"	~	"Karimnagar",
                                               district ==	"Peddapalli"	~	"Karimnagar",
                                               district ==	"Rajanna Sircilla"	~	"Karimnagar",
                                               district ==	"Jangaon"	~	"Warangal",
                                               district ==	"Hanumakonda"	~	"Warangal",
                                               district ==	"Warangal Rural"	~	"Warangal",
                                               district ==	"Mahabubabad"	~	"Warangal",
                                               district ==	"Yadadri Bhuvanagiri"	~	"Nalgonda",
                                               district ==	"Vikarabad"	~	"Rangareddy",
                                               district ==	"Medchal-Malkajgiri"	~	"Rangareddy",
                                               district ==	"Nagarkurnool"	~	"Mahbubnagar",
                                               district ==	"Jogulamba Gadwal"	~	"Mahbubnagar",
                                               district ==	"Gomati"	~	"Dhalai",
                                               district ==	"Khowai"	~	"West Tripura",
                                               district ==	"Sepahijala"	~	"West Tripura",
                                               district ==	"Unakoti"	~	"North Tripura",
                                               district ==	"Paschim Barddhaman"	~	"Barddhaman",
                                               district ==	"Purba Barddhaman"	~	"Barddhaman",
                                               district ==	"Jhargram"	~	"Paschim Medinipur",
                                               district ==	"Kalimpong"	~	"Darjeeling",
                                               district ==	"Alipurduar"	~	"Jalpaiguri",
                                               district ==	"North And Middle Andaman"	~	"Andamans",
                                               district ==	"South Andaman"	~	"Andamans",
                                               district ==	"Samba"	~	"Jammu",
                                               district ==	"Reasi"	~	"Udhampur",
                                               district ==	"Ramban"	~	"Doda",
                                               district ==	"Kishtwar"	~	"Doda",
                                               district ==	"Kulgam"	~	"Anantnang",
                                               district ==	"Shupiyan"	~	"Pulwama",
                                               district ==	"Ganderbal"	~	"Srinagar",
                                               district ==	"Bandipore"	~	"Baramulla",
                                               district ==	"South East"	~	"South",
                                               district ==	"Shahdara"	~	"North East",
                                               district == "Ashoknagar" ~ "Guna",
                                               district == "Anuppur" ~ "Shahdol",
                                               district == "Burhanpur" ~ "East Nimar",
                                               district == "Shahid Bhagat Singh Nagar" ~	"Nawanshahr",
                                               TRUE ~ district))

dlhs3 <- dlhs3 %>% filter(!is.na(DLHS3_id))
length(which(dlhs3$district != dlhs3$namefix)) # should be 0. If 0 worked.

#dropping old district names in each dataset
dlhs3 <- dlhs3 %>% select(-c(district))
dlhs4 <- dlhs4 %>% select(-c(district))
nfhsahs <- nfhsahs %>% select(-c(district))
nfhs5 <- nfhs5 %>% select(-c(district))

#binding back into one dataset
#starting with dlhs 3 and dlhs4


dlhs <- full_join(dlhs3, dlhs4, by = "namefix")  # works. You get duplicates because dlhs4 has districts that split + some districts not surveyed in dlhs3

dlhsnfhs4ahs <- full_join(dlhs, nfhsahs, by = "namefix")

dlhsnfhsahs <- full_join(dlhsnfhs4ahs, nfhs5, by = "namefix")

# HARMONIZED

district_harmonized <- dlhsnfhsahs %>% select(-c(n))
#write.csv(district_harmonized, "districts_harmonized.csv")

#creating new id based on name of district
dlhsnfhsahs <- dlhsnfhsahs %>%                                        # Create ID by group
  group_by(namefix) %>%
  mutate(dist_id = cur_group_id())

dlhsnfhsahs$AHS_id <- dlhsnfhsahs$NFHS_AHS_id
dlhsnfhsahs <- dlhsnfhsahs %>% rename(NFHS4_id = NFHS_AHS_id)

district_names <- dlhsnfhsahs %>% select(-c(n, dist_id))

district_names <-  district_names %>%
  pivot_longer(
    cols = ends_with("_id"),
    names_to = "survey",
    values_to = "id",
    values_drop_na = TRUE
  )

district_names <- district_names %>% group_by(namefix) %>%
  mutate(dist_id = cur_group_id())

df <- df %>% mutate(dist_survey = case_when(survey == "DLHS3" ~ "DLHS3_id",
                                           survey == "DLHS4" ~ "DLHS4_id",
                                           survey == "NFHS4" ~ "NFHS4_id",
                                           survey == "AHS" ~ "AHS_id",
                                           survey == "NFHS5" ~ "NFHS5_id",
                                           TRUE ~ survey))

df <- df %>% mutate(id = case_when(survey == "DLHS3" ~ state_dist,
                                   survey == "DLHS4" ~ state_dist,
                                   survey == "NFHS4" ~ district,
                                   survey == "AHS" ~ district,
                                   survey == "NFHS5" ~ district,
                                   TRUE ~ NA_real_))

district_names <- district_names %>% ungroup()
district_names <- district_names %>% select(-c(namefix))
district_names <- district_names %>% rename(dist_survey = survey)

df_district_merge <- left_join(district_names, df,
                               by = c("dist_survey", "id"))

#not working. trying by survey.

dlhsnfhsahs <- dlhsnfhsahs %>% ungroup()

dlhs3_district_names <- dlhsnfhsahs %>% select(c(DLHS3_id, dist_id))
dlhs4_district_names <- dlhsnfhsahs %>% select(c(DLHS4_id, dist_id))
nfhs4_district_names <- dlhsnfhsahs %>% select(c(NFHS4_id, dist_id))
ahs_district_names <- dlhsnfhsahs %>% select(c(AHS_id, dist_id))
nfhs5_district_names <- dlhsnfhsahs %>% select(NFHS5_id, dist_id)

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
