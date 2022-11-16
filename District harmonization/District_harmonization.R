setwd("./District harmonization")

# Fixing districts to match -----------------------------------------------

# To create this list the value labels for DLHS3,  NFHS4, and NFHS5 were copied from STATA and put into a word document.
#DLHS4 and AHS data did not have district labels. They were found elsewhere. The method for their creation is as follows:
#Two separate district lists were used. The first, downloaded from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UVTMR5
# was the DLHS4_AHS district names file. This is also in OneDrive as OneDrive-McGillUniversity/CIHR RSBY India - PROSPERED_Group/data/Linking code and CSVs/dataverse code
# The second is in the "Variables codebook.xlsx" assembled by former Nandi RA. This was used to supplement the list of following state_districts in AHS 
#They were not listed in the downloaded dataverse file. 502, 503, 1010, 1012, 1015, 1018, 2004, 2018
#Found here: "OneDrive-McGillUniversity/CIHR RSBY India - PROSPERED_Group/data/NFHS and DLHS data/data and documentation/Variables codebook.xlsx


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

names$DLHS4_AHS <- str_to_title(names$DLHS4_AHS)
names$DLHS3 <- str_to_title(names$DLHS3)
names$NFHS4 <- str_to_title(names$NFHS4)
names$NFHS5 <- str_to_title(names$NFHS5)



names$NFHS4 <- trim(names$NFHS4)
names$DLHS3 <- trim(names$DLHS3)
names$DLHS4_AHS <- trim(names$DLHS4_AHS)
names$NFHS5 <- trim(names$NFHS5)





#now making separate dataframes so can merge them based on district name

dlhsahs <- names %>% select(c(DLHS3, DLHS3_id, DLHS4_AHS, DLHS4_AHS_id))
nfhs4 <- names %>% select(c(NFHS4, NFHS_id))
nfhs5 <- names %>% select(c(NFHS5, NFHS5_id))

dlhs4ahs <- dlhsahs %>% select(c(DLHS4_AHS, DLHS4_AHS_id))
dlhs3 <- dlhsahs %>% select(c(DLHS3, DLHS3_id))

dlhs3 <- dlhs3 %>% rename(district = DLHS3)
dlhs4ahs <- dlhs4ahs %>% rename(district = DLHS4_AHS)
nfhs4 <- nfhs4 %>% rename(district = NFHS4)
nfhs5 <- nfhs5 %>% rename(district = NFHS5)

dlhs3 <- dlhs3 %>% filter(!is.na(DLHS3_id))
dlhs4ahs <- dlhs4ahs %>% filter(!is.na(DLHS4_AHS_id))

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

nfhs4 <- nfhs4 %>% mutate(namefix = case_when(district ==	"Longding"	~	"Tirap",
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

nfhs4 <- nfhs4 %>% filter(!is.na(nfhs4$NFHS_id))

dlhs4ahs <- dlhs4ahs %>%  mutate(namefix = case_when(district ==	"Longding"	~	"Tirap",
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

dlhs4ahs <- dlhs4ahs %>% filter(!is.na(DLHS4_AHS_id))

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
dlhs4ahs <- dlhs4ahs %>% select(-c(district))
nfhs4 <- nfhs4 %>% select(-c(district))
nfhs5 <- nfhs5 %>% select(-c(district))

#binding back into one dataset
#starting with dlhs 3 and dlhs4


dlhsahs <- full_join(dlhs3, dlhs4ahs, by = "namefix")  # works. You get duplicates because dlhs4 has districts that split + some districts not surveyed in dlhs3

dlhsnfhs4ahs <- full_join(dlhsahs, nfhs4, by = "namefix")

dlhsnfhsahs <- full_join(dlhsnfhs4ahs, nfhs5, by = "namefix")

dlhsnfhsahs <- dlhsnfhsahs %>% group_by(namefix) %>% add_count(namefix)

# HARMONIZED

district_harmonized <- dlhsnfhsahs %>% select(-c(n))
#write.csv(district_harmonized, "districts_harmonized.csv")

#creating new id based on name of district
dlhsnfhsahs <- dlhsnfhsahs %>%                                        # Create ID by group
  group_by(namefix) %>%
  mutate(dist_id = cur_group_id())

dlhsnfhsahs$AHS_id <- dlhsnfhsahs$DLHS4_AHS_id
dlhsnfhsahs <- dlhsnfhsahs %>% rename(NFHS4_id = NFHS_id, DLHS4_id = DLHS4_AHS_id)

#saving new harmonized IDs
#write.csv(dlhsnfhsahs,"districts_harmonized.csv")

dlhsnfhsahs <- read.csv("districts_harmonized.csv")

dlhsnfhsahs <- dlhsnfhsahs %>% ungroup()

dlhs3_district_names <- dlhsnfhsahs %>% select(c(DLHS3_id, dist_id))
dlhs4_district_names <- dlhsnfhsahs %>% select(c(DLHS4_id, dist_id))
nfhs4_district_names <- dlhsnfhsahs %>% select(c(NFHS4_id, dist_id))
ahs_district_names <- dlhsnfhsahs %>% select(c(AHS_id, dist_id))
nfhs5_district_names <- dlhsnfhsahs %>% select(NFHS5_id, dist_id)