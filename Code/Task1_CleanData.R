
##################SET UP############################
library(data.table)
library(ggplot2)
library(ggmap)
library(sf)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyverse)
library(tidyr)

##################LOAD DATA############################
#OPA
opa <- fread('Data/opa_properties_public.csv', header=TRUE, sep=",", colClasses='numeric')

#L&I licenses
licenses <- st_read('Data/li_business_licenses.shp') %>%
  st_transform(crs=2272)

#L&I violations
violations <- read.csv('Data/li_violations.csv')

#vacant property indicators
vacant <- st_read('Data/Vacant_Indicators_Bldg.shp') %>%
  st_transform(crs=2272)

#Evictions
summary <- read.csv('Data/summary-table.txt')
parcels <- read.csv('Data/2019_07_01_eviction_parcel.csv')

##################TASK 1: CLEAN DATA############################
#OPA CLEANING
opa_clean <- opa %>% 
  select(parcel_number,lng, lat, location, objectid, zip_code, mailing_address_1,
         mailing_address_2, mailing_care_of, mailing_city_state,
         mailing_street, mailing_zip, owner_1, owner_2,
         category_code_description,building_code_description) %>%
  filter(category_code_description!="Commercial") %>%
  #CREATE BINARY FACTOR VARIABLES FOR VACANCY/IN OUT OF PHILLY/MATCHING TAX ADDRESS
  mutate(mailing_street2 = ifelse(mailing_street=="", location, mailing_street)) %>%
  mutate(matching_tax_address = ifelse(location == mailing_street2, 1,0)) %>%
  mutate(vacant = ifelse(grepl("VACAN", building_code_description), 1,0)) %>%
  #create binary column for LL in or out of Philly
  mutate(mailing_city_state2 = ifelse(mailing_city_state=="", "PHILADELPHIA", mailing_city_state)) %>%
  mutate(inPhillyLL = ifelse(grepl("PHIL", mailing_city_state2), 1,0)) %>% 
  mutate(duplicated = duplicated(parcel_number)) %>%
  filter(duplicated=="FALSE")

#table(opa_clean$matching_tax_address)
#216k nonmatching, 365k matching (assumed to be matching if mailing is blank)
#meaning, 216k non owner occuppied, 365k owneroccuppied
#table(opa_clean$inPhillyLL)
#79352 not in Philly, 501978 in Philly

#Check class
opa_clean$matching_tax_address<-as.factor(opa_clean$matching_tax_address)
opa_clean$vacant<-as.factor(opa_clean$vacant)
opa_clean$inPhillyLL<-as.factor(opa_clean$inPhillyLL)

#Check NAs/Zeros/incomplete rows
#sum(is.na(opa_clean$location))

#subset OPA to non-vacant
opa_notvacant <- opa_clean %>% filter(vacant==0)

#pull out the top mailing addresses
top_mailing_address <- opa_clean %>% 
  group_by(mailing_street) %>%
  summarize(mailed_count = n())

#Compile those that are multiple property owners
multiple_property_owners <- top_mailing_address %>%
  filter(mailed_count > 1 & mailed_count < 10000)

#join back to full dataset
multiple_property_joined <- left_join(opa_clean, top_mailing_address, by = 'mailing_street') %>%
  mutate(mailed_count = ifelse(mailed_count == 361900, 1, mailed_count))

opa_clean_top_landlords <- multiple_property_joined %>% filter(mailed_count > 10)

#######VIOLATIONS CLEANING
violations_clean <- violations %>%
  select(-objectid, -censustract, -apfailkey,-violationtype, -casegroup, 
         -geocode_x,-geocode_y,-the_geom,-addresskey,-casenumber,
         -the_geom_webmercator)

#convert to right class
violations_clean$address <- as.character(violations_clean$address)
violations_clean$caseaddeddate <- as.Date(violations_clean$caseaddeddate)
violations_clean$caseresolutiondate <- as.Date(violations_clean$caseresolutiondate)
violations_clean$violationdate <- as.Date(violations_clean$violationdate)
violations_clean$violation <- 1

#limit violations to 2016/17/18
violations_clean$year <- substring(violations_clean$caseaddeddate,1,4)
violations_clean$year <- as.numeric(violations_clean$year)
all_violations <- violations_clean%>%
  filter(year == 2016| year == 2017|year ==2018)

violations_clean <- violations_clean%>%
  filter(year == 2016| year == 2017|year ==2018)%>%
  filter(prioritydesc!="NON HAZARDOUS")

#Make open/closed
sum(is.na(violations_clean$casestatus))
violations_open <- violations_clean %>%
  filter(casestatus =="OPEN")
violations_close <- violations_clean %>%
  filter(casestatus =="CLOSED")
violations_stopwork<-violations_clean %>%
  filter(casestatus =="STOP WORK")

#count up open/closed/stop work
violations_open_count<-violations_open%>% 
  group_by(opa_account_num)%>%
  summarize(open_viol_count=n())

violations_close_count<-violations_close%>% 
  group_by(opa_account_num)%>%
  summarize(close_viol_count=n())

violations_SW_count<-violations_stopwork%>% 
  group_by(opa_account_num)%>%
  summarize(SW_viol_count=n())

#subset to those not classified as not non-hazardous
violations_hazardous<-subset(violations_clean, violations_clean$prioritydesc !="NON HAZARDOUS")

#Only active Violations
violations_open<-subset(violations_clean, violations_clean$casestatus =="OPEN")

#aggregate by count of kinds of violations
violations_by_address<-violations %>% 
  group_by(opa_account_num) %>%
  summarize(total_viol_count=n())

hazard_viol_by_address <- violations_hazardous %>% 
  group_by(opa_account_num) %>%
  summarize(hazard_viol_count=n())

#LEFT JOIN all together
violation_counts <- left_join(violations_by_address, hazard_viol_by_address, by="opa_account_num")

#JOIN BACK
violations_final <- left_join(violations_clean, violation_counts, by="opa_account_num")

#REMOVE DUPLICATES, only unique violations by most recent
violations_final <- violations_final[order(violations_final$opa_account_num, violations_final$caseaddeddate, decreasing=TRUE),]
violations_final <- violations_final[!duplicated(violations_final$opa_account_num),]
#goes from 335578 rows to 82251 rows

#sort according to heat violations 
violations_clean$heat_violation<-ifelse(grepl("HEAT", violations_clean$violationdescription), 1,0)
heat_violations <- violations_clean %>%
  filter (heat_violation ==1)

#LICENSE CLEANING
#clean columns
licenses_clean <- licenses %>%
  select(-licensenum,-revenuecod,-censustrac, -objectid, 
         -councildis,-eclipse_ad)

#CUT DOWN TO 2016-2018
licenses_clean$year <- substring(licenses_clean$initialiss,1,4)
licenses_clean <- licenses_clean %>%
  filter(year == 2016|2017|2018)%>%
  mutate(ActiveLicense = ifelse(licenses_clean$licensesta=="Active", 1,0))

#fill in 0s for NAs
licenses_clean$ActiveLicense[is.na(licenses_clean$ActiveLicense)] <- 0
licenses_clean$license <- 1

#filter down to just rental licenses
#subset RENTALS / VACANT RESIDENTIALS / HIGH RISE
license_rentals<-licenses_clean %>%
  filter(licensetyp == "Rental"| licenses_clean$licensetyp == 'High Rise')

active_license_rentals<-license_rentals %>%
  filter(ActiveLicense == 1)
inactive_license_rentals<-license_rentals %>%
  filter(ActiveLicense == 0)

#REMOVE DUPLICATES, only unique licenses
licenses_clean <- licenses_clean[rev(order(as.Date(licenses_clean$mostrecent))),]
licenses_clean <- licenses_clean[!duplicated(licenses_clean$opaaccount),]
#goes from 52038 rows to 43752 rows 

#Vacant Cleaning
vacant$OPA_ID <- as.numeric(vacant$OPA_ID)
vacant$ADDRESS <- as.character(vacant$ADDRESS)
vacant$vacant <- 1
vacant$vacant <- as.factor(vacant$vacant)

#DOCKET CLEANING
#CLEAN SUMMARY AND JOIN TO PARCELS
names(parcels)[1] <- "id"
summary_clean_parcel <- summary %>%
  select(defendant_address,id, d_filing, month, year, 
         plaintiff, plaintiff_address, fitness, unaware, latitude, 
         longitude)%>%
  filter(year == 2016| year ==2017| year==2018) %>%
  left_join(parcels, by="id") #%>%
select(id, d_filing.x, month,year,
       plaintiff, plaintiff_address, fitness,
       unaware, latitude.x, longitude.x, parcel_number)

summary_clean2 <- summary_clean_parcel[!is.na(summary_clean_parcel$parcel_number), ]
summary_clean2$evict <- 1

#group by address and find eviction sums for each address
#make address and plaintiff character
summary_clean2$plaintiff<-as.character(summary_clean2$plaintiff)

evict_count <- summary_clean2 %>%
  group_by(parcel_number) %>%
  summarize(property_evict_count=n())

LL_count <- summary_clean2 %>%
  group_by(plaintiff) %>%
  summarize(LL_evict_count=n())

#join back to clean evictions
count_join <- left_join(summary_clean2,evict_count, by='parcel_number')
count_join2 <- left_join(count_join, LL_count, by='plaintiff')

#remove duplicates, remove rows according to most recent
evictions_unique <-
  count_join2 %>%
  group_by(parcel_number) %>%
  slice(which.max(d_filing.x))

#CLEAN REMOVE EMPTY LAT/LNG AND NA ADDRESS
evictions_unique_clean<- evictions_unique[!is.na(evictions_unique$longitude.x), ]
evictions_unique_clean<-evictions_unique_clean[-c(1),]

#############################TASK 5: FIND ILLEGAL RENTALS ###############################
##subset opa, to not vacant and not owner occupied
opa_notvacant_notOO<-opa_notvacant %>%
  filter(matching_tax_address == 0)

#join vacant indicators to opa by opa id
opa_notvacant_notOO$ADDRESS <- as.character(opa_notvacant_notOO$location)
opa_vacant_join <- left_join(opa_notvacant_notOO, vacant, by="ADDRESS")

#set vacant NAs to 0, because they did not match with a vacant parcel
opa_vacant_join$vacant.y <- as.numeric(opa_vacant_join$vacant.y)
opa_vacant_join$vacant.y[is.na(opa_vacant_join$vacant.y)] <- 0
#table(opa_vacant_join$vacant.y)

#clean to exclude vacant homes
opa_vacant_join2 <- opa_vacant_join %>%
  filter(opa_vacant_join$vacant.y != 1)

#join to rentals
opa_vacant_license <- left_join(opa_vacant_join2, license_rentals, by=c('location'= 'street_add'))
opa_vacant_license$ActiveLicense[is.na(opa_vacant_license$ActiveLicense)] <- 0
#table(opa_vacant_license$ActiveLicense)

#clean to include only those without active licenses
#illegal_rentals dataset has properties that are: not vacant, not owner occupied, and have no rental license
#77k properties
illegal_rentals <- opa_vacant_license %>%
  filter (ActiveLicense == 0)

illegal_rentals_clean <- illegal_rentals[,-c(19:50)]
illegal_rentals_clean$illegal_rental <- 1
illegal_rentals_only<-subset(illegal_rentals_clean, select=c(objectid, location))
illegal_rentals_only$illegal_rental<-1
illegal_rentals_only$duplicated <- duplicated(illegal_rentals_only$objectid)
illegal_rentals_only<-subset(illegal_rentals_only, illegal_rentals_only$duplicated==FALSE)
