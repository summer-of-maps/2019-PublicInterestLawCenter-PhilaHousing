######################TASK 4: LANDLORD DATABASE ##########################
#CLEAN OWNER NAMES IN OPA
opa_clean2 <- opa_clean %>% 
  filter (matching_tax_address==0) %>%
  mutate(clean_mailing = tools::toTitleCase(tolower(mailing_street))) %>%
  #but letters that appear alone aren't capitalized
  mutate(clean_mailing = str_replace_all(clean_mailing,
                                         regex("\\b[a-z]{1}\\b"),
                                         replacement= function(x){
                                           toupper(x)
                                         })) %>%
  #Street suffixes don't have periods after them
  mutate(clean_mailing = str_replace_all(clean_mailing,
                                         regex("\\b(?:Rt|Dr|St|Pk|Rd|La|Ave|Av|Sta)\\b"),
                                         replacement= function(x){
                                           paste0(x,".")
                                         })) %>%
  #directions aren't capitalized
  mutate(clean_mailing = str_replace_all(clean_mailing,
                                         regex("\\b(?:Nw|Sw|Se|Ne)\\b"),
                                         replacement= function(x){
                                           toupper(x)
                                         }))

opa_clean2$clean_mailing <- gsub("St.", "St", opa_clean2$clean_mailing)
opa_clean2$clean_mailing <- gsub("Street", "St", opa_clean2$clean_mailing)
opa_clean2$clean_mailing <- gsub("Ave.", "Ave", opa_clean2$clean_mailing)
opa_clean2$clean_mailing <- gsub("Avenue", "Ave", opa_clean2$clean_mailing)
opa_clean2$clean_mailing <- gsub("Ln.", "Ln", opa_clean2$clean_mailing)
opa_clean2$clean_mailing <- gsub("Lane", "Ln", opa_clean2$clean_mailing)
opa_clean2$clean_mailing <- gsub("Blvd.", "Blvd", opa_clean2$clean_mailing)
opa_clean2$clean_mailing <- gsub("Boulevard", "Blvd", opa_clean2$clean_mailing)
opa_clean2$clean_mailing <- gsub("Dr.", "Dr", opa_clean2$clean_mailing)
opa_clean2$clean_mailing <- gsub("Drive", "Dr", opa_clean2$clean_mailing)
opa_clean2$clean_mailing <- gsub("Crt.", "Ct", opa_clean2$clean_mailing)
opa_clean2$clean_mailing <- gsub("Court", "Ct", opa_clean2$clean_mailing)
opa_clean2$clean_mailing <- gsub("Rd.", "Rd", opa_clean2$clean_mailing)
opa_clean2$clean_mailing <- gsub("Road", "Rd", opa_clean2$clean_mailing)

#add together OPA, illegal rentals, already done OPA_IR
#Join 1: IR and OPA
#class(illegal_rentals_clean$objectid)
#names(OPA_IR)
#sum(is.na(OPA_IR$illegal_rental))

#OPA hazardous viols/viols and evict
violations_clean$hazard_viol<-1
all_violations$viol<-1
opa_clean2$opa_account_num<-as.character(opa_clean2$parcel_number)
violations_clean$opa_account_num<-as.character(violations_clean$opa_account_num)
all_violations$opa_account_num<-as.character(all_violations$opa_account_num)
summary_clean2$opa_account_num<-as.character(summary_clean2$parcel_number)

OPA_IR <- left_join(illegal_rentals_only, opa_clean2, by=c("objectid"))
OPA_viol<-left_join(all_violations, opa_clean2, by=c("opa_account_num"))
OPA_hazard_viol<-left_join(violations_clean, opa_clean2, by=c("opa_account_num"))
OPA_evict<-left_join(summary_clean2,opa_clean2, by=c("opa_account_num"))

#ST_JOIN
#LOAD IN BLOCK GROUP
demos_blockgroup<-st_read('Data/block_group_demos.shp')%>%
  st_transform(crs=2272)

#remove rows with population
demos<-subset(demos_blockgroup, demos_blockgroup$TotalPop>=1)
#remove not latino
demos<-subset(demos, select=-c(NotLatino))
#create percentage columns
demos$pctWhite<-((demos$White/demos$TotalPop)*100)
demos$pctBlack<-((demos$Black/demos$TotalPop)*100)
demos$pctAsian<-((demos$Asian/demos$TotalPop)*100)
demos$pctLatino<-((demos$Latino/demos$TotalPop)*100)

#create majority 
demos_df<-as.data.frame(demos)
demos$majority_race<-colnames(demos_df)[19:22][apply(demos_df[19:22], 1,which.max)]

#create binary for white/non-white
demos$minority_majority<-ifelse(demos$majority_race!="White", 1,0)

#JOIN TO OPA
OPA_demos_geo<-opa_clean2[!is.na(opa_clean2$lat), ]
OPA_sf<-st_as_sf(OPA_demos_geo, coords=c('lng','lat'), crs=4326) %>%
  st_transform(2272)
OPA_demos<-st_join(OPA_sf, demos, join=st_within)

OPA_IR_geo<- OPA_IR[!is.na(OPA_IR$lat), ]
OPA_IR_sf<-st_as_sf(OPA_IR_geo, coords=c('lng', 'lat'), crs=4326) %>%
  st_transform(2272)
OPA_IR_demos<-st_join(OPA_IR_sf, demos, join=st_within)

OPA_hazard_viol_geo<- OPA_hazard_viol[!is.na(OPA_hazard_viol$lat.x), ]
OPA_hazard_viol_sf<-st_as_sf(OPA_hazard_viol_geo, coords=c('lng.x', 'lat.x'), crs=4326) %>%
  st_transform(2272)
OPA_haz_viol_demos<-st_join(OPA_hazard_viol_sf, demos, join=st_within)

OPA_viol_geo<- OPA_viol[!is.na(OPA_viol$lat.x), ]
OPA_viol_sf<-st_as_sf(OPA_viol_geo, coords=c('lng.x', 'lat.x'), crs=4326) %>%
  st_transform(2272)
OPA_viol_demos<-st_join(OPA_viol_sf, demos, join=st_within)

OPA_evict_geo<- OPA_evict[!is.na(OPA_evict$latitude.x), ]
OPA_evict_sf<-st_as_sf(OPA_evict_geo, coords=c('longitude.x', 'latitude.x'), crs=4326) %>%
  st_transform(2272)
OPA_evict_demos<-st_join(OPA_evict_sf, demos, join=st_within)

#subset to only minority communities and aggregate by owner
OPA_minority<-OPA_demos %>%
  filter(minority_majority==1)
IR_minority<-OPA_IR_demos %>%
  filter(minority_majority==1)
haz_viol_minority<-OPA_haz_viol_demos %>%
  filter(minority_majority==1)
viol_minority<-OPA_viol_demos %>%
  filter(minority_majority==1)
evict_minority<-OPA_evict_demos %>%
  filter(minority_majority==1)

minority_totals<-OPA_minority %>%
  group_by(clean_mailing) %>%
  summarize(minority_props=n()) %>%
  as.data.frame() %>%
  select(-geometry)

minority_IR<-IR_minority %>% 
  group_by(clean_mailing) %>%
  summarize(IR_minority=n()) %>%
  as.data.frame() %>%
  select(-geometry)

minority_hazard_viol<-haz_viol_minority %>%
  group_by(clean_mailing) %>%
  summarize(haz_viol_minority=n()) %>%
  as.data.frame() %>%
  select(-geometry)

minority_viol<-viol_minority %>%
  group_by(clean_mailing) %>%
  summarize(viol_minority=n()) %>%
  as.data.frame() %>%
  select(-geometry)

minority_evict<-evict_minority %>%
  group_by(clean_mailing) %>%
  summarize(evict_minority=n()) %>%
  as.data.frame() %>%
  select(-geometry)

#AGGREGATE BY LANDLORD
#first, find the total number of properties for each owner, the denominator
LL_property_totals<-OPA_demos %>%
  group_by(clean_mailing) %>%
  summarize(total_props=n()) %>%
  as.data.frame() %>%
  select(-geometry)

IR_by_LL<-OPA_IR_demos %>% 
  group_by(clean_mailing) %>%
  summarize(IR_count=n()) %>%
  as.data.frame() %>%
  select(-geometry)

viol_by_LL<-OPA_viol_demos %>%
  group_by(clean_mailing) %>%
  summarize(viol_count=n()) %>%
  as.data.frame() %>%
  select(-geometry)

hazard_viol_by_LL<-OPA_haz_viol_demos %>%
  group_by(clean_mailing) %>%
  summarize(haz_viol_count=n()) %>%
  as.data.frame() %>%
  select(-geometry)

evict_by_LL<-OPA_evict_demos %>% 
  group_by(clean_mailing) %>%
  summarize(evict_count=n()) %>%
  as.data.frame() %>%
  select(-geometry)

#MERGE ALL COLUMNS TOGETHER BY OWNER NAME
#CONVERT EVERYTHING TO A DF
LL_db <- LL_property_totals %>%
  left_join(IR_by_LL) %>%
  left_join(viol_by_LL) %>%
  left_join(hazard_viol_by_LL) %>%
  left_join(evict_by_LL) %>%
  left_join(minority_totals) %>%
  left_join(minority_IR) %>%
  left_join(minority_viol)%>%
  left_join(minority_hazard_viol) %>%
  left_join(minority_evict)

#if minority is NA, set equal to 0
LL_db$ActiveLicense[is.na(LL_db$ActiveLicense)] <- 0
LL_db$IR_count[is.na(LL_db$IR_count)] <- 0
LL_db$viol_count[is.na(LL_db$viol_count)] <- 0
LL_db$evict_count[is.na(LL_db$evict_count)] <- 0
LL_db$minority_props[is.na(LL_db$minority_props)] <- 0
LL_db$viol_minority[is.na(LL_db$viol_minority)] <- 0
LL_db$IR_minority[is.na(LL_db$IR_minority)] <- 0
LL_db$evict_minority[is.na(LL_db$evict_minority)] <- 0

#create percentage columns
LL_db$pct_Minority<-((LL_db$minority_props/LL_db$total_props)*100)
LL_db$pct_IR_Minority<-((LL_db$IR_minority/LL_db$IR_count)*100)
LL_db$pct_Haz_Viol_Minority<-((LL_db$haz_viol_minority/LL_db$haz_viol_count)*100)
LL_db$pct_Viol_Minority<-((LL_db$viol_minority/LL_db$viol_count)*100)
LL_db$pct_Evict_Minority<-((LL_db$evict_minority/LL_db$evict_count)*100)

#set NA == 0
LL_db$pct_Minority[is.na(LL_db$pct_Minority)] <- 0
LL_db$pct_IR_Minority[is.na(LL_db$pct_IR_Minority)] <- 0
LL_db$pct_Viol_Minority[is.na(LL_db$pct_Viol_Minority)] <- 0
LL_db$pct_Haz_Viol_Minority[is.na(LL_db$pct_Haz_Viol_Minority)] <- 0
LL_db$pct_Evict_Minority[is.na(LL_db$pct_Evict_Minority)] <- 0

#write.csv(LL_db, "Landlord_database.csv")
#Link to LL names
#read in names csv
LL_names<-read.csv('Data/LL_address.csv')
names(LL_names)[1]<-"Street"

#clean street so that it formatted the same way as mailing_address
LL_names <- LL_names %>% 
  mutate(clean_mailing = tools::toTitleCase(tolower(Street))) %>%
  #but letters that appear alone aren't capitalized
  mutate(clean_mailing = str_replace_all(clean_mailing,
                                         regex("\\b[a-z]{1}\\b"),
                                         replacement= function(x){
                                           toupper(x)
                                         })) %>%
  #Street suffixes don't have periods after them
  mutate(clean_mailing = str_replace_all(clean_mailing,
                                         regex("\\b(?:Rt|Dr|St|Pk|Rd|La|Ave|Av|Sta)\\b"),
                                         replacement= function(x){
                                           paste0(x,".")
                                         })) %>%
  #directions aren't capitalized
  mutate(clean_mailing = str_replace_all(clean_mailing,
                                         regex("\\b(?:Nw|Sw|Se|Ne)\\b"),
                                         replacement= function(x){
                                           toupper(x)
                                         }))
LL_names$clean_mailing <- gsub("St.", "St", LL_names$clean_mailing)
LL_names$clean_mailing <- gsub("Street", "St", LL_names$clean_mailing)
LL_names$clean_mailing <- gsub("Ave.", "Ave", LL_names$clean_mailing)
LL_names$clean_mailing <- gsub("Avenue", "Ave", LL_names$clean_mailing)
LL_names$clean_mailing <- gsub("Ln.", "Ln", LL_names$clean_mailing)
LL_names$clean_mailing <- gsub("Lane", "Ln", LL_names$clean_mailing)
LL_names$clean_mailing <- gsub("Blvd.", "Blvd", LL_names$clean_mailing)
LL_names$clean_mailing <- gsub("Boulevard", "Blvd", LL_names$clean_mailing)
LL_names$clean_mailing <- gsub("Dr.", "Dr", LL_names$clean_mailing)
LL_names$clean_mailing <- gsub("Drive", "Dr", LL_names$clean_mailing)

#JOIN BACK
LL_db_names<-LL_db %>%
  left_join(LL_names) %>%
  select(-Zip, -Street)

#reorder columns
names(LL_db_names)
LL_db_names2 <-LL_db_names[,c(1,17,18,2:16)]

#fill in 0s for NAs
LL_db_names2$ActiveLicense[is.na(LL_db_names2$ActiveLicense)] <- NA
LL_db_names2$haz_viol_count[is.na(LL_db_names2$haz_viol_count)] <- 0

#write.csv(LL_db_names2, 'processed_data/Landlord_database.csv')
# % of property in communities of color
# % of violations in communities of color
# % of evictions in communities of color
# % of potential illegal rentals in communities of color
# # of violations
# # of potential illegal rentals
# # of evictions

#Find LLCS
LL_db_opa <- opa_clean2 %>%
  #left join back to OPA,
  left_join(LL_db) %>%
  filter (total_props >= 1) %>%
  mutate(LLC = ifelse(grepl(" LL", owner_1), 1,
                      ifelse(grepl(" CORP", owner_1), 1,
                             ifelse(grepl(" LLC", owner_1), 1,
                                    ifelse(grepl(" INVESTMENT", owner_1), 1,
                                           ifelse(grepl(" PARTNER", owner_1), 1,
                                                  ifelse(grepl(" LP", owner_1), 1,
                                                         ifelse(grepl(" L C", owner_1), 1,
                                                                ifelse(grepl(" L L", owner_1), 1,
                                                                       ifelse(grepl(" L P", owner_1), 1,
                                                                              ifelse(grepl(" ASSOC", owner_1), 1,
                                                                                     ifelse(grepl(" PROPERT", owner_1), 1,
                                                                                            ifelse(grepl(" DEVELOP", owner_1), 1,
                                                                                                   ifelse(grepl(" TRUST", owner_1), 1,
                                                                                                          ifelse(grepl(" BANK", owner_1), 1,
                                                                                                                 ifelse(grepl(" INC", owner_1), 1,
                                                                                                                        ifelse(grepl("REAL ESTATE", owner_1), 1,
                                                                                                                               ifelse(grepl(" GROUP", owner_1), 1,
                                                                                                                                      ifelse(grepl("LIMITED", owner_1), 1,
                                                                                                                                             ifelse(grepl(" CORP", owner_1), 1,
                                                                                                                                                    ifelse(grepl(" L P", owner_1), 1,0
                                                                                                                                                    )))))))))))))))))))))


#SF to spatial join to demos
LL_db_opa_sf<- LL_db_opa[!is.na(LL_db_opa$lat), ]
LL_db_opa_sf <-   LL_db_opa_sf %>%
  st_as_sf(coords=c('lng', 'lat'), crs=4326) %>%
  st_transform(2272)

LLC_sf <- LL_db_opa_sf %>%
  filter(LLC ==1)

