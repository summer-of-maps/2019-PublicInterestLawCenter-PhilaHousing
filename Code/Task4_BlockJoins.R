#####################TASK 3: IDENTIFY SUBSTANDARD HOUSING ######################
#DEMOS ALREADY EXISTS

##############CONVERT JOINS TO SF ####################
#Convert join 5 into an sf
join5_geo<- join5_clean[!is.na(join5_clean$lat.x), ]
join5_geo<-st_as_sf(join5_geo, coords=c("lng.x", "lat.x"), crs=4326) %>%
  st_transform(crs=2272)
#join5_sample<-join5_geo[sample(nrow(join5_geo), 100), ]

#Join join5_clean to demos_blockgroup
#check projection: TRUE!
#st_crs(demos_blockgroup)==st_crs(join5_geo)
join5_demos<-st_join(join5_geo, demos, join=st_within)

#Create identifier for whether IR/Viol/eviction
#Creat lat/lng by joining back
join5_demos2<-left_join(join5_demos, join5_clean, by="opa_account_num")

#remove  extra columns
join5_demos3<-subset(join5_demos2, select=-c(location.x.y,objectid.y,
                                             matching_tax_address.y, inPhillyLL.y,
                                             illegal_rental.y, licensetyp.y,
                                             initialiss.y, mostrecent.y, licensesta.y,
                                             numberofun.y, property_evict_count.y, LL_evict_count.y,
                                             evict.y, id.y, d_filing.x.y, plaintiff.y,
                                             plaintiff_address.y, fitness.y, unaware.y, violationdate.y,
                                             mostrecentinsp.y, status.y, total_viol_count.y, hazard_viol_count.y,
                                             violation.y))

#table(join5_demos$Identifier4.x)
#365k without any substandard indicator
#50k properties with violations
#109k potential illegal rentals
#20k IR/Viols
#5k evictions
#3k evictions/viol
#10k eviction/IR
#5k eviction/ir/viol
#join5_demosONLY<-subset(join5_demos, join5_demos$Identifier4.x!="")

#create majority race, first make dataframe
#join5_demos_df<-as.data.frame(join5_demos3)
#join5_demos3$majority_race<-colnames(join5_demos_df)[46:49][apply(join5_demos_df[46:49], 1,which.max)]

#subset to IRs/viols/evictions
join5_evict<-subset(join5_demos3, join5_demos3$evict.x==1)
join5_IRs<-subset(join5_demos3, join5_demos3$illegal_rental.x==1)
join5_viols<-subset(join5_demos3, join5_demos3$violation.x==1)
join5_demos3$violation.x[is.na(join5_demos3$violation.x)] <- 0
join5_demos3$illegal_rental.x[is.na(join5_demos3$illegal_rental.x)] <- 0
join5_demos3$violation.x[is.na(join5_demos3$violation.x)] <- 0

join5_none <- join5_demos3 %>%
  filter(violation.x == 0) %>%
  filter(illegal_rental.x ==0) %>%
  filter (evict.x == 0)

#ATTACH MAJORITY RACE TO EACH POINT
#make dataframe and subset just race
join5_evict2<-as.data.frame(join5_evict) %>%
  select(-geometry)
join5_IRs2<-as.data.frame(join5_IRs) %>%
  select(-geometry)
join5_viols2<-as.data.frame(join5_viols) %>%
  select(-geometry)
join5_none2<-as.data.frame(join5_none) %>%
  select(-geometry)

##join5_evict$majority_race<-colnames(join5_evict2)[46:49][apply(join5_evict2[46:49], 1,which.max)]
#join5_IRs$majority_race<-colnames(join5_IRs2)[46:491][apply(join5_IRs2[46:49], 1,which.max)]
#join5_viols$majority_race<-colnames(join5_viols2)[46:49][apply(join5_viols2[46:49], 1,which.max)]
#join5_none$majority_race<-colnames(join5_none2)[46:49][apply(join5_none2[46:49], 1,which.max)]

#join5_complete <- rbind(join5_evict, join5_IRs, join5_viols, join5_complete) %>%
#  mutate(duplicated = duplicated(opa_account_num)) %>%
#  filter(duplicated == FALSE)

#Summary Statistics
#mean(join5_viols$total_viol_count.x)
#mean(join5_evict$property_evict_count.x)
#quantile(join5_viols$total_viol_count.x, probs=seq(0,1,0.25), na.rm=F)
#quantile(join5_evict$property_evict_count.x, probs=seq(0,1,0.25), na.rm=T)

#############AGGREGATE BY BLOCK GROUP ###################
#read in points for everything
heat_violations_clean_sf <- heat_violations[!is.na(heat_violations$lng), ]
heat_violations_clean_sf <- st_as_sf(heat_violations_clean_sf, coords=c('lng','lat'), crs=4326) %>%
  st_transform(2272)

violations_clean_sf <- violations_clean[!is.na(violations_clean$lng), ]
violations_clean_sf <- st_as_sf(violations_clean_sf, coords=c('lng','lat'), crs=4326) %>%
  st_transform(2272)

opa_clean$opa_account_num<-as.character(opa_clean$parcel_number)
summary_clean2$opa_account_num<-as.character(summary_clean2$parcel_number)
OPA_evict<-left_join(summary_clean2,opa_clean, by=c("opa_account_num"))
OPA_evict<- OPA_evict[!is.na(OPA_evict$latitude.x), ]
OPA_evict_sf <- st_as_sf(OPA_evict, coords=c("longitude.x", 'latitude.x'), crs=4326) %>%
  st_transform(2272)

#SPATIAL JOIN TO BLOCK GROUPS AND AGGREGATE
#HEAT VIOLATIONS
heat_violations_count <- heat_violations_clean_sf %>%
  st_join(demos, join=st_within) %>%
  group_by(Block_Gr_1) %>%
  summarize (heat_viols = n()) %>%
  as.data.frame() %>%
  select(-geometry)

#Hazardous Violations
haz_violations_count <- violations_clean_sf %>%
  st_join(demos, join=st_within) %>%
  group_by(Block_Gr_1) %>%
  summarize (haz_viols = n()) %>%
  as.data.frame() %>%
  select(-geometry)

#Evictions
eviction_block_count <- OPA_evict_sf %>%
  st_join(demos, join=st_within) %>%
  drop_na(Block_Gr_1) %>%
  group_by(Block_Gr_1) %>%
  summarize (evict_count = n()) %>%
  as.data.frame() %>%
  select(-geometry)

#Potential Illegal Rentals
IR_block_count <- join5_IRs %>%
#  st_join(demos, join=st_within) %>%
  drop_na(Block_Gr_1) %>%
  group_by(Block_Gr_1) %>%
  summarize (IR_count = n()) %>%
  as.data.frame() %>%
  select(-geometry)

#LLCS
LLC_block_count <- LLC_sf %>%
  st_join(demos, join=st_within) %>%
  group_by(Block_Gr_1) %>%
  summarize (LLC_count = n()) %>%
  as.data.frame() %>%
  select(-geometry)

#Bind together
block_counts <- demos %>%
  left_join(heat_violations_count) %>%
  left_join(haz_violations_count) %>%
  left_join(eviction_block_count) %>%
  left_join(IR_block_count) %>%
  left_join(LLC_block_count)

#write.csv(block_counts, 'processed_data/block_counts.csv')
