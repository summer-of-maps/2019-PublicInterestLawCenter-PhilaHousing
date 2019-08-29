##############################FIT/UNWARE MAPS#################
####join evictions and open violations
all_violations$opa_account_num <- as.character(all_violations$opa_account_num)
summary_clean2$opa_account_num <- as.character(summary_clean2$parcel_number)

#join together
violations_evictions <- all_violations %>%
  left_join(summary_clean2)
#all_violations, 336k rows, evictions, 52k rows, joined together

#date filing and resolution as dates
violations_evictions$d_filing.x <- as.Date(violations_evictions$d_filing.x)
violations_evictions$caseresolutiondate <- as.Date(violations_evictions$caseresolutiondate)

#filter to only those where the case is still open and the violation comes first
violations_evictions$violation_pre_evict <- ifelse(violations_evictions$caseaddeddate<=violations_evictions$d_filing.x, 1, 0)
violations_evictions$violation_pre_evict2 <- ifelse (violations_evictions$caseresolutiondate<=violations_evictions$d_filing.x, 1,0)
violations_evictions$violation_pre_evict3 <- ifelse(is.na(violations_evictions$violation_pre_evict2), violations_evictions$violation_pre_evict, violations_evictions$violation_pre_evict2)

open_viol_evict <- violations_evictions %>%
  #PULL ONLY ROWS THAT HAVE EVICTIONS AND VIOLATIONS
  filter(violation_pre_evict3==1) %>%
  select(lng, opa_account_num, address, caseaddeddate, caseresolutiondate, 
       violationdescription, casestatus, prioritydesc, lat, violation,
       fitness, unaware, evict, d_filing.x, violation_pre_evict3, year)

#REMOVE DUPLICATES, only unique violations by most recent
open_viol_evict_sf<-open_viol_evict[!duplicated(open_viol_evict$opa_account_num), ]
open_viol_evict_sf <- st_as_sf(open_viol_evict, coords=c('lng','lat'), crs=4326) %>%
  st_transform(2272)

open_viol_evict <- open_viol_evict_sf %>%
  st_join(demos, join=st_within )

unaware_all_viols <- open_viol_evict %>%
  filter(unaware == "t") %>%
  st_as_sf(coords=c('lng', 'lat'), crs=4326) %>%
  st_transform(2272) %>%
  st_join(demos, join=st_within)

fit_hazard_viols <- open_viol_evict %>%
  filter(fitness=="fit") %>%
  filter(prioritydesc !="NON HAZARDOUS") %>%
  st_as_sf(coords=c('lng', 'lat'), crs=4326) %>%
  st_transform(2272) %>%
  st_join(demos, join=st_within)

unaware_all_viols2 <- as.data.frame(unaware_all_viols)
fit_hazard_viols2 <- as.data.frame(fit_hazard_viols)
unaware_all_viols$majority_race<-colnames(unaware_all_viols2)[68:71][apply(unaware_all_viols2[68:71], 1,which.max)]


#Pull only those that are unaware and have viol or are fit and have hazard
#2017 and 2018 separate
unaware_all_viols_2017 <- open_viol_evict %>%
  filter(unaware=="t") %>%
  filter(year==2017) %>%
  st_as_sf(coords=c('lng', 'lat'), crs=4326) %>%
  st_transform(2272)

unaware_all_viols_2018 <- open_viol_evict %>%
  filter(unaware=="t") %>%
  filter(year==2018) %>%
  st_as_sf(coords=c('lng', 'lat'), crs=4326) %>%
  st_transform(2272)

fit_hazard_viols_2017 <- open_viol_evict %>%
  filter(fitness=="fit") %>%
  filter(prioritydesc !="NON HAZARDOUS") %>%
  filter(year==2017) %>%
  st_as_sf(coords=c('lng', 'lat'), crs=4326) %>%
  st_transform(2272)

fit_hazard_viols_2018 <- open_viol_evict %>%
  filter(fitness=="fit") %>%
  filter(prioritydesc !="NON HAZARDOUS") %>%
  filter(year==2018) %>%
  st_as_sf(coords=c('lng', 'lat'), crs=4326) %>%
  st_transform(2272)

##ST_WRITE
#st_write(unaware_all_viols_2017, 'processed_data/unaware_all_viols2017.shp')
#st_write(unaware_all_viols_2018, 'processed_data/unaware_all_viols2018.shp')
#st_write(fit_hazard_viols_2017, 'processed_data/fit_hazard_viols_2017.shp')
#st_write(fit_hazard_viols_2018, 'processed_data/fit_hazard_viols_2018.shp')
