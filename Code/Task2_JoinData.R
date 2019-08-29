##################TASK 2: WRANGLE AND JOIN DATA############################
#FIND: non-vacant, OPA with different addresses, that have no rental license
#join licenses, violations, opa, docket, IRs

#Join 1: IR and OPA
OPA_IR <- left_join(opa_clean, illegal_rentals_only, by=c("objectid"))
#sum(is.na(OPA_IR$illegal_rental))
#full join by objectid

#Join 2: License to OPA
OPA_IR$opa_account_num <- as.character(OPA_IR$parcel_number)
licenses_clean$opa_account_num <- as.character(licenses_clean$opaaccount)
opa_license <- left_join(OPA_IR, licenses_clean, by=c('opa_account_num'))
#sum(is.na(opa_license$license))
#567658-530966
#36692/43752 #% of licenses joined

#Join 3: Vacant to License/OPA
opa_license$Address <- as.character(opa_license$location.x)
vacant$Address <- as.character(vacant$ADDRESS)
opa_license_vacant <- left_join(opa_license, vacant, by="Address")
#sum(is.na(opa_license_vacant$vacant.y))
#567658-555316=12342/12298 vacant joined

#join 4: Docket to Vacant/license/OPA
evictions_unique_clean$opa_account_num <- as.character(evictions_unique_clean$parcel_number)
opa_license_vacant_evictions <- left_join(opa_license_vacant, evictions_unique_clean, by="opa_account_num")
#table(opa_license_vacant_evictions$property_evict_count)
#578961-544441 #34520/36109
#96% of evictions joined

#join 5: Violations to Docket/Vacant/license/OPA
violations_final$opa_account_num <- as.character(violations_final$opa_account_num)
opa_license_vacant_evictions_violations<-left_join(opa_license_vacant_evictions, violations_final, by="opa_account_num")
#sum(is.na(opa_license_vacant_evictions_violations$violation))
#578961-492359
#86602/82251

#remove duplicates but keep most recent violation row
opa_license_vacant_evictions_violations2 <- opa_license_vacant_evictions_violations[order(opa_license_vacant_evictions_violations$opa_account_num, opa_license_vacant_evictions_violations$caseaddeddate, decreasing=TRUE),]
opa_license_vacant_evictions_violations3 <- opa_license_vacant_evictions_violations2[!duplicated(opa_license_vacant_evictions_violations2$opa_account_num),]
#sum(is.na(opa_license_vacant_evictions_violations3$violation))
#567261-488442
#78819/82251
#96% join

#remove extra columns
join5_clean <- subset(opa_license_vacant_evictions_violations3, select=c(
  opa_account_num,lng.x,lat.x, location.x, objectid,matching_tax_address, owner_1,
  inPhillyLL,illegal_rental,licensetyp,initialiss,mostrecent,licensesta,
  numberofun,property_evict_count,LL_evict_count,evict,id,d_filing.x,plaintiff,plaintiff_address,fitness,unaware,
  violationdate,mostrecentinsp,status,total_viol_count,hazard_viol_count,violation))

#write.csv(join5_clean, 'processed_data/all_joined_properties.csv')