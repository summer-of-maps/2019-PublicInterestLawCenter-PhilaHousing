library(corrplot)

#READ IN DATA
#remove extra columns, only bring in >1 population, and rename viol_count
#rename counts and cut down columns, make it just a df
tracts_IR <- st_read('raw_data/IRs_tract_join.shp') %>%
  st_transform(crs=2272) %>%
  mutate(IR_count = Count_) %>%
  select(-FID_1, -OBJECTID, -STATEFP10, -COUNTYFP10, -TRACTCE10, -NAME10, -NAMELSAD10, -MTFCC10, -FUNCSTAT10, -ALAND10,
         -AWATER10, -LOGRECNO, -GEOID2, -tract) %>%
  filter(tot_popula >=1)


#st_join points to demographics (points already joined in Task 3)
LLC_tract <- LLC_sf %>%
  st_join (tracts_IR, join=st_within)

evict_tract <- OPA_evict_sf %>%
  st_join (tracts_IR, join=st_within)

viol_tract <- violations_clean_sf %>%
  st_join (tracts_IR, join=st_within)

#aggregate counts to the tract level
LLC_count <- LLC_tract %>%
  drop_na(GEOID10) %>%
  group_by(GEOID10) %>%
  summarize(LLC_count=n()) %>%
  as.data.frame() %>%
  select(-geometry)

evict_count <- evict_tract %>%
  drop_na(GEOID10) %>%
  group_by(GEOID10) %>%
  summarize(evict_count =n()) %>%
  as.data.frame() %>%
  select(-geometry)

viol_count <- viol_tract %>%
  drop_na(GEOID10) %>%
  group_by(GEOID10) %>%
  summarize(viol_count =n()) %>%
  as.data.frame() %>%
  select(-geometry)

#left join together
tract_counts <- tracts_IR %>%
  left_join(viol_count) %>%
  left_join (LLC_count) %>%
  left_join (evict_count)

#read in latino
latino <- read.csv('raw_data/census_demos.csv')

#join back to tract_counts
latino$GEOID10 <- as.character(latino$GEOID10)
tract_counts <- tract_counts %>%
  left_join(latino)
tract_counts$pctLatino <- ((tract_counts$Latino/tract_counts$tot_popula)*100)

#create majority column by creating df
demos_df <- tract_counts %>%
  mutate(White = num_white, Black = num_black, Asian= num_asian) %>%
  select(GEOID10, White, Black, Asian, Latino) %>%
  as.data.frame() %>%
  select(-geometry)

#determine majority race
tract_counts$majority_race<-colnames(demos_df)[2:5][apply(demos_df[2:5], 1,which.max)]
#create binary for white/non-white
tract_counts$minority_majority<-ifelse(tract_counts$majority_race!="White", 1,0)

#write.csv(tract_counts, 'processed_data/tract_counts.csv')

#######################MODELING  #################
#linear models and scatterplots
#Race and Violations
ggplot(tract_counts, aes(x=pct_notwht, y=viol_count))+
  geom_point(color="#7a0177")+
  geom_smooth(method=lm, color= "#f768a1")+
  labs(x="Percent Not White",
       y="Count of HHs with Hazardous Violations")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

race_viol <- lm(viol_count~pct_notwht, data=tract_counts)
summary(race_viol)

#Race and Evictions
ggplot(tract_counts, aes(x=pct_notwht, y=evict_count))+
  geom_point(color="#7a0177")+
  geom_smooth(method=lm, color= "#f768a1")+
  labs(x="Percent Not White",
       y="Count of HHs with Evictions")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

race_evict <- lm(evict_count~pct_notwht, data=tract_counts)
summary(race_evict)

#Race and IRs
ggplot(tract_counts, aes(x=pct_notwht, y=IR_count))+
  geom_point(color="#7a0177")+
  geom_smooth(method=lm, color= "#f768a1")+
  labs(x="Percent Not White",
       y="Count of HHs ID'ed as Potential IRs")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

race_IR <- lm(IR_count~pct_notwht, data=tract_counts)
summary(race_IR)

#Race and LLCs
ggplot(tract_counts, aes(x=pct_notwht, y=LLC_count))+
  geom_point(color="#7a0177")+
  geom_smooth(method=lm, color= "#f768a1")+
  labs(x="Percent Not White",
       y="Count of LLC Owned Properties")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

race_LLC <- lm(LLC_count~pct_notwht, data=tract_counts)
summary(race_LLC)

#LLCs and Violations
ggplot(tract_counts, aes(x=LLC_count, y=viol_count))+
  geom_point(color="#7a0177")+
  geom_smooth(method=lm, color= "#f768a1")+
  labs(x="Count of LLC Owned Properties",
       y="Count of Properties with Hazardous Violations")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

LLC_viol <- lm(viol_count~LLC_count, data=tract_counts)
summary(LLC_viol)

#LLCs and Evictions
ggplot(tract_counts, aes(x=LLC_count, y=evict_count))+
  geom_point(color="#7a0177")+
  geom_smooth(method=lm, color= "#f768a1")+
  labs(x="Count of LLC Owned Properties",
       y="Count of Properties with Evictions")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

LLC_evict <- lm(evict_count~LLC_count, data=tract_counts)
summary(race_LLC)

#LLCs and IRs
ggplot(tract_counts, aes(x=LLC_count, y=IR_count))+
  geom_point(color="#7a0177")+
  geom_smooth(method=lm, color= "#f768a1")+
  labs(x="Count of LLC Owned Properties",
       y="Count of Potential Illegal Rentals")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

LLC_IR <- lm(IR_count~LLC_count, data=tract_counts)
summary(LLC_IR)

####################### COR PLOT #################
#set na to 0 to avoid question marks
tract_counts$viol_count[is.na(tract_counts$viol_count)] <- 0
tract_counts$evict_count[is.na(tract_counts$evict_count)] <- 0

VARIABLES <- tract_counts %>%
  as.data.frame() %>%
  mutate(Pct_White= pct_white, Pct_Black = pct_black, Pct_Asian = pct_asian,
         Pct_NotWhite = pct_notwht, Pct_Latino = pctLatino, Pct_NotUSborn = pct_foreig,
         Pct_NonCitizen = pct_noncit, Pct_Poverty = pct_pop_be, Pct_RentBurden = pct_rent_o,
         Pct_LeadElev = per_elevat) %>%
  select (-geometry) %>%
  select (Pct_White, Pct_Black, Pct_Asian, Pct_Latino, Pct_NotWhite,
          Pct_NotUSborn, Pct_NonCitizen, Pct_Poverty, Pct_RentBurden,
          Pct_LeadElev, viol_count, IR_count, LLC_count, evict_count)

M <- cor(VARIABLES)
corrplot(M,type = "upper", method='number', tl.col = "#4D4C4C")
