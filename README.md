Mapping Substandard Housing in Philadelphia
---
Philadelphia’s median housing age is 93 years old, making much of the city’s housing stock qualify as substandard housing. Philadelphia has extensive data available on housing, tenure, ownership, habitability, but analyzing across datasets can prove difficult.

This R project will show anyone who is interested, how to identify substandard housing, the communities it impacts, and the landlords responsible. The R project outlines the required cleaning and wrangling of administrative datasets from the Office of Property Assessments, Licenses and Inspections, and the Municipal Court Docket, to name a few.

Getting Started
---

Clone this repo in its entirety as each script's outputs feed into other scripts. Inside of each folder are scripts and R projects. Open the R projects in your directory in R studio.

**Data to Download**

Start by downloading the following datasets from OpenDataPhilly:
- Office of Property Assessment, Property Assessments  
- Licenses and Inspections, Licenses  
- Licenses and Inspections, Code Violations  
- Email summerofmaps@azavea.com to access the eviction docket, scraped by Jonathan Pyle

**Prerequisites**

This cleaning and analysis were conducted using RStudio version 3.5.1 and the following packages:

- dplyr  
- stringr  
- lubridate  
- data.table  
- ggplot2  
- sf  
- corrplot


**Running the scripts**

Start by cloning this repository. Scripts must be run consecutively in the following order, they cannot be run as standalone scripts:

1. *Task1_CleanData*  

- Description: Takes in the raw OpenDataPhilly files, and cleans data  
- Input: opa_properties_public.csv, li_business_licenses.shp, li_violations.csv, Vacant_Indicators_Bldg.shp, summary-table.txt, 2019_07_01_eviction_parcel.csv  
- Output: summary_clean2, heat_violations, opa_clean, illegal_rentals_only, licenses_clean, vacant, evictions_unique, violations_final  

2. *Task2_JoinData*  
- Description: Joins datasets together by OPA_ID  
- Input: opa_clean, illegal_rentals_only, licenses_clean, vacant, evictions_unique, violations_final  
- Output: join5_clean  

3. *Task3_Landlords_LLCs*
- Description: Identifies top landlords in Philly by aggregating by mailing address. Identifies LLC/shell company-owned properties by indicating properties with legal entity naming structures.
- Input: opa_clean, block_group_demos.shp, illegal_rentals_only, all_violations, violations_clean, summary_clean2
- Output: LL_db_opa_sf, LLC_sf
4. *Task4_BlockJoins* 
- Description: Spatially joins substandard housing indicators to demographics and census block groups. Aggregates substandard housing indicators to the census block group count level. 
- Input: demos, join5_clean, violations_clean, opa_clean, summary_clean2, heat_violations, LLC_sf, OPA_evict_sf, violations_clean_sf
- Output: block_counts
5. *Task5_Regression* 
- Description: Loads in census demographics at the tract level, aggregates counts to the tract level, explores the relationship between demographics and substandard housing.
- Input: IRs_tract_join.shp, LLC_sf, OPA_evict_sf, violations_clean_sf, census_demos.csv 
- Output: tract_counts, VARIABLES
6. *Task6_Unfit_Unware* 
- Description: Determines which properties had open violations at the time of their eviction filing. 
- Input: all_violations, summary_clean2
- Output: unaware_all_viols_2017, unaware_all_viols_2018, fit_hazard_viols_2017, fit_hazard_viols_2018

Authors
---
Fay Walker, Azavea Summer of Maps Fellow
Mentor: Esther Needham, Azavea Data Analytics Project Manager

Acknowledgments
---
This code was written as part of an Azavea Summer of Maps Fellowship, for The Public Interest Law Center.
