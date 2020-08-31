## Uganda 4W Data Cleaning - COVID19
## Last modified 30-06-2020

rm(list = ls())
today <- Sys.Date()

## Load libraries
library(readxl)
library(tidyverse)
library(butteR)
library(koboloadeR)
library(survey)
library(openxlsx)
library(lubridate)
library(sf)
library(srvyr)
library(clog)


## Load data and clean headers
lst <- lapply(1:2, function(i) read_excel("inputs/Uganda_Ministry_of_Health_COVID-19_Response_Activity_and_Resource_Mapping_-_all_versions_-_False_-_2020-07-14-12-56-01.xlsx", 
                                          sheet = i))
orgs <- lst[[1]]
activities <- lst[[2]]

colnames(orgs)<-gsub("\\/", ".", colnames(orgs))
colnames(activities)<-gsub("\\/", ".", colnames(activities))

## Load Kobo tool
kobo_tool <- lapply(1:3, function(i) read_excel("inputs/tool/Uganda_ULEARN_4W_Kobo.xlsx", sheet = i))
survey_sheet <- kobo_tool[[1]]
choices_sheet <- kobo_tool[[2]]

## Delete test entries, NAs, and other issues
orgs <- orgs %>% filter(!is.na(name_agency)) %>% filter(!name_agency == "w") %>% filter(!grepl("test|Test|TEST", name_agency))
activities <- Filter(function(x)!all(is.na(x) ), activities)

## Join orgs and activities
df <- left_join(orgs,activities, by = c("_index" = "_parent_index"))


## Delete test UUIDs
test_uuid <- c("54079d36-3bdd-415b-b38e-ebd1ac28419e","8f264159-abb4-4f3b-9a3a-b0d552c221af",
               "d47712cc-2705-458c-b115-ecb4e7721e29","8d4ebeb3-a5de-4a26-9958-14b95e6b8b60",
               "a9dca676-d89c-4366-8574-75f74ee8ee64", "d7fa94d3-a15c-440c-ac44-04c1edfd3062",	
               "a855df34-5b73-47db-bc2e-7c3c0458968f")

df <- df %>%  filter(!(`_uuid` %in% test_uuid))


## Data recoding 

## Column headers of multiple select columns
df[df==FALSE] <- 0
df[df==TRUE] <- 1

## Rename columns for data cleaning
source("./scripts/rename_columns.R")
df <- df %>% filter(!is.na(project_id))


df$projects_uID <- paste0("ID-",df$project_id)

## Save file to proceed with data cleaning
write.xlsx(df, paste0("./outputs/UG_4W COVID_for data cleaning_",today,".xlsx"))
read.xlsx(paste0("./outputs/UG_4W COVID_for data cleaning_",today,".xlsx"))


## Cleaning using cleaning log
## Load cleaning log
cleaning_log <- read.xlsx("./inputs/ULEARN_4Ws matrix_cleaning_log_AN_V4.xlsx")
cleaning_log_num <- read.xlsx("./inputs/ULEARN_4Ws matrix_cleaning_log_AN_V3.xlsx")


#df_id <- df$projects_uID
#cleaning_log <- cleaning_log %>% filter((uuid %in% df_id))

my_cleaninglog <- cleaninglog(ids = cleaning_log$uuid,
                              variables = cleaning_log$indicator,
                              new_values = cleaning_log$new_value,
                              name = cleaning_log$change_type,
                              change = cleaning_log$edit,
                              data_id_column_name = "projects_uID")


df <- clog_clean(df, my_cleaninglog)

my_cleaninglog_num <- cleaninglog(ids = cleaning_log_num$uuid,
                              variables = cleaning_log_num$indicator,
                              new_values = cleaning_log_num$new_value,
                              name = cleaning_log_num$change_type,
                              change = cleaning_log_num$edit,
                              data_id_column_name = "projects_uID")


df <- clog_clean(df, my_cleaninglog_num)




## Overview by partner
## We need the internal funding as we will add as part of the donor list
df$donor_internal_funds <- "NA"
df$donor_internal_funds[df$donor.internal_funds == 1] <- "Internal Funding"

## After recoding we are able to remove the integer individual select columns from the dataset as this is just a summary
df_text <- df %>% select_if(is.character)

df_text_projects <- df_text %>% select(projects_uID, donor:other_activities, donor_internal_funds)

Org_contact <- df %>% select(projects_uID, name_agency, name:new_project_funding, funding_amount, -contains("note"), -projects_num, -comments,-info_needs)


contact_details <- Org_contact %>% select(name_agency,name,position,email,phone) %>% distinct(email,.keep_all= TRUE)

## Let us merge all these columns/fields into one database
analysis_df_list<-list(Org_contact,df_text_projects)

df_update <-purrr::reduce(analysis_df_list, left_join)

## Pillars list
df_update$coordination1 <- "NA"
df_update$coordination1[df_update$coordination != "none"] <- "Coordination"

df_update$infection_prevention1 <- "NA"
df_update$infection_prevention1[df_update$infection_prevention != "none"] <- "Infection prevention"

df_update$surveillance1 <- "NA"
df_update$surveillance1[df_update$surveillance != "none"] <- "Surveillance"

df_update$case_management1 <- "NA"
df_update$case_management1[df_update$case_management != "none"] <- "Case management"

df_update$wash1 <- "NA"
df_update$wash1[df_update$WH != "none"] <- "WASH"

df_update$ict_innovations1 <- "NA"
df_update$ict_innovations1[df_update$ict_innovation != "none"] <- "ICT innovations"

df_update$mental_health1 <- "NA"
df_update$mental_health1[df_update$mental_health_psychosocial_support != "none"] <- "Mental health"

df_update$risk_communications1 <- "NA"
df_update$risk_communications1[df_update$risk_communication != "none"] <- "Risk communications"

df_update$logistics1 <- "NA"
df_update$logistics1[df_update$logistics != "none"] <- "Logistics"

df_update$human_resources1 <- "NA"
df_update$human_resources1[df_update$human_resources != "none"] <- "Human resources"

## Concatenated covered pillars per project
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

df_update$pillars <- paste(df_update$coordination1, df_update$infection_prevention1, df_update$surveillance1, df_update$case_management1,
                           df_update$wash1, df_update$ict_innovations1, df_update$mental_health1, df_update$risk_communications1,
                           df_update$logistics1, df_update$human_resources1, sep = ",")


df_update <- df_update %>% mutate(pillars = str_remove_all(pillars,"NA,")) %>% mutate(pillars = str_remove_all(pillars,",NA")) %>% 
  mutate(pillars = str_replace(pillars, "NA", "other"))

df_update$pillars <- trim(df_update$pillars)



## Concatenated donors per project
donors <- df_update %>%  select(foreign_donor:other_donor, donor_internal_funds)
df_update$donors <-  apply(donors, 1, function(x) paste(na.omit(x),collapse=", ") )

donors_columns <- df_update %>%  select(foreign_donor:other_donor, donor_internal_funds) %>% colnames()
project_donor <- df_update %>%  select(projects_uID,donors_columns)

df_update$donors <- sapply(strsplit(df_update$donors, ",", fixed = TRUE), function(x) 
  paste(unique(x), collapse = ",")) 

df_update <- df_update %>% mutate(donors = str_remove_all(donors,",other")) %>% 
  mutate(donors = str_remove_all(donors,"other,")) %>% mutate(donors = str_remove_all(donors,"NA,")) %>% mutate(donors = str_remove_all(donors,", NA"))

df_update$donors <- trim(df_update$donors)


## Concatenated regions per project
regions <- df_update %>%  select(contains("region"))
df_update$regions <-  apply(regions, 1, function(x) paste(na.omit(x),collapse=", ") )

df_update$regions <- sapply(strsplit(df_update$regions, ",", fixed = TRUE), function(x) 
  paste(unique(x), collapse = ","))

## Concatenated activities per project
districts <- df_update %>%  select(contains("district"))
df_update$districts <-  apply(districts, 1, function(x) paste(na.omit(x),collapse=", ") ) 

df_update$districts <- sapply(strsplit(df_update$districts, ",", fixed = TRUE), function(x) 
  paste(unique(x), collapse = ","))


## Concatenate implimenting partners
implimenting_partner <- df_update %>%  select(contains("implimenting"))
df_update$implimenting_partner <-  apply(implimenting_partner, 1, function(x) paste(na.omit(x),collapse=", ") ) 


# Concatenated activities per project
df_update$projects_activities <- paste(df_update$coordination,
                                       df_update$infection_prevention,
                                       df_update$surveillance,
                                       df_update$case_management,
                                       df_update$WH,
                                       df_update$ict_innovation,
                                       df_update$mental_health_psychosocial_support,
                                       df_update$risk_comms_support_form,
                                       df_update$logistics,
                                       df_update$human_resources, 
                                       sep = ", ")


df_update <- df_update %>% mutate(projects_activities = str_remove_all(projects_activities,"NA,")) %>% 
                           mutate(projects_activities = str_remove_all(projects_activities,",NA"))

df_update$projects_activities <- trim(df_update$projects_activities)


## Final Output (I guess)
df_update <- df_update %>%  select(projects_uID, 
                                   name_agency, 
                                   project_title,
                                   donors,
                                   project_status, 
                                   ongoing_ability, 
                                   new_project_funding, 
                                   funding_amount,
                                   pillars,
                                   projects_activities,
                                   other_activities,
                                   implimenting_partner, 
                                   regions, 
                                   districts)


write.csv(df_update, paste0("outputs/UG_convid_matrix_",today,".csv",na = ""))

## Pillars detail

## List of pillars
df$coordination1 <- 0
df$coordination1[df$coordination != "none"] <- 1

df$infection_prevention1 <- 0
df$infection_prevention1[df$infection_prevention != "none"] <- 1

df$surveillance1 <- 0
df$surveillance1[df$surveillance != "none"] <- 1

df$case_management1 <- 0
df$case_management1[df$case_management != "none"] <- 1

df$wash1 <- 0
df$wash1[df$WH != "none"] <- 1

df$ict_innovations1 <- 0
df$ict_innovations1[df$ict_innovation != "none"] <- 1

df$mental_health1 <- 0
df$mental_health1[df$mental_health_psychosocial_support != "none"] <- 1

df$risk_communications1 <- 0
df$risk_communications1[df$risk_communication != "none"] <- 1

df$logistics1 <- 0
df$logistics1[df$logistics != "none"] <- 1

df$human_resources1 <- 0
df$human_resources1[df$human_resources != "none"] <- 1



coord_activities_columns <- df %>%  select(contains("coordination."), -contains("none")) 
coord_activities <- df %>%  select(projects_uID,name_agency, coordination1 ,contains("coordination."), -contains("none")) %>% mutate(coord_activities_num = rowSums(coord_activities_columns, na.rm = TRUE))

infe_activities_columns <- df %>%  select(contains("infection_prevention."), - contains("none")) 
infe_activities <-df %>%  select(projects_uID,name_agency,infection_prevention1 ,contains("infection_prevention."), - contains("none")) %>% mutate( infeactivities_num = rowSums(infe_activities_columns, na.rm = TRUE))

surveilance_activities_columns <- df %>%  select(contains("surveillance."), - contains("none")) 
surveilance_activities <-df %>%  select(projects_uID,name_agency,surveillance1 ,contains("surveillance."), - contains("none")) %>% mutate( surve_activities_num = rowSums(surveilance_activities_columns, na.rm = TRUE))

casemanagement_activities_columns <- df %>%  select(contains("case_management."), - contains("none")) 
casemanagement_activities <-df %>%  select(projects_uID,name_agency,case_management1 ,contains("case_management."), - contains("none")) %>% mutate( case_activities_num = rowSums(casemanagement_activities_columns, na.rm = TRUE))

WASH_activities_columns <- df %>%  select(contains("WH."), - contains("none")) 
WASH_activities <-df %>%  select(projects_uID,name_agency, wash1 ,contains("WH."), - contains("none")) %>% mutate( WASH_activities_num = rowSums(WASH_activities_columns, na.rm = TRUE))


ict_columns <- df %>%  select(contains("ict_innovation."), - contains("none")) 
ict_activities <-df %>%  select(projects_uID,name_agency,ict_innovations1 ,contains("ict_innovation."), - contains("none")) %>% mutate( ict_activities_num = rowSums(ict_columns, na.rm = TRUE))

mental_activities_columns <- df %>%  select(contains("mental_health_psychosocial_support."), - contains("none")) 
mental_activities <-df %>%  select(projects_uID,name_agency,mental_health1 ,contains("mental_health_psychosocial_support."), - contains("none")) %>% mutate( mental_health_activities_num = rowSums(mental_activities_columns, na.rm = TRUE))


risk_activities_columns <- df %>%  select(contains("risk_communication."), - contains("none")) 
risk_activities <-df %>%  select(projects_uID,name_agency,risk_communications1 ,contains("risk_communication."), - contains("none")) %>% mutate( risk_activities_num = rowSums(risk_activities_columns, na.rm = TRUE))

logistics_activities_columns <- df %>%  select(contains("logistics."), - contains("none")) 
logistics_activities <-df %>%  select(projects_uID,name_agency,logistics1 ,contains("logistics."), - contains("none")) %>% mutate( logistics_activities_num = rowSums(logistics_activities_columns, na.rm = TRUE))

HR_activities_columns <- df %>%  select(contains("human_resources."), - contains("none")) 
HR_activities <-df %>%  select(projects_uID,name_agency,human_resources1 ,contains("human_resources."), - contains("none")) %>% mutate( HR_activities_num = rowSums(HR_activities_columns, na.rm = TRUE))

## Support form
support_form_actors <- df %>% select(ongoing_ability, new_project_funding, name_agency, coordination_support_cash_amount, 
                                     coordination_support_supplies_amount,
                                     infection_support_cash_amount, infection_support_supplies_amount,
                                     surveillance_support_supplies_amount,
                                     case_management_support_cash_amount, case_management_support_supplies_amount,
                                     wash_support_cash_amount,  wash_support_supplies_amount,
                                     ict_support_cash_amount,
                                     mental_health_support_cash_amount, mental_health_support_supplies_amount,
                                     risk_comms_support_cash_amount, risk_comms_support_supplies_amount,
                                     logs_support_supplies_amount,
                                     human_resources_support_cash_amount) %>% 
  mutate(confirmed_funding = ifelse(is.na(ongoing_ability),new_project_funding,ongoing_ability)) %>%
  filter(confirmed_funding == "yes") %>%  
  select(name_agency:human_resources_support_cash_amount)

## Table join. Let us merge all these columns/fields into one database
analysis_df_list<-list(coord_activities,infe_activities,surveilance_activities,casemanagement_activities,WASH_activities,
                       ict_activities,mental_activities,risk_activities,logistics_activities, HR_activities,support_form_actors)

projects_activities <-purrr::reduce(analysis_df_list, left_join)


actors_pillar_activity <- projects_activities %>%  
  select(name_agency, coordination1, infection_prevention1, surveillance1, case_management1, wash1,
         ict_innovations1, mental_health1,	risk_communications1,	logistics1,	human_resources1, coordination_support_cash_amount, 
         coordination_support_supplies_amount,
         infection_support_cash_amount, infection_support_supplies_amount,
         surveillance_support_supplies_amount,
         case_management_support_cash_amount, case_management_support_supplies_amount,
         wash_support_cash_amount,  wash_support_supplies_amount,
         ict_support_cash_amount,
         mental_health_support_cash_amount, mental_health_support_supplies_amount,
         risk_comms_support_cash_amount, risk_comms_support_supplies_amount,
         logs_support_supplies_amount,
         human_resources_support_cash_amount) %>% 
  group_by(name_agency) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))


## Project actor 
actor_project_id <- df_update %>%  select(projects_uID,name_agency)

## create an ID that we can use to connect our integer and character select multiple data frames
Project_ids <- df %>%  select(project_id,projects_uID) %>% 
  group_by(projects_uID)

## District info
## Individual select (0/1 integers) multiple columns for reshaping
sel_mul <- butteR::auto_detect_select_multiple(df)

df_mul_col <- df %>% select("project_id", contains("district_"), "donor", "foreign_donor",                                           
                            "human_resources", "human_resources_support_form",                            
                            "ict_innovation", "ict_support_form",                                      
                            "infection_prevention", "infection_support_form",                                  
                            "logistics", "logs_support_form",                                       
                            "mental_health_psychosocial_support", "mental_health_support_form",                              
                            "region_activity_area", "region_case_management",                                  
                            "region_coordination", "region_human_resources",                                  
                            "region_ict_innovation", "region_infection_prevention",                             
                            "region_logistics", "region_mental_health_psychosocial_support",               
                            "region_risk_communication", "region_surveillance",                                    
                            "region_WH", "risk_comms_support_form",                                 
                            "risk_communication", "surveillance",                                            
                            "surveillance_support_form", "UN",                                                      
                            "wash_support_form", "WH",   "case_management", "case_management_support_form",                            
                            "coordination", "coordination_support_form") %>% select_if(is.numeric)


aggreg_to <- df_mul_col %>% select(everything(), -project_id) %>% colnames()

multiple_select_data  <- df_mul_col %>% pivot_longer(aggreg_to,
                                                     names_to="donor_type",
                                                     values_to="indicator_val")

## Create an ID that we can use to connect our integer and character select multiple data frames
Project_ids <- df %>%  select(project_id, projects_uID) %>% 
  group_by(projects_uID)




## All activity areas districts
list <- "district_activity_area_"

df_area_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>% 
  mutate(donor_type = str_remove_all(donor_type,"district_activity_area_")) %>% 
  separate(donor_type,c("area_regions", "area_districts")) %>% 
  select(project_id,area_regions, area_districts) 

## Coordination districts
list <- "district_coordination."

df_coord_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>% 
  mutate(donor_type = str_remove_all(donor_type,"district_coordination_")) %>% 
  mutate(donor_type = str_replace(donor_type, "district_coordination.", "acholi.")) %>% 
  separate(donor_type,c("coord_regions", "coord_districts")) %>% 
  select(project_id,coord_regions, coord_districts) 


analysis_df_list<-list(Project_ids,
                       df_area_district,
                       df_coord_district,
                       coord_activities)

district_coord_activities <-purrr::reduce(analysis_df_list, left_join)


district_coord_activities <- district_coord_activities %>%  
                             mutate(region = ifelse(is.na(coord_regions),area_regions,coord_regions),
                             district =  ifelse(is.na(coord_districts),area_districts,coord_districts)) %>% 
                             filter(coordination1 == 1 & !is.na(district) ) %>%  
                             select(projects_uID,region,district,coordination1 :coord_activities_num)



list <-list(district_coord_activities,actor_project_id)

coodination_actors <-purrr::reduce(list, left_join)

## Factsheets
region_coodination_actors <- coodination_actors %>% group_by(region) %>% summarise(coord_actors = n_distinct(name_agency))
region_coodination_actors_names <- coodination_actors %>% group_by(region) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

region_coordination <- left_join(region_coodination_actors, region_coodination_actors_names)

coord_actors_locations <- coodination_actors %>% group_by(district) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

district_coord_activities <- district_coord_activities[,c(2:16)]

district_coord_activities <- district_coord_activities %>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))

list<-list(district_coord_activities,coord_actors_locations)

district_coord_activities <-purrr::reduce(list, left_join)

## Actors_in_a_region_by activity
## Infection prevention districts
list <- "district_infection_prevention."

df_infe_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>%
  mutate(donor_type = str_remove_all(donor_type,"district_infection_prevention_")) %>%
  mutate(donor_type = str_replace(donor_type, "district_infection_prevention.", "acholi.")) %>%
  separate(donor_type,c("infection_prevention_regions", "infection_prevention_districts")) %>%
  select(project_id,infection_prevention_regions,infection_prevention_districts)


infe_support <- df %>% select(projects_uID,infection_support_cash_amount,infection_support_supplies_amount)


analysis_df_list<-list(Project_ids,df_area_district,df_infe_district,infe_activities,infe_support)

district_infe_activities <-purrr::reduce(analysis_df_list, left_join)

district_infe_activities <- district_infe_activities %>%  
  mutate(region = ifelse(is.na(infection_prevention_regions),area_regions,infection_prevention_regions),
         district =  ifelse(is.na(infection_prevention_districts),area_districts,infection_prevention_districts)) %>% 
  filter(infection_prevention1 == 1 & !is.na(district)  ) %>%  select(region,district,infection_prevention1:infection_support_supplies_amount)


list <-list(district_infe_activities,actor_project_id)

infection_actors <-purrr::reduce(list, left_join)

## Factsheets
region_infe_actors <- infection_actors %>% group_by(region) %>% summarise(infe_actors = n_distinct(name_agency))
region_infe_actors_names <- infection_actors %>% group_by(region) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

region_infection <- left_join(region_infe_actors, region_infe_actors_names)

infection_actors_locations <-infection_actors %>% group_by(district) %>% 
  summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

district_infe_activities <- district_infe_activities[,2:12]

district_infe_activities <- district_infe_activities %>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))


list<-list(district_infe_activities,infection_actors_locations)

district_infe_activities <-purrr::reduce(list, left_join)


## Surveillance districts
list <- "district_surveillance."

df_surv_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>%
  mutate(donor_type = str_remove_all(donor_type,"district_surveillance_")) %>%
  mutate(donor_type = str_replace(donor_type, "district_surveillance.", "acholi.")) %>%
  separate(donor_type,c("surveilance_regions", "surveilance_districts"))%>%
  select(project_id,surveilance_regions,surveilance_districts)


surveilance_support <- df %>% select(projects_uID, surveillance_support_supplies_amount)

analysis_df_list<-list(Project_ids,df_area_district,df_surv_district,surveilance_activities,surveilance_support)

district_surve_activities <-purrr::reduce(analysis_df_list, left_join)

district_surve_activities <- district_surve_activities %>%  
  mutate(region = ifelse(is.na(surveilance_regions),area_regions,surveilance_regions),
         district =  ifelse(is.na(surveilance_districts),area_districts,surveilance_districts)) %>% 
  filter(surveillance1 == 1 & !is.na(district)  ) %>%  select(projects_uID,region,district,surveillance1:surveillance_support_supplies_amount)

list <-list(district_surve_activities,actor_project_id)

survey_actors <-purrr::reduce(list, left_join)


## Factsheets
region_survey_actors <- survey_actors %>% group_by(region) %>% summarise(survey_actors = n_distinct(name_agency))
region_survey_actors_names <-survey_actors %>% group_by(region) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

region_surveillance <- left_join(region_survey_actors, region_survey_actors_names)

survey_actors_locations <-survey_actors %>% group_by(district) %>% 
  summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

district_surve_activities <- district_surve_activities[,2:17]

district_surve_activities <- district_surve_activities%>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))

list<-list(district_surve_activities,survey_actors_locations)

district_surve_activities <-purrr::reduce(list, left_join)


## Case management district
list <- "district_case_management."

df_case_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>%
  mutate(donor_type = str_remove_all(donor_type,"district_case_management_")) %>%
  mutate(donor_type = str_replace(donor_type, "district_case_management.", "acholi.")) %>%
  separate(donor_type,c("case_management_regions", "case_mangement_districts")) %>%
  select(project_id,case_management_regions,case_mangement_districts)

case_support <- df %>% select(projects_uID,case_management_support_cash_amount,case_management_support_supplies_amount)

analysis_df_list<-list(Project_ids,df_area_district,df_case_district,casemanagement_activities,case_support)

district_case_activities <-purrr::reduce(analysis_df_list, left_join)

district_case_activities <- district_case_activities %>%  
  mutate(region = ifelse(is.na(case_management_regions),area_regions,case_management_regions),
         district =  ifelse(is.na(case_mangement_districts),area_districts,case_mangement_districts)) %>% 
  filter(case_management1 == 1 & !is.na(district)  ) %>%  select(projects_uID,region,district,case_management1:case_management_support_supplies_amount)

list <-list(district_case_activities,actor_project_id)

case_actors <-purrr::reduce(list, left_join)


## Factsheets
region_case_actors <- case_actors %>% group_by(region) %>% summarise(case_actors = n_distinct(name_agency))
region_case_actors_names <- case_actors %>% group_by(region) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

region_case_management <- left_join(region_case_actors, region_case_actors_names)

case_actors_locations <-case_actors %>% group_by(district) %>% 
  summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

district_case_activities <- district_case_activities[,2:18]

district_case_activities <- district_case_activities %>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))

list<-list(district_case_activities,case_actors_locations)

district_case_activities <-purrr::reduce(list, left_join)


## WASH districts
list <- "district_WH."

df_WH_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>%
  mutate(donor_type = str_remove_all(donor_type,"district_WH_")) %>%
  mutate(donor_type = str_replace(donor_type, "district_WH.", "acholi.")) %>%
  separate(donor_type,c("WH_regions", "WH_districts")) %>%
  select(project_id,WH_regions,WH_districts)

WH_support <- df %>% select(projects_uID,wash_support_cash_amount,wash_support_supplies_amount)

analysis_df_list<-list(Project_ids,df_area_district,df_WH_district,WASH_activities,WH_support)

district_WH_activities <-purrr::reduce(analysis_df_list, left_join)

district_WH_activities <- district_WH_activities %>%  
  mutate(region = ifelse(is.na(WH_regions),area_regions,WH_regions),
         district =  ifelse(is.na(WH_districts),area_districts,WH_districts)) %>% 
  filter(wash1 == 1 & !is.na(district)  ) %>%  select(projects_uID,region,district,wash1:wash_support_supplies_amount)

list <-list(district_WH_activities,actor_project_id)

WH_actors <-purrr::reduce(list, left_join)

## Factsheets
region_WH_actors <- WH_actors %>% group_by(region) %>% summarise(WASH_actors = n_distinct(name_agency))
region_WH_actors_names <- WH_actors %>% group_by(region) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

region_WH <- left_join(region_WH_actors, region_WH_actors_names)

WH_actors_locations <- WH_actors %>% group_by(district) %>% 
  summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

district_WH_activities <- district_WH_activities[,2:13]

district_WH_activities <- district_WH_activities %>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))

list<-list(district_WH_activities,WH_actors_locations)

district_WH_activities <-purrr::reduce(list, left_join)


## Ict district
list <- "district_ict_innovation."

df_ICT_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>%
  mutate(donor_type = str_remove_all(donor_type,"district_ict_innovation_")) %>%
  mutate(donor_type = str_replace(donor_type, "district_ict_innovation.", "acholi.")) %>%
  separate(donor_type,c("ict_regions", "ict_districts")) %>%
  select(project_id,ict_regions,ict_districts)

ict_support <- df %>% select(projects_uID,ict_support_cash_amount)

analysis_df_list<-list(Project_ids,df_area_district,df_ICT_district,ict_activities,ict_support)

district_ict_activities <-purrr::reduce(analysis_df_list, left_join)

district_ict_activities <- district_ict_activities %>%  
  mutate(region = ifelse(is.na(ict_regions),area_regions,ict_regions),
         district =  ifelse(is.na(ict_districts),area_districts,ict_districts)) %>% 
  filter(ict_innovations1 == 1 & !is.na(district)  ) %>%  select(projects_uID,region,district,ict_innovations1:ict_support_cash_amount)


list <-list(district_ict_activities,actor_project_id)

ict_actors <-purrr::reduce(list, left_join)


## Factsheets
region_ict_actors <- ict_actors %>% group_by(region) %>% summarise(ict_actors = n_distinct(name_agency))
region_ict_actors_names <- ict_actors %>% group_by(region) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

region_ict <- left_join(region_ict_actors, region_ict_actors_names)

ict_actors_locations <- ict_actors %>% group_by(district) %>% 
  summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

district_ict_activities <- district_ict_activities[,2:19]

district_ict_activities <- district_ict_activities %>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))

list<-list(district_ict_activities,ict_actors_locations)
district_ict_activities <-purrr::reduce(list, left_join)


## Mental Health districts
list <- "district_mental_health_psychosocial_support."

df_mental_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>%
  mutate(donor_type = str_remove_all(donor_type,"district_mental_health_psychosocial_support_")) %>%
  mutate(donor_type = str_replace(donor_type, "district_mental_health_psychosocial_support.", "acholi.")) %>%
  separate(donor_type,c("mental_regions", "mental_districts")) %>%
  select(project_id,mental_regions,mental_districts)


mental_support <- df %>% select(projects_uID,mental_health_support_cash_amount,mental_health_support_supplies_amount)

analysis_df_list<-list(Project_ids,df_area_district,df_mental_district,mental_activities,mental_support)


district_mental_activities <-purrr::reduce(analysis_df_list, left_join)

district_mental_activities <- district_mental_activities %>%  
  mutate(region = ifelse(is.na(mental_regions),area_regions,mental_regions),
         district =  ifelse(is.na(mental_districts),area_districts,mental_districts)) %>% 
  filter(mental_health1 == 1 & !is.na(district)  ) %>%  select(projects_uID,region,district,mental_health1:mental_health_support_supplies_amount)

list <-list(district_mental_activities,actor_project_id)

mental_actors <-purrr::reduce(list, left_join)

## Factsheets
region_mental_actors <- mental_actors %>% group_by(region) %>% summarise(mental_actors = n_distinct(name_agency))
region_mental_actors_names <- mental_actors %>% group_by(region) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))
  
region_mental_health <- left_join(region_mental_actors, region_mental_actors_names)


mental_actors_locations <- mental_actors %>% group_by(district) %>% 
  summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

district_mental_activities <- district_mental_activities[,2:11]

district_mental_activities <- district_mental_activities %>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))

list<-list(district_mental_activities,mental_actors_locations)
district_mental_activities <-purrr::reduce(list, left_join)


## Risk districts
list <- "district_risk_communication."

df_risk_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>%
  mutate(donor_type = str_remove_all(donor_type,"district_risk_communication_")) %>%
  mutate(donor_type = str_replace(donor_type, "district_risk_communication.", "acholi.")) %>%
  separate(donor_type,c("risk_comm_regions", "risk_comm_districts")) %>%
  select(project_id,risk_comm_regions,risk_comm_districts)


riskcomm_support <- df %>% select(projects_uID,risk_comms_support_cash_amount,risk_comms_support_supplies_amount)

analysis_df_list<-list(Project_ids,df_area_district,df_risk_district,risk_activities,riskcomm_support)

district_risk_activities <-purrr::reduce(analysis_df_list, left_join)

district_risk_activities <- district_risk_activities %>%  
  mutate(region = ifelse(is.na(risk_comm_regions),area_regions,risk_comm_regions),
         district =  ifelse(is.na(risk_comm_districts),area_districts,risk_comm_districts)) %>% 
  filter(risk_communications1 == 1 & !is.na(district)  ) %>%  select(projects_uID,region, district,risk_communications1:risk_comms_support_supplies_amount)


list <-list(district_risk_activities,actor_project_id)

risk_actors <-purrr::reduce(list, left_join)


## Factsheets
region_risk_actors <- risk_actors %>% group_by(region) %>% summarise(risk_actors = n_distinct(name_agency))
region_risk_actors_names <- risk_actors %>% group_by(region) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

region_risk_comms <- left_join(region_risk_actors, region_risk_actors_names)


risk_actors_locations <- risk_actors %>% group_by(district) %>% 
  summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

district_risk_activities <- district_risk_activities[,2:23]

district_risk_activities <- district_risk_activities %>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))

list<-list(district_risk_activities,risk_actors_locations)
district_risk_activities <-purrr::reduce(list, left_join)


## logistics districts
list <- "district_logistics."

df_logs_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>%
  mutate(donor_type = str_remove_all(donor_type,"district_logistics_")) %>%
  mutate(donor_type = str_replace(donor_type, "district_logistics.", "acholi.")) %>%
  separate(donor_type,c("logs_regions", "logs_districts")) %>%
  select(project_id,logs_regions,logs_districts)

logs_support <- df %>% select(projects_uID,logs_support_supplies_amount)

analysis_df_list<-list(Project_ids,df_area_district,df_logs_district,logistics_activities,logs_support)

district_logs_activities <-purrr::reduce(analysis_df_list, left_join)

district_logs_activities <- district_logs_activities %>%  
  mutate(region = ifelse(is.na(logs_regions),area_regions,logs_regions),
         district =  ifelse(is.na(logs_districts),area_districts,logs_districts)) %>% 
  filter(logistics1 == 1 & !is.na(district)  ) %>%  select(projects_uID,region,district,logistics1:logs_support_supplies_amount)


list <-list(district_logs_activities,actor_project_id)

logs_actors <-purrr::reduce(list, left_join)

## Factsheets
region_logs_actors <- logs_actors %>% group_by(region) %>% summarise(logs_actors = n_distinct(name_agency))
region_logs_actors_names <- logs_actors %>% group_by(region) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

region_logistics <- left_join(region_logs_actors, region_logs_actors_names) 

logs_actors_locations <- logs_actors %>% group_by(district) %>% 
  summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

district_logs_activities <- district_logs_activities[,2:54]

district_logs_activities <- district_logs_activities %>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))

list<-list(district_logs_activities,logs_actors_locations)
district_logs_activities <-purrr::reduce(list, left_join)


## HR districts
list <- "district_human_resources."

df_HR_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>%
  mutate(donor_type = str_remove_all(donor_type,"district_human_resources_")) %>%
  mutate(donor_type = str_replace(donor_type, "district_human_resources.", "acholi.")) %>%
  separate(donor_type,c("HR_regions", "HR_districts")) %>%
  select(project_id,HR_regions,HR_districts)


HR_support <- df %>% select(projects_uID,human_resources_support_cash_amount)


analysis_df_list<-list(Project_ids,df_area_district,df_HR_district,HR_activities,HR_support)

district_hr_activities <-purrr::reduce(analysis_df_list, left_join)

district_hr_activities <- district_hr_activities %>%  
  mutate(region = ifelse(is.na(HR_regions),area_regions,HR_regions),
         district =  ifelse(is.na(HR_districts),area_districts,HR_districts)) %>% 
  filter(human_resources1 == 1 & !is.na(district)) %>%  select(projects_uID,region,district,human_resources1:human_resources_support_cash_amount)


list <-list(district_hr_activities,actor_project_id)

hr_actors <-purrr::reduce(list, left_join)


## Factsheets
region_hr_actors <- hr_actors %>% group_by(region) %>% summarise(hr_actors = n_distinct(name_agency))
region_hr_actors_names <- hr_actors %>% group_by(region) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

region_hr <- left_join(region_hr_actors, region_hr_actors_names)


hr_actors_locations <- hr_actors %>% group_by(district) %>% 
  summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

district_hr_activities <- district_hr_activities[,2:17]

district_hr_activities <- district_hr_activities %>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))

list<-list(district_hr_activities,hr_actors_locations)
district_hr_activities <-purrr::reduce(list, left_join)


## Uganda regions and districts info
admin_info <- choices_sheet %>% slice(222:356) %>%  filter(!is.na(list_name))

## Districts list
ug_regions <-  admin_info %>%  filter(list_name == "region_list") %>%  select(name, label) %>%  rename("region"= name, "region_label"= label)


## Factsheets data merge
analysis_df_list<-list(region_coodination_actors,region_infe_actors,region_survey_actors,region_case_actors,region_WH_actors,region_ict_actors,
                       region_risk_actors,region_mental_actors,region_logs_actors,region_hr_actors)

region_factsheets <-purrr::reduce(analysis_df_list, left_join)


colnames(region_factsheets) <-paste0("num_", colnames(region_factsheets))

region_factsheets <- region_factsheets %>% rename("region"=num_region)

## Actors mappiung
actors_districts <- bind_rows(coodination_actors,infection_actors,case_actors,survey_actors,WH_actors,
                              ict_actors,risk_actors,mental_actors,logs_actors,hr_actors)

## Donors districts
analysis_df_list<-list(actors_districts, project_donor)

project_region_donor <-purrr::reduce(analysis_df_list, left_join)


project_region_donor <- project_region_donor %>% select(region,donors_columns)

donors_regions  <- project_region_donor %>% pivot_longer(donors_columns,
                                                         names_to="donor_type",
                                                         values_to="indicator_val")


region_donors <- donors_regions %>% filter(!is.na(indicator_val))

region_donors_list <- region_donors %>% group_by(region) %>% 
                      summarize(region_donor = paste(sort(unique(indicator_val)),collapse=" | "))

region_donors_count <- region_donors[,2:4]


region_donors_count <- region_donors_count %>% group_by(region) %>% summarise(donors_count = n_distinct(indicator_val))

## Coordination actor district
coodination_actors <- actors_districts %>% filter(coordination1 == 1) %>% select(region, district, name_agency)


region_coodination_districts <- coodination_actors %>% group_by(region) %>% 
  summarize(coord_districts = paste(sort(unique(district)),collapse=", "))

region_coodination_actors <-coodination_actors %>% group_by(region) %>% 
  summarize(coord_actors = paste(sort(unique(name_agency)),collapse=", "))

all_coodination_actors <- coodination_actors[,4]
num_coodination_actors <- coodination_actors[,4]

all_coodination_actors <-all_coodination_actors %>%  
  summarize(coord_actors = paste(sort(unique(name_agency)),collapse=", "))

num_total_coord_actors <- num_coodination_actors %>%  summarise(num_total_cord_actors = n_distinct(name_agency))

num_cord_districts <- coodination_actors %>% group_by(region) %>% summarise(num_cord_districts = n_distinct(district))

## Infection disease actors
infection_actors <- actors_districts %>% filter(infection_prevention1 == 1) %>% select(region,district,name_agency)

region_infe_districts <- infection_actors %>% group_by(region) %>% 
  summarize(infection_districts = paste(sort(unique(district)),collapse=", "))


region_infe_actors <-infection_actors %>% group_by(region) %>% 
  summarize(infe_actors = paste(sort(unique(name_agency)),collapse=", "))


num_infe_districts <- infection_actors %>% group_by(region) %>% summarise(num_infe_districts = n_distinct(district))

## Surveillance actors
survey_actors <- actors_districts %>% filter(surveillance1 == 1) %>% select(region,district,name_agency)


region_sur_districts <-survey_actors %>% group_by(region) %>% 
  summarize(survey_districts = paste(sort(unique(district)),collapse=", "))


region_sur_actors <-survey_actors %>% group_by(region) %>% 
  summarize(survey_actors = paste(sort(unique(name_agency)),collapse=", "))


num_survey_districts <- survey_actors %>% group_by(region) %>% summarise(num_survey_districts = n_distinct(district))

## Case management actors
case_actors <- actors_districts %>% filter(case_management1 == 1) %>% select(region,district,name_agency)

region_case_districts <-case_actors %>% group_by(region) %>% 
  summarize(case_districts = paste(sort(unique(district)),collapse=", "))


region_case_actors <-case_actors %>% group_by(region) %>% 
  summarize(case_actors = paste(sort(unique(name_agency)),collapse=", "))

num_case_districts <- case_actors %>% group_by(region) %>% summarise(num_case_districts = n_distinct(district))


## WASH actors
WH_actors <- actors_districts %>% filter(wash1 == 1) %>% select(region,district,name_agency)

region_WH_districts <-WH_actors %>% group_by(region) %>% 
  summarize(wh_districts = paste(sort(unique(district)),collapse=", "))


region_WH_actors <-WH_actors %>% group_by(region) %>% 
  summarize(wh_actors = paste(sort(unique(name_agency)),collapse=", "))


num_WH_districts <- WH_actors %>% group_by(region) %>% summarise(num_wh_districts = n_distinct(district))

## ICT actors
ict_actors <- actors_districts %>% filter(ict_innovations1 == 1) %>% select(region,district,name_agency)

region_ict_districts <-ict_actors %>% group_by(region) %>% 
  summarize(ict_districts = paste(sort(unique(district)),collapse=", "))


region_ict_actors <-ict_actors %>% group_by(region) %>% 
  summarize(ict_actors = paste(sort(unique(name_agency)),collapse=", "))


num_ict_districts <- ict_actors %>% group_by(region) %>% summarise(ict_wh_districts = n_distinct(district))


## Risk communications actors
risk_actors <- actors_districts %>% filter(risk_communications1 == 1) %>% select(region,district,name_agency)

region_risk_districts <-district_risk_activities %>% group_by(region) %>% 
  summarize(risk_districts = paste(sort(unique(district)),collapse=", "))


region_risk_actors <-risk_actors %>% group_by(region) %>% 
  summarize(risk_actors = paste(sort(unique(name_agency)),collapse=", "))

num_risk_districts <- district_risk_activities %>% group_by(region) %>% summarise(num_risk_districts = n_distinct(district))


## Mental health actors
mental_actors <- actors_districts %>% filter(mental_health1 == 1) %>% select(region,district,name_agency)

region_mental_districts <-mental_actors %>% group_by(region) %>% 
  summarize(mental_districts = paste(sort(unique(district)),collapse=", "))


region_mental_actors <-mental_actors %>% group_by(region) %>% 
  summarize(mental_actors = paste(sort(unique(name_agency)),collapse=", "))

num_mental_districts <- mental_actors %>% group_by(region) %>% summarise(num_mental_districts = n_distinct(district))

## Logistics actors
logs_actors <- actors_districts %>% filter(logistics1 == 1) %>% select(region,district,name_agency)

region_logs_districts <-logs_actors %>% group_by(region) %>% 
  summarize(logs_districts = paste(sort(unique(district)),collapse=", "))


region_logs_actors <-logs_actors %>% group_by(region) %>% 
  summarize(logs_actors = paste(sort(unique(name_agency)),collapse=", "))

num_logs_districts <- logs_actors %>% group_by(region) %>% summarise(num_logs_districts = n_distinct(district))


## HR actors
hr_actors <- actors_districts %>% filter(human_resources1 == 1) %>% select(region,district,name_agency)

region_hr_districts <-hr_actors %>% group_by(region) %>% 
  summarize(hr_districts = paste(sort(unique(district)),collapse=", "))


region_hr_actors <-hr_actors %>% group_by(region) %>% 
  summarize(hr_actors = paste(sort(unique(name_agency)),collapse=", "))

num_hr_districts <- hr_actors %>% group_by(region) %>% summarise(num_hr_districts = n_distinct(district))


## Factsheets data merge
analysis_df_list<-list(region_coodination_actors,
                       region_infe_actors,
                       region_sur_actors,
                       region_case_actors,
                       region_WH_actors,
                       region_ict_actors,
                       region_risk_actors,
                       region_mental_actors,
                       region_logs_actors,
                       region_hr_actors)

region_factsheets_names <-purrr::reduce(analysis_df_list, left_join)


analysis_df_list<-list(num_cord_districts,
                       num_infe_districts,
                       num_survey_districts,
                       num_case_districts,
                       num_WH_districts,
                       num_ict_districts,
                       num_risk_districts,
                       num_mental_districts,
                       num_logs_districts,
                       num_hr_districts)

region_factsheets_figures <-purrr::reduce(analysis_df_list, left_join)


analysis_df_list<-list(region_coodination_districts,
                       region_infe_districts, 
                       region_sur_districts,
                       region_case_districts,
                       region_WH_districts,
                       region_ict_districts,
                       region_risk_districts,
                       region_mental_districts,
                       region_logs_districts,
                       region_hr_districts)

region_factsheets_districts <-purrr::reduce(analysis_df_list, left_join)


## Overall actors district
actor_per_region <- actors_districts %>% select(region,district,name_agency)

district_per_region <- actor_per_region %>% group_by(region) %>% 
  summarize(hr_districts = paste(sort(unique(district)),collapse=", "))


actor_per_region <- actors_districts%>% group_by(region) %>% 
  summarize(actor_per_region = paste(sort(unique(name_agency)),collapse=", "))

num_overal_actors <- actors_districts %>% group_by(region) %>% summarise(num_overal_actors = n_distinct(name_agency))

## Factsheet datamerge final
analysis_df_list<-list(ug_regions,
                       num_overal_actors,
                       actor_per_region,
                       region_donors_list,
                       region_donors_count,
                       region_factsheets,
                       region_factsheets_names,
                       region_factsheets_figures,
                       region_factsheets_districts)

region_factsheets_datamerge <-purrr::reduce(analysis_df_list, left_join)

## Export file
list_of_datasets <- list("Contact info" = contact_details, 
                         "data_merge_file"= region_factsheets_datamerge,
                         "overview by partner" = df, 
                         "Pillars by actors" = actors_pillar_activity, 
                         "Coordination" = district_coord_activities,
                         "Infection prevention" = district_infe_activities, 
                         "Surveillance" = district_surve_activities, 
                         "Case management"= district_case_activities,
                         "Risk Communication" = district_risk_activities,
                         "Mental health" = district_mental_activities, 
                         "ICT" = district_ict_activities, 
                         "WASH" = district_WH_activities, 
                         "Logistics" = district_logs_activities, 
                         "HR" = district_hr_activities)

write.xlsx(list_of_datasets, file = paste0("outputs/UG_4W COVID_",today,".xlsx"))


## Aggregation at regional level - Unique number of actors
region_tot_actors <- actors_districts %>% group_by(region) %>% summarise(tot_actors = n_distinct(name_agency))
region_actors_names <- actors_districts %>% group_by(region) %>% summarize(actors_all = paste(sort(unique(name_agency)),collapse=", "))

region_actors <- left_join(region_tot_actors, region_actors_names)


## Create output
list_region_df <- list("Total actors" = region_actors,
                       "Coordination" = region_coordination,
                       "Infection prevention" = region_infection, 
                       "Surveillance" = region_surveillance, 
                       "Case management"= region_case_management,
                       "Risk Communication" = region_risk_comms,
                       "Mental health" = region_mental_health, 
                       "ICT" = region_ict, 
                       "WASH" = region_WH, 
                       "Logistics" = region_logistics, 
                       "HR" = region_hr)
  

write.xlsx(list_region_df, file = paste0("outputs/UG_4W COVID_regions_",today,".xlsx"))


## Aggregation at regional National - Unique number of actors
actors_districts$level <- "uganda"
coodination_actors$level <- "uganda"
infection_actors$level <- "uganda"
survey_actors$level <- "uganda"
case_actors$level <- "uganda"
WH_actors$level <- "uganda"
ict_actors$level <- "uganda"
mental_actors$level <- "uganda"
risk_actors$level <- "uganda"
logs_actors$level <- "uganda"
hr_actors$level <- "uganda"

## Aggregation at regional level - Unique number of actors
nat_tot_actors <- actors_districts %>% group_by(level) %>% summarise(tot_actors = n_distinct(name_agency))
nat_actors_names <- actors_districts %>% group_by(level) %>% summarize(actors_all = paste(sort(unique(name_agency)),collapse=", "))

nat_actors <- left_join(nat_tot_actors, nat_actors_names)

nat_coodination_actors <- coodination_actors %>% group_by(level) %>% summarise(coord_actors = n_distinct(name_agency))
nat_coodination_actors_names <- coodination_actors %>% group_by(level) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

nat_coord <- left_join(nat_coodination_actors,nat_coodination_actors_names)

nat_infe_actors <- infection_actors %>% group_by(level) %>% summarise(infe_actors = n_distinct(name_agency))
nat_infe_actors_names <- infection_actors %>% group_by(level) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

nat_infe <- left_join(nat_infe_actors, nat_infe_actors_names)

nat_survey_actors <- survey_actors %>% group_by(level) %>% summarise(survey_actors = n_distinct(name_agency))
nat_survey_actors_names <-survey_actors %>% group_by(level) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

nat_survey <- left_join(nat_survey_actors, nat_survey_actors_names)

nat_case_actors <- case_actors %>% group_by(level) %>% summarise(case_actors = n_distinct(name_agency))
nat_case_actors_names <- case_actors %>% group_by(level) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

nat_case <- left_join(nat_case_actors, nat_case_actors_names)

nat_WH_actors <- WH_actors %>% group_by(level) %>% summarise(WASH_actors = n_distinct(name_agency))
nat_WH_actors_names <- WH_actors %>% group_by(level) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

nat_WH <- left_join(nat_WH_actors, nat_WH_actors_names)

nat_ict_actors <- ict_actors %>% group_by(level) %>% summarise(ict_actors = n_distinct(name_agency))
nat_ict_actors_names <- ict_actors %>% group_by(level) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

nat_ict <- left_join(nat_ict_actors, nat_ict_actors_names)

nat_mental_actors <- mental_actors %>% group_by(level) %>% summarise(mental_actors = n_distinct(name_agency))
nat_mental_actors_names <- mental_actors %>% group_by(level) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

nat_mental <- left_join(nat_mental_actors, nat_mental_actors_names)

nat_risk_actors <- risk_actors %>% group_by(level) %>% summarise(risk_actors = n_distinct(name_agency))
nat_risk_actors_names <- risk_actors %>% group_by(level) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

nat_risk <- left_join(nat_risk_actors, nat_risk_actors_names)

nat_logs_actors <- logs_actors %>% group_by(level) %>% summarise(logs_actors = n_distinct(name_agency))
nat_logs_actors_names <- logs_actors %>% group_by(level) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

nat_logs <- left_join(nat_logs_actors, nat_logs_actors_names)

nat_hr_actors <- hr_actors %>% group_by(level) %>% summarise(hr_actors = n_distinct(name_agency))
nat_hr_actors_names <- hr_actors %>% group_by(level) %>% summarize(actors = paste(sort(unique(name_agency)),collapse=", "))

nat_hr <- left_join(nat_hr_actors, nat_hr_actors_names)

## Make output
all_for_nat <- list("Total actors" = nat_actors,
                    "Coordination" = nat_coord,
                    "Infection prevention" = nat_infe, 
                    "Surveillance" = nat_survey, 
                    "Case management"= nat_case,
                    "Risk Communication" = nat_risk,
                    "Mental health" = nat_mental, 
                    "ICT" = nat_ict, 
                    "WASH" = nat_WH, 
                    "Logistics" = nat_logs, 
                    "HR" = nat_hr)


write.xlsx(all_for_nat, file = paste0("outputs/UG_4W COVID_national_",today,".xlsx"))






