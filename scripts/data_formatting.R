library(readxl)
library(tidyverse)
library(butteR)
library(koboloadeR)
library(survey)
library(openxlsx)
library(lubridate)
library(sf)
library(srvyr)



#Load data------

#raw data
lst <- lapply(1:2, function(i) read_excel("inputs/impact_ug_ulearn_convid_4w_April2020.xlsx", sheet = i))

orgs <- lst[[1]]
activities <- lst[[2]]


colnames(orgs)<-gsub("\\/", ".", colnames(orgs))
colnames(activities)<-gsub("\\/", ".", colnames(activities))


#Xls form tool used
kobo_tool <- lapply(1:3, function(i) read_excel("inputs/tools/ULEARN_covid_response_4W_tool6APR2020.xlsx", sheet = i))
survey_sheet <- kobo_tool[[1]]
choices_sheet <- kobo_tool[[3]]


#Read in the cleaning log

cleaning_log <- read.csv("inputs/cleaning_log.csv")


#combine Orgs & activities----

#UUIDS that were test records
test_uuid <- c("54079d36-3bdd-415b-b38e-ebd1ac28419e","8f264159-abb4-4f3b-9a3a-b0d552c221af",
               "d47712cc-2705-458c-b115-ecb4e7721e29","8d4ebeb3-a5de-4a26-9958-14b95e6b8b60",
               "a9dca676-d89c-4366-8574-75f74ee8ee64", "d7fa94d3-a15c-440c-ac44-04c1edfd3062",	
               "a855df34-5b73-47db-bc2e-7c3c0458968f")

Test_records <- orgs %>%  filter((`_uuid` %in% test_uuid))


#make sure we only join the relevant cells with orgs info or have listed acvities

orgs <- orgs %>%  filter(!(`_uuid` %in% test_uuid)) %>% filter(!is.na(A.name_agency))
activities <- Filter(function(x)!all(is.na(x) ), activities)


#Join the activity list and INGO list


df <- left_join(orgs,activities, by = c("_index" = "_parent_index"))



#Data recoding----

#column headers of multiple select columns

df[df==FALSE]<- 0
df[df==TRUE]<- 1





#We drop the columns we don't need 

df <- df %>%  select(everything(),- (20:29) )


#Call in the column renaming script

source("scripts/rename_columns.R")


df$projects_uID <- paste0("ID-",df$project_id)


df <- df %>% filter(!is.na(project_id))

#Check cleaning log
# Reflect cleaning log------


cleaning_check <- butteR::check_cleaning_log(df = df,
                                             df_uuid = "projects_uID",
                                             cl = cleaning_log,
                                             cl_change_type_col ="change_type",
                                             cl_change_col = "indicator",
                                             cl_uuid = "uuid",
                                             cl_new_val = "new_value")


df_clean <- butteR::implement_cleaning_log(df = df,
                                            df_uuid = "projects_uID",
                                            cl = cleaning_log,
                                            cl_change_type_col ="change_type",
                                            cl_change_col = "indicator",
                                            cl_uuid = "uuid",
                                            cl_new_val = "new_value")






df <- df_clean


#overview by partner----
# we need the internal funding as we will add as part of the donor list

df$donor_internal_funds[df$B.projects.donor.internal_funds == 1] <- "internal_funding"



#After recoding we are able to remove the integer individual select columns from the dataset as this is just a summary

df_text <- df %>% select_if(is.character)
df_text_projects <- df_text %>% select(projects_uID,B.projects.donor:B.projects.other_activities,donor_internal_funds)

Org_contact <- df %>% select(projects_uID, name_agency:B.projects_count,name:B.projects.new_project_funding,B.projects.funding_amount, - contains("note"), - B.projects_num, -comments,-info_needs)


contact_details <- Org_contact %>% select(name_agency,name,position,email,phone) %>% distinct(email,.keep_all= TRUE)


#table join. Let us merge all these columns/fields into one database
analysis_df_list<-list(Org_contact,df_text_projects)

# settlement_joined<-purrr::reduce(analysis_df_list, left_join(by= c("D.info_state","D.info_county", "D.info_settlement")))
d.f <-purrr::reduce(analysis_df_list, left_join)





#Pillars list


d.f$Coordination <- "NA"
d.f$Coordination[d.f$B.projects.coordination != "none"] <- "Coordination"

d.f$Infection_prevention <- "NA"
d.f$Infection_prevention[d.f$B.projects.infection_prevention != "none"] <- "Infection prevention"

d.f$Surveillance <- "NA"
d.f$Surveillance[d.f$B.projects.surveillance != "none"] <- "Surveillance"

d.f$Case_management <- "NA"
d.f$Case_management[d.f$B.projects.case_management != "none"] <- "Case management"

d.f$WASH <- "NA"
d.f$WASH[d.f$B.projects.WH != "none"] <- "WASH"

d.f$ICT_innovations <- "NA"
d.f$ICT_innovations[d.f$B.projects.ict_innovation != "none"] <- "ICT innovations"


d.f$Mental_health <- "NA"
d.f$Mental_health[d.f$B.projects.mental_health_psychosocial_support != "none"] <- "Mental health"


d.f$Risk_communications <- "NA"
d.f$Risk_communications[d.f$B.projects.risk_communication != "none"] <- "Risk communications"


d.f$Logistics <- "NA"
d.f$Logistics[d.f$B.projects.logistics != "none"] <- "Logistics"


d.f$Human_resources <- "NA"
d.f$Human_resources[d.f$B.projects.human_resources != "none"] <- "Human resource"

#concatenated covered pillars per project

d.f$pillars <- paste(d.f$Coordination,d.f$Infection_prevention,d.f$Surveillance,d.f$Case_management,d.f$WASH,d.f$ICT_innovations,d.f$Mental_health,d.f$Risk_communications,d.f$Logistics,d.f$Human_resources, sep = ", ")


d.f <- d.f %>% mutate(pillars = str_remove_all(pillars,"NA,")) %>% mutate(pillars = str_remove_all(pillars,",NA")) %>% 
  mutate(pillars = str_replace(pillars, "NA", "other"))


#Concatenated activities per project

d.f$projects_activities <- paste(d.f$B.projects.coordination,d.f$B.projects.infection_prevention,d.f$B.projects.surveillance,d.f$B.projects.case_management,d.f$B.projects.WH,
                                 d.f$B.projects.ict_innovation,d.f$B.projects.mental_health_psychosocial_support,d.f$B.projects.risk_comms_support_form,d.f$B.projects.logistics,d.f$B.projects.human_resources, sep = ", ")


d.f <- d.f %>% mutate(projects_activities = str_replace(projects_activities, "none", "other")) %>% 
  mutate(projects_activities = str_remove_all(projects_activities,"NA,")) %>% mutate(projects_activities = str_remove_all(projects_activities,",NA"))






#Concatenated donors per project
donors <- d.f %>%  select(B.projects.foreign_donor:B.projects.other_donor, donor_internal_funds)
d.f$donors <-  apply(donors, 1, function(x) paste(na.omit(x),collapse=", ") )

donors_columns <- d.f %>%  select(B.projects.foreign_donor:B.projects.other_donor, donor_internal_funds) %>% colnames()
project_donor <- d.f %>%  select(projects_uID,donors_columns)

d.f$donors <- sapply(strsplit(d.f$donors, ",", fixed = TRUE), function(x) 
  paste(unique(x), collapse = ",")) 


d.f <- d.f %>% mutate(donors = str_remove_all(donors,",other")) %>% 
  mutate(donors = str_remove_all(donors,"other,"))

#Concatenated regions per project
regions <- d.f %>%  select(contains("region"))
d.f$regions <-  apply(regions, 1, function(x) paste(na.omit(x),collapse=", ") )

d.f$regions <- sapply(strsplit(d.f$regions, ",", fixed = TRUE), function(x) 
  paste(unique(x), collapse = ","))



#Concatenated activities per project
districts <- d.f %>%  select(contains("district"))
d.f$districts <-  apply(districts, 1, function(x) paste(na.omit(x),collapse=", ") ) 




#concatenate implimenting partners
implimenting_partner <- d.f %>%  select(contains("implimenting"))
d.f$implimenting_partner <-  apply(implimenting_partner, 1, function(x) paste(na.omit(x),collapse=", ") ) 






#d.f <- d.f %>% mutate(cov_districts = str_replace(districts, "", ","))
#d.f$districts = str_replace(', ',',',d.f$districts);

d.f$districts <- sapply(strsplit(d.f$districts, ",", fixed = TRUE), function(x) 
  paste(unique(x), collapse = ","))



#projects status
#d.f <- d.f %>%  mutate(Project_status = ifelse(A.movement_type=="exit","SouthSudan", D.prev_country))




d.f1 <- d.f %>%  select(projects_uID,name_agency,B.projects.project_title,donors,B.projects.project_status,B.projects.ongoing_ability,B.projects.new_project_funding,B.projects.funding_amount,pillars,projects_activities,B.projects.other_activities,implimenting_partner,regions,districts)


write.csv(d.f1,"outputs/UG_convid_matrix_30April2020.csv",na = "")





#Other responses------

others_responses <- d.f %>%  select(projects_uID, contains("other"))




#- overview by region (do you think we should aggregate subregions to region level - central, western, southern, eastern, west nile): 
#number of pillars being support, number of partners active, estimated value of support







#Pillars detail----

#list of pillars


df$Coordination <- 0
df$Coordination[df$B.projects.coordination != "none"] <- 1

df$Infection_prevention <- 0
df$Infection_prevention[df$B.projects.infection_prevention != "none"] <- 1

df$Surveillance <- 0
df$Surveillance[df$B.projects.surveillance != "none"] <- 1

df$Case_management <- 0
df$Case_management[df$B.projects.case_management != "none"] <- 1

df$WASH <- 0
df$WASH[df$B.projects.WH != "none"] <- 1

df$ICT_innovations <- 0
df$ICT_innovations[df$B.projects.ict_innovation != "none"] <- 1


df$Mental_health <- 0
df$Mental_health[df$B.projects.mental_health_psychosocial_support != "none"] <- 1


df$Risk_communications <- 0
df$Risk_communications[df$B.projects.risk_communication != "none"] <- 1


df$Logistics <- 0
df$Logistics[df$B.projects.logistics != "none"] <- 1


df$Human_resources <- 0
df$Human_resources[df$B.projects.human_resources != "none"] <- 1

coord_activities_columns <- df %>%  select(contains("B.projects.coordination."), - contains("none")) 
coord_activities <-df %>%  select(projects_uID,name_agency,Coordination ,contains("B.projects.coordination."), - contains("none")) %>% mutate( coord_activities_num = rowSums(coord_activities_columns, na.rm = TRUE))

infe_activities_columns <- df %>%  select(contains("B.projects.infection_prevention."), - contains("none")) 
infe_activities <-df %>%  select(projects_uID,name_agency,Infection_prevention ,contains("B.projects.infection_prevention."), - contains("none")) %>% mutate( infeactivities_num = rowSums(infe_activities_columns, na.rm = TRUE))

surveilance_activities_columns <- df %>%  select(contains("B.projects.surveillance."), - contains("none")) 
surveilance_activities <-df %>%  select(projects_uID,name_agency,Surveillance ,contains("B.projects.surveillance."), - contains("none")) %>% mutate( surve_activities_num = rowSums(surveilance_activities_columns, na.rm = TRUE))


casemanagement_activities_columns <- df %>%  select(contains("B.projects.case_management."), - contains("none")) 
casemanagement_activities <-df %>%  select(projects_uID,name_agency,Case_management ,contains("B.projects.case_management."), - contains("none")) %>% mutate( case_activities_num = rowSums(casemanagement_activities_columns, na.rm = TRUE))

WASH_activities_columns <- df %>%  select(contains("B.projects.WH."), - contains("none")) 
WASH_activities <-df %>%  select(projects_uID,name_agency,WASH ,contains("B.projects.WH."), - contains("none")) %>% mutate( WASH_activities_num = rowSums(WASH_activities_columns, na.rm = TRUE))


ict_columns <- df %>%  select(contains("B.projects.ict_innovation."), - contains("none")) 
ict_activities <-df %>%  select(projects_uID,name_agency,ICT_innovations ,contains("B.projects.ict_innovation."), - contains("none")) %>% mutate( ict_activities_num = rowSums(ict_columns, na.rm = TRUE))

mental_activities_columns <- df %>%  select(contains("B.projects.mental_health_psychosocial_support."), - contains("none")) 
mental_activities <-df %>%  select(projects_uID,name_agency,Mental_health ,contains("B.projects.mental_health_psychosocial_support."), - contains("none")) %>% mutate( mental_health_activities_num = rowSums(mental_activities_columns, na.rm = TRUE))


risk_activities_columns <- df %>%  select(contains("B.projects.risk_communication."), - contains("none")) 
risk_activities <-df %>%  select(projects_uID,name_agency,Risk_communications ,contains("B.projects.risk_communication."), - contains("none")) %>% mutate( risk_activities_num = rowSums(risk_activities_columns, na.rm = TRUE))

logistics_activities_columns <- df %>%  select(contains("B.projects.logistics."), - contains("none")) 
logistics_activities <-df %>%  select(projects_uID,name_agency,Logistics ,contains("B.projects.logistics."), - contains("none")) %>% mutate( logistics_activities_num = rowSums(logistics_activities_columns, na.rm = TRUE))

HR_activities_columns <- df %>%  select(contains("B.projects.human_resources."), - contains("none")) 
HR_activities <-df %>%  select(projects_uID,name_agency,Human_resources ,contains("B.projects.human_resources."), - contains("none")) %>% mutate( HR_activities_num = rowSums(HR_activities_columns, na.rm = TRUE))


#Support form----

support_form_actors <- df %>% select( B.projects.ongoing_ability,B.projects.new_project_funding,name_agency, B.projects.coordination_support_cash_amount, B.projects.coordination_support_supplies_amount,
                                      B.projects.infection_support_cash_amount, B.projects.infection_support_supplies_amount,
                                      B.projects.surveillance_support_supplies_amount,
                                      B.projects.case_management_support_cash_amount, B.projects.case_management_support_supplies_amount,
                                      B.projects.wash_support_cash_amount,  B.projects.wash_support_supplies_amount,
                                      B.projects.ict_support_cash_amount,
                                      B.projects.mental_health_support_cash_amount, B.projects.mental_health_support_supplies_amount,
                                      B.projects.risk_comms_support_cash_amount, B.projects.risk_comms_support_supplies_amount,
                                      B.projects.logs_support_supplies_amount,
                                      B.projects.human_resources_support_cash_amount) %>% 
  mutate(confirmed_funding = ifelse(is.na(B.projects.ongoing_ability),B.projects.new_project_funding,B.projects.ongoing_ability)) %>%
  filter(confirmed_funding == "yes") %>%  
  select(name_agency:B.projects.human_resources_support_cash_amount)


#table join. Let us merge all these columns/fields into one database
analysis_df_list<-list(coord_activities,infe_activities,surveilance_activities,casemanagement_activities,WASH_activities,
                       ict_activities,mental_activities,risk_activities,logistics_activities, HR_activities,support_form_actors)

projects_activities <-purrr::reduce(analysis_df_list, left_join)


actors_pillar_activity <- projects_activities %>%  
  select(name_agency ,Coordination,Infection_prevention,Surveillance,Case_management,WASH,ICT_innovations,Mental_health,	Risk_communications,	Logistics,	Human_resources,B.projects.coordination_support_cash_amount, B.projects.coordination_support_supplies_amount,
         B.projects.infection_support_cash_amount, B.projects.infection_support_supplies_amount,
         B.projects.surveillance_support_supplies_amount,
         B.projects.case_management_support_cash_amount, B.projects.case_management_support_supplies_amount,
         B.projects.wash_support_cash_amount,  B.projects.wash_support_supplies_amount,
         B.projects.ict_support_cash_amount,
         B.projects.mental_health_support_cash_amount, B.projects.mental_health_support_supplies_amount,
         B.projects.risk_comms_support_cash_amount, B.projects.risk_comms_support_supplies_amount,
         B.projects.logs_support_supplies_amount,
         B.projects.human_resources_support_cash_amount) %>% 
  group_by(name_agency) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))

#Project actor ID


actor_project_id <- d.f1 %>%  select(projects_uID,name_agency)

#create an ID that we can use to connect our integer and character select multiple data frames

Project_ids <- df %>%  select(project_id,projects_uID) %>% 
  group_by(projects_uID)




#District info ----

#Individual select (0/1 integers) multiple columns for reshaping

sel_mul <- butteR::auto_detect_select_multiple(df)

df_mul_col <- df %>% select(project_id,contains(sel_mul)) %>% select_if(is.numeric)


aggreg_to <- df_mul_col %>%  select(everything(),- project_id) %>% colnames()


multiple_select_data  <- df_mul_col %>% pivot_longer(aggreg_to,
                                                     names_to="donor_type",
                                                     values_to="indicator_val")







#create an ID that we can use to connect our integer and character select multiple data frames

Project_ids <- df %>%  select(project_id,projects_uID) %>% 
  group_by(projects_uID)



#all activity areas districts

list <- "B.projects.district_activity_area_"

df_area_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>% 
  mutate(donor_type = str_remove_all(donor_type,"B.projects.district_activity_area_")) %>% 
  separate(donor_type,c("area_regions", "area_districts")) %>% 
  select(project_id,area_regions, area_districts) 


#Coordination districts----

list <- "B.projects.b1.district_coordination."

df_coord_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>% 
  mutate(donor_type = str_remove_all(donor_type,"B.projects.b1.district_coordination_")) %>% 
  mutate(donor_type = str_replace(donor_type, "B.projects.b1.district_coordination.", "acholi.")) %>% 
  separate(donor_type,c("Coord_regions", "Coord_districts")) %>% 
  select(project_id,Coord_regions, Coord_districts) 


analysis_df_list<-list(Project_ids,df_area_district,df_coord_district,coord_activities)

district_coord_activities <-purrr::reduce(analysis_df_list, left_join)


district_coord_activities <- district_coord_activities %>%  
  mutate(region = ifelse(is.na(Coord_regions),area_regions,Coord_regions),
         district =  ifelse(is.na(Coord_districts),area_districts,Coord_districts)) %>% 
  filter(Coordination == 1 & !is.na(district) ) %>%  select(projects_uID,region,district,Coordination :coord_activities_num)



list <-list(district_coord_activities,actor_project_id)

coodination_actors <-purrr::reduce(list, left_join)

#Factsheets-----

region_coodination_actors <- coodination_actors %>% group_by(region) %>% summarise(coord_actors = n_distinct(name_agency))




coord_actors_locations <-coodination_actors %>% group_by(district) %>% 
  summarize(actors = paste(sort(unique(name_agency)),collapse=", "))



district_coord_activities <- district_coord_activities[,c(2:16)]

district_coord_activities <- district_coord_activities %>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))


list<-list(district_coord_activities,coord_actors_locations)

district_coord_activities <-purrr::reduce(list, left_join)


#Actors_in_a_region_by acivity





#infe districts -----
#"B.projects.b2.district_infection_prevention."

list <- "B.projects.b2.district_infection_prevention."


df_infe_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>%
  mutate(donor_type = str_remove_all(donor_type,"B.projects.b2.district_infection_prevention_")) %>%
  mutate(donor_type = str_replace(donor_type, "B.projects.b2.district_infection_prevention.", "acholi.")) %>%
  separate(donor_type,c("Infection_prevention_regions", "Infection_prevention_districts")) %>%
  select(project_id,Infection_prevention_regions,Infection_prevention_districts)


infe_support <- df %>% select(projects_uID,B.projects.infection_support_cash_amount,B.projects.infection_support_supplies_amount)


analysis_df_list<-list(Project_ids,df_area_district,df_infe_district,infe_activities,infe_support)

district_infe_activities <-purrr::reduce(analysis_df_list, left_join)




district_infe_activities <- district_infe_activities %>%  
  mutate(region = ifelse(is.na(Infection_prevention_regions),area_regions,Infection_prevention_regions),
         district =  ifelse(is.na(Infection_prevention_districts),area_districts,Infection_prevention_districts)) %>% 
  filter(Infection_prevention == 1 & !is.na(district)  ) %>%  select(region,district,Infection_prevention:B.projects.infection_support_supplies_amount)


list <-list(district_infe_activities,actor_project_id)

infection_actors <-purrr::reduce(list, left_join)


#Factsheets-----
region_infe_actors <- infection_actors %>% group_by(region) %>% summarise(infe_actors = n_distinct(name_agency))



infection_actors_locations <-infection_actors %>% group_by(district) %>% 
  summarize(actors = paste(sort(unique(name_agency)),collapse=", "))




district_infe_activities <- district_infe_activities[,2:12]

district_infe_activities <- district_infe_activities %>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))




list<-list(district_infe_activities,infection_actors_locations)

district_infe_activities <-purrr::reduce(list, left_join)




#Survey districts-----
#"B.projects.b3.district_surveillance"

list <- "B.projects.b3.district_surveillance."

df_surv_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>%
  mutate(donor_type = str_remove_all(donor_type,"B.projects.b3.district_surveillance_")) %>%
  mutate(donor_type = str_replace(donor_type, "B.projects.b3.district_surveillance.", "acholi.")) %>%
  separate(donor_type,c("surveilance_regions", "surveilance_districts"))%>%
  select(project_id,surveilance_regions,surveilance_districts)



surveilance_support <- df %>% select(projects_uID,B.projects.surveillance_support_supplies_amount)


analysis_df_list<-list(Project_ids,df_area_district,df_surv_district,surveilance_activities,surveilance_support)

district_surve_activities <-purrr::reduce(analysis_df_list, left_join)

district_surve_activities <- district_surve_activities %>%  
  mutate(region = ifelse(is.na(surveilance_regions),area_regions,surveilance_regions),
         district =  ifelse(is.na(surveilance_districts),area_districts,surveilance_districts)) %>% 
  filter(Surveillance == 1 & !is.na(district)  ) %>%  select(projects_uID,region,district,Surveillance:B.projects.surveillance_support_supplies_amount)


list <-list(district_surve_activities,actor_project_id)

survey_actors <-purrr::reduce(list, left_join)


#Factsheets-----
region_survey_actors <- survey_actors %>% group_by(region) %>% summarise(survey_actors = n_distinct(name_agency))



survey_actors_locations <-survey_actors %>% group_by(district) %>% 
  summarize(actors = paste(sort(unique(name_agency)),collapse=", "))




district_surve_activities <- district_surve_activities[,2:17]

district_surve_activities <- district_surve_activities%>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))



list<-list(district_surve_activities,survey_actors_locations)

district_surve_activities <-purrr::reduce(list, left_join)



#case management district------
#"B.projects.b4.district_case_management."

list <- "B.projects.b4.district_case_management."

df_case_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>%
  mutate(donor_type = str_remove_all(donor_type,"B.projects.b4.district_case_management_")) %>%
  mutate(donor_type = str_replace(donor_type, "B.projects.b4.district_case_management.", "acholi.")) %>%
  separate(donor_type,c("case_management_regions", "case_mangement_districts")) %>%
  select(project_id,case_management_regions,case_mangement_districts)


case_support <- df %>% select(projects_uID,B.projects.case_management_support_cash_amount,B.projects.case_management_support_supplies_amount)

analysis_df_list<-list(Project_ids,df_area_district,df_case_district,casemanagement_activities,case_support)

district_case_activities <-purrr::reduce(analysis_df_list, left_join)


district_case_activities <- district_case_activities %>%  
  mutate(region = ifelse(is.na(case_management_regions),area_regions,case_management_regions),
         district =  ifelse(is.na(case_mangement_districts),area_districts,case_mangement_districts)) %>% 
  filter(Case_management == 1 & !is.na(district)  ) %>%  select(projects_uID,region,district,Case_management:B.projects.case_management_support_supplies_amount)



list <-list(district_case_activities,actor_project_id)

case_actors <-purrr::reduce(list, left_join)

#Factsheets-----
region_case_actorss <- case_actors %>% group_by(region) %>% summarise(case_actors = n_distinct(name_agency))



case_actors_locations <-case_actors %>% group_by(district) %>% 
  summarize(actors = paste(sort(unique(name_agency)),collapse=", "))






district_case_activities <- district_case_activities[,2:18]

district_case_activities <- district_case_activities %>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))



list<-list(district_case_activities,case_actors_locations)

district_case_activities <-purrr::reduce(list, left_join)






#WASH districts-----

#"B.projects.b5.district_WH."

list <- "B.projects.b5.district_WH."

df_WH_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>%
  mutate(donor_type = str_remove_all(donor_type,"B.projects.b5.district_WH_")) %>%
  mutate(donor_type = str_replace(donor_type, "B.projects.b5.district_WH.", "acholi.")) %>%
  separate(donor_type,c("WH_regions", "WH_districts")) %>%
  select(project_id,WH_regions,WH_districts)

WH_support <- df %>% select(projects_uID,B.projects.wash_support_cash_amount,B.projects.wash_support_supplies_amount)

analysis_df_list<-list(Project_ids,df_area_district,df_WH_district,WASH_activities,WH_support)

district_WH_activities <-purrr::reduce(analysis_df_list, left_join)

district_WH_activities <- district_WH_activities %>%  
  mutate(region = ifelse(is.na(WH_regions),area_regions,WH_regions),
         district =  ifelse(is.na(WH_districts),area_districts,WH_districts)) %>% 
  filter(WASH == 1 & !is.na(district)  ) %>%  select(projects_uID,region,district,WASH:B.projects.wash_support_supplies_amount)

list <-list(district_WH_activities,actor_project_id)

WH_actors <-purrr::reduce(list, left_join)

#Factsheets-----
region_WH_actors <- WH_actors %>% group_by(region) %>% summarise(WASH_actors = n_distinct(name_agency))



WH_actors_locations <- WH_actors %>% group_by(district) %>% 
  summarize(actors = paste(sort(unique(name_agency)),collapse=", "))


district_WH_activities <- district_WH_activities[,2:13]

district_WH_activities <- district_WH_activities %>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))


list<-list(district_WH_activities,WH_actors_locations)
district_WH_activities <-purrr::reduce(list, left_join)


#ict district----
#"B.projects.b6.district_ict_innovation."

list <- "B.projects.b6.district_ict_innovation."

df_ICT_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>%
  mutate(donor_type = str_remove_all(donor_type,"B.projects.b6.district_ict_innovation_")) %>%
  mutate(donor_type = str_replace(donor_type, "B.projects.b6.district_ict_innovation.", "acholi.")) %>%
  separate(donor_type,c("ict_regions", "ict_districts")) %>%
  select(project_id,ict_regions,ict_districts)

ict_support <- df %>% select(projects_uID,B.projects.ict_support_cash_amount)

analysis_df_list<-list(Project_ids,df_area_district,df_ICT_district,ict_activities,ict_support)

district_ict_activities <-purrr::reduce(analysis_df_list, left_join)


district_ict_activities <- district_ict_activities %>%  
  mutate(region = ifelse(is.na(ict_regions),area_regions,ict_regions),
         district =  ifelse(is.na(ict_districts),area_districts,ict_districts)) %>% 
  filter(ICT_innovations == 1 & !is.na(district)  ) %>%  select(projects_uID,region,district,ICT_innovations:B.projects.ict_support_cash_amount)


list <-list(district_ict_activities,actor_project_id)

ict_actors <-purrr::reduce(list, left_join)



#Factsheets-----
region_ict_actors <- ict_actors %>% group_by(region) %>% summarise(ict_actors = n_distinct(name_agency))


ict_actors_locations <- ict_actors %>% group_by(district) %>% 
  summarize(actors = paste(sort(unique(name_agency)),collapse=", "))





district_ict_activities <- district_ict_activities[,2:19]

district_ict_activities <- district_ict_activities %>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))


list<-list(district_ict_activities,ict_actors_locations)
district_ict_activities <-purrr::reduce(list, left_join)


#mental districts----
#"B.projects.b7.district_mental_health_psychosocial_support."

list <- "B.projects.b7.district_mental_health_psychosocial_support."

df_mental_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>%
  mutate(donor_type = str_remove_all(donor_type,"B.projects.b7.district_mental_health_psychosocial_support_")) %>%
  mutate(donor_type = str_replace(donor_type, "B.projects.b7.district_mental_health_psychosocial_support.", "acholi.")) %>%
  separate(donor_type,c("mental_regions", "mental_districts")) %>%
  select(project_id,mental_regions,mental_districts)


mental_support <- df %>% select(projects_uID,B.projects.mental_health_support_cash_amount,B.projects.mental_health_support_supplies_amount)

analysis_df_list<-list(Project_ids,df_area_district,df_mental_district,mental_activities,mental_support)


district_mental_activities <-purrr::reduce(analysis_df_list, left_join)


district_mental_activities <- district_mental_activities %>%  
  mutate(region = ifelse(is.na(mental_regions),area_regions,mental_regions),
         district =  ifelse(is.na(mental_districts),area_districts,mental_districts)) %>% 
  filter(Mental_health == 1 & !is.na(district)  ) %>%  select(projects_uID,region,district,Mental_health:B.projects.mental_health_support_supplies_amount)




list <-list(district_mental_activities,actor_project_id)

mental_actors <-purrr::reduce(list, left_join)

#Factsheets-----
region_mental_actors <- mental_actors %>% group_by(region) %>% summarise(mental_actors = n_distinct(name_agency))


mental_actors_locations <- mental_actors %>% group_by(district) %>% 
  summarize(actors = paste(sort(unique(name_agency)),collapse=", "))



district_mental_activities <- district_mental_activities[,2:11]

district_mental_activities <- district_mental_activities %>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))



list<-list(district_mental_activities,mental_actors_locations)
district_mental_activities <-purrr::reduce(list, left_join)

#risk districts----
#"	B.projects.b8.district_risk_communication."

list <- "B.projects.b8.district_risk_communication."

df_risk_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>%
  mutate(donor_type = str_remove_all(donor_type,"B.projects.b8.district_risk_communication_")) %>%
  mutate(donor_type = str_replace(donor_type, "B.projects.b8.district_risk_communication.", "acholi.")) %>%
  separate(donor_type,c("risk_comm_regions", "risk_comm_districts")) %>%
  select(project_id,risk_comm_regions,risk_comm_districts)


riskcomm_support <- df %>% select(projects_uID,B.projects.risk_comms_support_cash_amount,B.projects.risk_comms_support_supplies_amount)


analysis_df_list<-list(Project_ids,df_area_district,df_risk_district,risk_activities,riskcomm_support)

district_risk_activities <-purrr::reduce(analysis_df_list, left_join)



district_risk_activities <- district_risk_activities %>%  
  mutate(region = ifelse(is.na(risk_comm_regions),area_regions,risk_comm_regions),
         district =  ifelse(is.na(risk_comm_districts),area_districts,risk_comm_districts)) %>% 
  filter(Risk_communications == 1 & !is.na(district)  ) %>%  select(projects_uID,region, district,Risk_communications:B.projects.risk_comms_support_supplies_amount)





list <-list(district_risk_activities,actor_project_id)

risk_actors <-purrr::reduce(list, left_join)


#Factsheets-----
region_risk_actors <- risk_actors %>% group_by(region) %>% summarise(risk_actors = n_distinct(name_agency))


risk_actors_locations <- risk_actors %>% group_by(district) %>% 
  summarize(actors = paste(sort(unique(name_agency)),collapse=", "))



district_risk_activities <- district_risk_activities[,2:23]

district_risk_activities <- district_risk_activities %>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))



list<-list(district_risk_activities,risk_actors_locations)
district_risk_activities <-purrr::reduce(list, left_join)

#logistics districts----
#"	B.projects.b9.district_logistics."

list <- "B.projects.b9.district_logistics."

df_logs_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>%
  mutate(donor_type = str_remove_all(donor_type,"B.projects.b9.district_logistics_")) %>%
  mutate(donor_type = str_replace(donor_type, "B.projects.b9.district_logistics.", "acholi.")) %>%
  separate(donor_type,c("logs_regions", "logs_districts")) %>%
  select(project_id,logs_regions,logs_districts)

logs_support <- df %>% select(projects_uID,B.projects.logs_support_supplies_amount)




analysis_df_list<-list(Project_ids,df_area_district,df_logs_district,logistics_activities,logs_support)

district_logs_activities <-purrr::reduce(analysis_df_list, left_join)

district_logs_activities <- district_logs_activities %>%  
  mutate(region = ifelse(is.na(logs_regions),area_regions,logs_regions),
         district =  ifelse(is.na(logs_districts),area_districts,logs_districts)) %>% 
  filter(Logistics == 1 & !is.na(district)  ) %>%  select(projects_uID,region,district,Logistics:B.projects.logs_support_supplies_amount)



list <-list(district_logs_activities,actor_project_id)

logs_actors <-purrr::reduce(list, left_join)

#Factsheets-----
region_logs_actors <- logs_actors %>% group_by(region) %>% summarise(logs_actors = n_distinct(name_agency))



logs_actors_locations <- logs_actors %>% group_by(district) %>% 
  summarize(actors = paste(sort(unique(name_agency)),collapse=", "))



district_logs_activities <- district_logs_activities[,2:54]

district_logs_activities <- district_logs_activities %>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))



list<-list(district_logs_activities,logs_actors_locations)
district_logs_activities <-purrr::reduce(list, left_join)


#HR districts----
#"B.projects.b10.district_human_resources"

list <- "B.projects.b10.district_human_resources."

df_HR_district <- filter(multiple_select_data, str_detect(donor_type, paste(list, collapse="|"))) %>% filter(indicator_val == 1  ) %>%
  mutate(donor_type = str_remove_all(donor_type,"B.projects.b10.district_human_resources_")) %>%
  mutate(donor_type = str_replace(donor_type, "B.projects.b10.district_human_resources.", "acholi.")) %>%
  separate(donor_type,c("HR_regions", "HR_districts")) %>%
  select(project_id,HR_regions,HR_districts)


HR_support <- df %>% select(projects_uID,B.projects.human_resources_support_cash_amount)


analysis_df_list<-list(Project_ids,df_area_district,df_HR_district,HR_activities,HR_support)

district_hr_activities <-purrr::reduce(analysis_df_list, left_join)




district_hr_activities <- district_hr_activities %>%  
  mutate(region = ifelse(is.na(HR_regions),area_regions,HR_regions),
         district =  ifelse(is.na(HR_districts),area_districts,HR_districts)) %>% 
  filter(Human_resources == 1 & !is.na(district)) %>%  select(projects_uID,region,district,Human_resources:B.projects.human_resources_support_cash_amount)




list <-list(district_hr_activities,actor_project_id)

hr_actors <-purrr::reduce(list, left_join)


#Factsheets-----
region_hr_actors <- hr_actors %>% group_by(region) %>% summarise(hr_actors = n_distinct(name_agency))


hr_actors_locations <- hr_actors %>% group_by(district) %>% 
  summarize(actors = paste(sort(unique(name_agency)),collapse=", "))



district_hr_activities <- district_hr_activities[,2:17]

district_hr_activities <- district_hr_activities %>%  
  group_by(region,district) %>% 
  summarise_all(funs(sum(., na.rm = TRUE)))



list<-list(district_hr_activities,hr_actors_locations)
district_hr_activities <-purrr::reduce(list, left_join)





#analysis_df_list<-list(district_coordination_activities,Infection_prevention_activities,district_surve_activities1,district_case_activities1,
#                    district_WH_activities1,district_ict_activities1,district_mental_activities1,district_mental_activities1,
 #                   district_logs_activities1,district_hr_activities1)

#Activities_by_district <-purrr::reduce(analysis_df_list, left_join)




#Uganda regions and districts info



admin_info <- choices_sheet %>% slice(156:307) %>%  filter(!is.na(list_name))



#districts list
#ug_districts <- admin_info %>%  filter(list_name != "region_list") %>%  select(name:region) %>%  rename("region_join"=region, "district_label"= label, "district_join"= name)

ug_regions <-  admin_info %>%  filter(list_name == "region_list") %>%  select(name, label) %>%  rename("region"= name, "region_label"= label)


#Factsheets  data merge----
analysis_df_list<-list(region_coodination_actors,region_infe_actors,region_survey_actors,region_case_actorss,region_WH_actors,region_ict_actors,
                       region_risk_actors,region_mental_actors,region_logs_actors,region_hr_actors)

Region_factsheets <-purrr::reduce(analysis_df_list, left_join)


colnames(Region_factsheets)<-paste0("num_",colnames(Region_factsheets))

Region_factsheets <- Region_factsheets %>% rename( "region"=num_region)

#actors maping----------

Actors_districts <- bind_rows(coodination_actors,infection_actors,case_actors,survey_actors,WH_actors,ict_actors,risk_actors,mental_actors,logs_actors,hr_actors)

#Donors districts---------

analysis_df_list<-list(Actors_districts,project_donor)

project_region_donor <-purrr::reduce(analysis_df_list, left_join)


project_region_donor <- project_region_donor %>% select(region,donors_columns)

donors_regions  <- project_region_donor %>% pivot_longer(donors_columns,
                                                     names_to="donor_type",
                                                     values_to="indicator_val")



region_donors <- donors_regions %>% filter(!is.na(indicator_val) )

region_donors_list <-region_donors %>% group_by(region) %>% 
  summarize(region_donor = paste(sort(unique(indicator_val)),collapse=" | "))

region_donors_count <- region_donors[,2:4]
region_donors_count <- region_donors_count %>% group_by(region) %>% summarise(donors_count = n_distinct(indicator_val))


#coord actor district------
coodination_actors <- Actors_districts %>% filter(Coordination == 1) %>% select(region,district,name_agency)


Actors_districts_sum <- Actors_districts[,17]

#total actors----
actors_total <- Actors_districts_sum  %>% summarize(actors_all = paste(sort(unique(name_agency)),collapse=", "))

num_actors_total <- Actors_districts_sum %>% summarise(num_actors_total = n_distinct(name_agency))



region_coodination_districts <-coodination_actors %>% group_by(region) %>% 
  summarize(coord_districts = paste(sort(unique(district)),collapse=", "))


region_coodination_actors <-coodination_actors %>% group_by(region) %>% 
  summarize(coord_actors = paste(sort(unique(name_agency)),collapse=", "))

#all coordination actors----
region_coodination_actors <-coodination_actors %>% group_by(region) %>% 
  summarize(coord_actors = paste(sort(unique(name_agency)),collapse=", "))


all_coodination_actors <- coodination_actors[,4]
num_coodination_actors <- coodination_actors[,4]

all_coodination_actors <-all_coodination_actors %>%  
  summarize(coord_actors = paste(sort(unique(name_agency)),collapse=", "))

num_total_coord_actors <- num_coodination_actors %>%  summarise(num_total_cord_actors = n_distinct(name_agency))



num_cord_districts <- coodination_actors %>% group_by(region) %>% summarise(num_cord_districts = n_distinct(district))


#infe actor district------
infection_actors <- Actors_districts %>% filter(Infection_prevention == 1) %>% select(region,district,name_agency)

region_infe_districts <-infection_actors %>% group_by(region) %>% 
  summarize(infection_districts = paste(sort(unique(district)),collapse=", "))


region_infe_actors <-infection_actors %>% group_by(region) %>% 
  summarize(infe_actors = paste(sort(unique(name_agency)),collapse=", "))


num_infe_districts <- infection_actors %>% group_by(region) %>% summarise(num_infe_districts = n_distinct(district))


#survey actor district------
survey_actors <- Actors_districts %>% filter(Surveillance == 1) %>% select(region,district,name_agency)

region_sur_districts <-survey_actors %>% group_by(region) %>% 
  summarize(survey_districts = paste(sort(unique(district)),collapse=", "))


region_sur_actors <-survey_actors %>% group_by(region) %>% 
  summarize(survey_actors = paste(sort(unique(name_agency)),collapse=", "))


num_survey_districts <- survey_actors %>% group_by(region) %>% summarise(num_survey_districts = n_distinct(district))



#case actor district------
case_actors <- Actors_districts %>% filter(Case_management == 1) %>% select(region,district,name_agency)

region_case_districts <-case_actors %>% group_by(region) %>% 
  summarize(case_districts = paste(sort(unique(district)),collapse=", "))


region_case_actors <-case_actors %>% group_by(region) %>% 
  summarize(case_actors = paste(sort(unique(name_agency)),collapse=", "))

num_case_districts <- case_actors %>% group_by(region) %>% summarise(num_case_districts = n_distinct(district))



#WASH actor district------
WH_actors <- Actors_districts %>% filter(WASH == 1) %>% select(region,district,name_agency)

region_WH_districts <-WH_actors %>% group_by(region) %>% 
  summarize(wh_districts = paste(sort(unique(district)),collapse=", "))


region_WH_actors <-WH_actors %>% group_by(region) %>% 
  summarize(wh_actors = paste(sort(unique(name_agency)),collapse=", "))


num_WH_districts <- WH_actors %>% group_by(region) %>% summarise(num_wh_districts = n_distinct(district))

#ict_actor district------
ict_actors <- Actors_districts %>% filter(ICT_innovations == 1) %>% select(region,district,name_agency)

region_ict_districts <-ict_actors %>% group_by(region) %>% 
  summarize(ict_districts = paste(sort(unique(district)),collapse=", "))


region_ict_actors <-ict_actors %>% group_by(region) %>% 
  summarize(ict_actors = paste(sort(unique(name_agency)),collapse=", "))


num_ict_districts <- ict_actors %>% group_by(region) %>% summarise(ict_wh_districts = n_distinct(district))



#Risk actor district------
risk_actors <- Actors_districts %>% filter(Risk_communications == 1) %>% select(region,district,name_agency)

region_risk_districts <-district_risk_activities %>% group_by(region) %>% 
  summarize(risk_districts = paste(sort(unique(district)),collapse=", "))


region_risk_actors <-risk_actors %>% group_by(region) %>% 
  summarize(risk_actors = paste(sort(unique(name_agency)),collapse=", "))

num_risk_districts <- district_risk_activities %>% group_by(region) %>% summarise(num_risk_districts = n_distinct(district))


#mental actor district------
mental_actors <- Actors_districts %>% filter(Mental_health == 1) %>% select(region,district,name_agency)

region_mental_districts <-mental_actors %>% group_by(region) %>% 
  summarize(mental_districts = paste(sort(unique(district)),collapse=", "))


region_mental_actors <-mental_actors %>% group_by(region) %>% 
  summarize(mental_actors = paste(sort(unique(name_agency)),collapse=", "))

num_mental_districts <- mental_actors %>% group_by(region) %>% summarise(num_mental_districts = n_distinct(district))


#logs actor district------
logs_actors <- Actors_districts %>% filter(Logistics == 1) %>% select(region,district,name_agency)

region_logs_districts <-logs_actors %>% group_by(region) %>% 
  summarize(logs_districts = paste(sort(unique(district)),collapse=", "))


region_logs_actors <-logs_actors %>% group_by(region) %>% 
  summarize(logs_actors = paste(sort(unique(name_agency)),collapse=", "))

num_logs_districts <- logs_actors %>% group_by(region) %>% summarise(num_logs_districts = n_distinct(district))



#HR actor district------
hr_actors <- Actors_districts %>% filter(Human_resources == 1) %>% select(region,district,name_agency)

region_hr_districts <-hr_actors %>% group_by(region) %>% 
  summarize(hr_districts = paste(sort(unique(district)),collapse=", "))


region_hr_actors <-hr_actors %>% group_by(region) %>% 
  summarize(hr_actors = paste(sort(unique(name_agency)),collapse=", "))

num_hr_districts <- hr_actors %>% group_by(region) %>% summarise(num_hr_districts = n_distinct(district))



#Factsheets  data merge 2----
analysis_df_list<-list(region_coodination_actors,region_infe_actors,region_sur_actors,region_case_actors,region_WH_actors,region_ict_actors,
                       region_risk_actors,region_mental_actors,region_logs_actors,region_hr_actors)

Region_factsheets2 <-purrr::reduce(analysis_df_list, left_join)


analysis_df_list<-list(num_cord_districts,num_survey_districts,num_case_districts,num_WH_districts,num_ict_districts,num_risk_districts,
                       num_mental_districts,num_logs_districts,num_hr_districts)

Region_factsheets3 <-purrr::reduce(analysis_df_list, left_join)


analysis_df_list<-list(region_coodination_districts,region_sur_districts,region_infe_districts,region_case_districts,region_WH_districts,region_ict_districts,
                        region_risk_districts,region_mental_districts,region_logs_districts,region_hr_districts)

Region_factsheets4 <-purrr::reduce(analysis_df_list, left_join)







#overal actors district------
actor_per_region <- Actors_districts %>% select(region,district,name_agency)

district_per_region <-actor_per_region %>% group_by(region) %>% 
  summarize(hr_districts = paste(sort(unique(district)),collapse=", "))


actor_per_region <- Actors_districts%>% group_by(region) %>% 
  summarize(actor_per_region = paste(sort(unique(name_agency)),collapse=", "))

num_overal_actors <- Actors_districts %>% group_by(region) %>% summarise(num_overal_actors = n_distinct(name_agency))

#Factsheet datamerge finale ----

analysis_df_list<-list(ug_regions,num_overal_actors,actor_per_region,region_donors_list,region_donors_count,Region_factsheets,Region_factsheets2,Region_factsheets3,Region_factsheets4)

Region_factsheets_datamerge <-purrr::reduce(analysis_df_list, left_join)




#export file-----


list_of_datasets <- list("Contact info" = contact_details, "data_merge_file"= Region_factsheets_datamerge,"overview by partner" = d.f1, "Pillars by actors" = actors_pillar_activity, "Coordination locations" = district_coord_activities,"infect prevention" = district_infe_activities, "Surveilance location" = district_surve_activities, "Case management loc"= district_case_activities,
                         "risk comm loc" = district_risk_activities,"Mental health loc" = district_mental_activities, "ICT locations" = district_ict_activities, "WASH location" = district_WH_activities, "logistics location" = district_logs_activities, "HR locations" = district_hr_activities )
write.xlsx(list_of_datasets, file = "outputs/UG_convid_4w20200430.xlsx")


