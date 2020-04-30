library(koboloadeR)
library(dplyr)
library(stringr)
source("scripts/keys.R")




# MONTHLY INPUT - ONLY PART OF SCRIPT WHICH SHOULD REALLY BE UPDATED ------

output_final_folder<-"inputs/raw_data"
assessment_year_month<-"2020_02"




#Get form IF
form_list  <- kobo_datasets(user =  c(keys[1],keys[2]), api="kobohr")


df <- kobo_data_downloader(formid = 521177,user =  c(keys[1],keys[2]), api="kobohr", check = FALSE)



# THIS SHOULD NOT CHANGE --------------------------------------------------

filename_short<-paste0(assessment_year_month,"_New_Settlements_in_Kobo_Data.csv")



feb_files <- list(feb_38b,feb_38a,feb_38)
#combine the feb datasets

aok_data <- bind_rows(feb_files,.id = NULL)




colnames(aok_data)<-gsub("\\/", ".", colnames(aok_data))

aok_other_settlement<- aok_data %>%
  filter(!is.na(D.info_settlement_other)) %>%
  filter(D.info_settlement_other!= "n/a") %>%
  select(uuid="_uuid" ,
         A.base,
         A.enumerator_id,
         D.info_county,
         D.info_settlement_other) %>% arrange(A.base)



# dqm_folder_path<-"../../../../../Dropbox (SSD REACH)/REACH South Sudan upscale/12_AoK/06_AoK_Data_Analysis/10_Data_Quality_Monitoring/"


write.csv(feb_38,"inputs/2020_02/raw_data/aok_feb2020_v38.csv")
write.csv(feb_38a,"inputs/2020_02/raw_data/aok_feb2020_v38a.csv")
write.csv(feb_38b,"inputs/2020_02/raw_data/aok_feb2020_v38b.csv")




