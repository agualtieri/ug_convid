## Uganda 4W activitiesset Wrangling for DB
## Last modified 06-07-2020

rm(list = ls())
today <- Sys.Date()

## Load libraries
library(tidyverse)
library(openxlsx)
library(lubridate)
library(clog)
library(stringr)
library(readxl)
library(cleaner)



## Load external scripts
source("./scripts/moveme.R")

## Load activities and remove unused column and rename column headers
activities <- read.csv("./outputs/UG_convid_matrix_2020-08-24.csv", stringsAsFactors = F)

activities <- activities %>% select(-X, -donors, -ongoing_ability, -new_project_funding, -funding_amount)

names(activities)[names(activities) == "projects_uID"] <- "ProjectID"
names(activities)[names(activities) == "name_agency"] <- "Partner"
names(activities)[names(activities) == "project_title"] <- "Project"
names(activities)[names(activities) == "project_status"] <- "Project_Status"
names(activities)[names(activities) == "pillars"] <- "Pillars_supported_by_project"
names(activities)[names(activities) == "projects_activities"] <- "Activities_conducted"
names(activities)[names(activities) == "other_activities"] <- "Other_nonpPillar_activities"
names(activities)[names(activities) == "implimenting_partner"] <- "Implementing_partner"
names(activities)[names(activities) == "regions"] <- "Regions"
names(activities)[names(activities) == "districts"] <- "Districts"

## Fix "project_Status"
activities$Project_Status <- gsub("ongoing_project", "Ongoing Project", activities$Project_Status)
activities$Project_Status <- gsub("new_project", "New Project", activities$Project_Status)

## Fix "Pillars_supported_by_project" column
activities$Pillars_supported_by_project <- str_replace_all(activities$Pillars_supported_by_project, ",", ", ")

## Fix "Activities_conducted" column
activities$Activities_conducted <- gsub("Coordination, ", "", activities$Activities_conducted)
activities$Activities_conducted <- gsub("Infection prevention, ", "", activities$Activities_conducted) 
activities$Activities_conducted <- gsub("Surveillance, ", "", activities$Activities_conducted)
activities$Activities_conducted <- gsub("Case management, ", "", activities$Activities_conducted)
activities$Activities_conducted <- gsub("Logistics, ", "", activities$Activities_conducted)
activities$Activities_conducted <- gsub("Human resource", "", activities$Activities_conducted)
activities$Activities_conducted <- gsub("none, ", "", activities$Activities_conducted)
activities$Activities_conducted <- gsub("supplies, ", "", activities$Activities_conducted)
activities$Activities_conducted <- gsub("personnel, ", "", activities$Activities_conducted)
activities$Activities_conducted <- gsub("supplies", "", activities$Activities_conducted)
activities$Activities_conducted <- gsub("cash,", "", activities$Activities_conducted)
activities$Activities_conducted <- gsub("cash", "", activities$Activities_conducted)
activities$Activities_conducted <- gsub("other, ", "", activities$Activities_conducted)
activities$Activities_conducted <- gsub(",", "", activities$Activities_conducted)

activities$Activities_conducted <- str_trim(activities$Activities_conducted, "both")

activities$Activities_conducted <- gsub(" ", ", ", activities$Activities_conducted)

## Split and rename
activity_labels <- read.xlsx("./inputs/tool/Uganda_ULEARN_4W_Kobo_choices.xlsx")

test <- str_split_fixed(activities$Activities_conducted, ", ", 20)
test <- as.data.frame(test)
test[1:20] <- activity_labels$label[match(unlist(test[1:20]), activity_labels$name)] 
test$ProjectID <- test$ProjectID

test$Activities_conducted <- Reduce(function(...) paste(..., sep = ", "), test[1:20])


## Replace the new column into the activitiesframe and remove NAs
activities$Activities_conducted <- test$Activities_conducted
activities$Activities_conducted <- gsub("NA, ", "", activities$Activities_conducted)
activities$Activities_conducted <- gsub("NA", "", activities$Activities_conducted)

## Fix "Regions" column
activities$Regions <- gsub(",", "", activities$Regions)
activities$Regions <- gsub(" ", ", ", activities$Regions)
activities$Regions <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",    # Uppercase with Base R
                      activities$Regions, perl = TRUE)

activities$Regions <- gsub("Westnile", "West Nile", activities$Regions)

## Fix "Districts" column
activities$Districts <- gsub(",", "", activities$Districts)
activities$Districts <- gsub(" ", ", ", activities$Districts)
activities$Districts <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",    # Uppercase with Base R
                     activities$Districts, perl = TRUE)

## Merge "activities" and "nonPillar activities" columns into one
activities$Activities_conducted2 <- paste(activities$Activities_conducted, activities$Other_nonpPillar_activities)
activities$Activities_conducted2 <- gsub("NA", "", activities$Activities_conducted2)
activities$Activities_conducted2 <- str_trim(activities$Activities_conducted2, side="both")


## Delete old and moveme
activities$Activities_conducted <- NULL
activities$Other_nonpPillar_activities <- NULL


activities <- activities[moveme(names(activities), "Activities_conducted2 after Pillars_supported_by_project")]


## Save file
write.csv(activities, "./outputs/UGA_COVID_Activities.csv", row.names = FALSE, na= "")


## Load actors output
actors <- lapply(5:14, function(i) read_excel("outputs/UG_4W COVID_2020-08-24.xlsx", sheet = i))

## Rename cols
names(actors[[1]])[names(actors[[1]]) == "actors"] <- "Coordination"
names(actors[[2]])[names(actors[[2]]) == "actors"] <- "PreventionControl"
names(actors[[3]])[names(actors[[3]]) == "actors"] <- "Surveillance"
names(actors[[4]])[names(actors[[4]]) == "actors"] <- "CaseManagement"
names(actors[[5]])[names(actors[[5]]) == "actors"] <- "RiskCommunication"
names(actors[[6]])[names(actors[[6]]) == "actors"] <- "MentalHealth"
names(actors[[7]])[names(actors[[7]]) == "actors"] <- "ICTInnovation"
names(actors[[8]])[names(actors[[8]]) == "actors"] <- "WASH"
names(actors[[9]])[names(actors[[9]]) == "actors"] <- "Logistics"
names(actors[[10]])[names(actors[[10]]) == "actors"] <- "HumanResources"

names(actors[[1]])[names(actors[[1]]) == "coordination1"] <- "Coordination_n"
names(actors[[2]])[names(actors[[2]]) == "infection_prevention1"] <- "PreventionControl_n"
names(actors[[3]])[names(actors[[3]]) == "surveillance1"] <- "Surveillance_n"
names(actors[[4]])[names(actors[[4]]) == "case_management1"] <- "CaseManagement_n"
names(actors[[5]])[names(actors[[5]]) == "risk_communications1"] <- "RiskCommunication_n"
names(actors[[6]])[names(actors[[6]]) == "mental_health1"] <- "MentalHealth_n"
names(actors[[7]])[names(actors[[7]]) == "ict_innovations1"] <- "ICTInnovation_n"
names(actors[[8]])[names(actors[[8]]) == "wash1"] <- "WASH_n"
names(actors[[9]])[names(actors[[9]]) == "logistics1"] <- "Logistics_n"
names(actors[[10]])[names(actors[[10]]) == "human_resources1"] <- "HumanResources_n"

## Select only the columns that are needed
get_three <- function(data){data[1:3]}
get_last <- function(data){data[,ncol(data)]}

list_three <- lapply(actors, get_three)
list_last <- lapply(actors, get_last)

## Bind all
list_all <- mapply(cbind, list_three, list_last, SIMPLIFY = F)

## Create a dataframe
actors_df <- purrr::reduce(list_all, left_join, by="district")

## Concatenate actors and remove variables not needed
# actors_df$all_actors <- Reduce(function(...) paste(..., sep = ", "), select(actors_df, contains("actors")))
actors_df <- select(actors_df, -contains("region."))

actors_df  <- data.frame(lapply(actors_df, function(x) {
                   gsub(", ", " | ", x) }))

names(actors_df)[names(actors_df) == "district"] <- "District"
actors_df$District <- toupper(actors_df$District)

## Split and count
coord <- str_split_fixed(actors_df$Coordination, "\\|", 100) %>%  as.data.frame() %>% mutate_all(na_if,"") %>% 
         mutate(Coordination_n = rowSums(!is.na(.)))

actors_df$Coordination_n <- coord$Coordination_n


prev <- str_split_fixed(actors_df$PreventionControl, "\\|", 100) %>%  as.data.frame() %>% mutate_all(na_if,"") %>% 
        mutate(PreventionControl_n = rowSums(!is.na(.)))

actors_df$PreventionControl_n <- prev$PreventionControl_n

surv <- str_split_fixed(actors_df$Surveillance, "\\|", 100) %>%  as.data.frame() %>% mutate_all(na_if,"") %>% 
        mutate(Surveillance_n = rowSums(!is.na(.)))

actors_df$Surveillance_n <- surv$Surveillance_n
  
case <- str_split_fixed(actors_df$CaseManagement, "\\|", 100) %>%  as.data.frame() %>% mutate_all(na_if,"") %>% 
        mutate(CaseManagement_n = rowSums(!is.na(.)))

actors_df$CaseManagement_n <- case$CaseManagement_n


risk <- str_split_fixed(actors_df$RiskCommunication, "\\|", 100) %>%  as.data.frame() %>% mutate_all(na_if,"") %>% 
        mutate(RiskCommunicationt_n = rowSums(!is.na(.)))

actors_df$RiskCommunication_n <- risk$RiskCommunicationt_n


ment <- str_split_fixed(actors_df$MentalHealth, "\\|", 100) %>%  as.data.frame() %>% mutate_all(na_if,"") %>% 
        mutate(MentalHealth_n = rowSums(!is.na(.)))

actors_df$MentalHealth_n <- ment$MentalHealth_n


ict <- str_split_fixed(actors_df$ICTInnovation, "\\|", 100) %>%  as.data.frame() %>% mutate_all(na_if,"") %>% 
       mutate(ICTInnovation_n = rowSums(!is.na(.)))


actors_df$ICTInnovation_n <- ict$ICTInnovation_n


wash <- str_split_fixed(actors_df$WASH, "\\|", 100) %>%  as.data.frame() %>% mutate_all(na_if,"") %>% 
        mutate(WASH_n = rowSums(!is.na(.)))

actors_df$WASH_n <- wash$WASH_n


logs <- str_split_fixed(actors_df$Logistics, "\\|", 100) %>%  as.data.frame() %>% mutate_all(na_if,"") %>% 
        mutate(Logistics_n = rowSums(!is.na(.)))


actors_df$Logistics_n <- logs$Logistics_n


huma <- str_split_fixed(actors_df$HumanResources, "\\|", 100) %>%  as.data.frame() %>% mutate_all(na_if,"") %>% 
        mutate(HumanResources_n = rowSums(!is.na(.)))

actors_df$HumanResources_n <- huma$HumanResources_n



## Move stuff around
actors_df <- actors_df[moveme(names(actors_df), "PreventionControl_n after Coordination_n; 
                                                 Surveillance_n after PreventionControl_n;
                                                 CaseManagement_n after Surveillance_n;
                                                 RiskCommunication_n after Surveillance_n;
                                                 MentalHealth_n after RiskCommunication_n;
                                                 ICTInnovation_n after MentalHealth_n;
                                                 WASH_n after ICTInnovation_n;
                                                 Logistics_n after WASH_n;
                                                 HumanResources_n after Logistics_n")]

## Replace NA with 0
cols <- names(actors_df[2:11])
actors_df[,cols] <- apply(actors_df[,cols], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))

actors_df[2:11][is.na(actors_df[2:11])] <- 0


## Save output
write.csv(actors_df, "./outputs/UGA_COVID_Actors.csv", row.names = FALSE, na= "")

