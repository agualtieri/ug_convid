## Uganda 4W Dataset Wrangling for DB
## Last modified 06-07-2020

rm(list = ls())
today <- Sys.Date()

## Load libraries
library(tidyverse)
library(openxlsx)
library(lubridate)
library(clog)
library(dataqualitycontrol)
library(stringr)

## Load data and remove unused column and rename column headers
data <- read.csv("./outputs/UG_convid_matrix_2020-07-06.csv", stringsAsFactors = F)

data <- data %>% anonymise_dataset(c("X", "donors", "ongoing_ability", "new_project_funding", "funding_amount"))

names(data)[names(data) == "projects_uID"] <- "ProjectID"
names(data)[names(data) == "name_agency"] <- "Partner"
names(data)[names(data) == "project_title"] <- "Project"
names(data)[names(data) == "project_status"] <- "Project_Status"
names(data)[names(data) == "pillars"] <- "Pillars_supported_by_project"
names(data)[names(data) == "projects_activities"] <- "Activities_conducted"
names(data)[names(data) == "other_activities"] <- "Other_nonpPillar_activities"
names(data)[names(data) == "implementing_partner"] <- "Implementing_partner"
names(data)[names(data) == "regions"] <- "Regions"
names(data)[names(data) == "districts"] <- "Districts"

## Fix "Pillars_supported_by_project" column
data$Pillars_supported_by_project <- str_replace_all(data$Pillars_supported_by_project, ",", ", ")

## Fix "Activities_conducted" column
data$Activities_conducted
