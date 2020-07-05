

names(df)[names(df) == "_index.y"] <- "project_id"

colClean <- function(x){ colnames(x) <- gsub("C.", "", colnames(x)); x }
df <- colClean(df)

colClean <- function(x){ colnames(x) <- gsub("A.", "", colnames(x)); x }
df <- colClean(df)


names(df)[names(df) == "district_activity_area"] <- "district_activity_area_acholi"
names(df)[names(df) == "district_coordination"] <- "district_coordination_acholi"
names(df)[names(df) == "district_surveillance"] <- "district_surveillance_acholi"
names(df)[names(df) == "district_case_management"] <- "district_case_management_acholi"
names(df)[names(df) == "district_WASH"] <- "district_WASH_acholi"
names(df)[names(df) == "district_ict_innovation"] <- "district_ict_innovation_acholi"
names(df)[names(df) == "district_mental_health_psychosocial_support"] <- "district_mental_health_psychosocial_support_acholi"
names(df)[names(df) == "district_risk_communication"] <- "district_risk_communication_acholi"
names(df)[names(df) == "district_human_resources"] <- "district_human_resources_acholi"
names(df)[names(df) == "district_infection_prevention"] <- "district_infection_prevention_acholi"
names(df)[names(df) == "district_logistics"] <- "district_logistics_acholi"



colClean <- function(x){ colnames(x) <- gsub("001", "ankole", colnames(x)); x }
df <- colClean(df)

colClean <- function(x){ colnames(x) <- gsub("002", "bukedi", colnames(x)); x }
df <- colClean(df)

colClean <- function(x){ colnames(x) <- gsub("003", "bunyoro", colnames(x)); x }
df <- colClean(df)

colClean <- function(x){ colnames(x) <- gsub("004", "busoga", colnames(x)); x }
df <- colClean(df)

colClean <- function(x){ colnames(x) <- gsub("005", "elgon", colnames(x)); x }
df <- colClean(df)

colClean <- function(x){ colnames(x) <- gsub("006", "kampala", colnames(x)); x }
df <- colClean(df)

colClean <- function(x){ colnames(x) <- gsub("007", "karamoja", colnames(x)); x }
df <- colClean(df)

colClean <- function(x){ colnames(x) <- gsub("008", "kigezi", colnames(x)); x }
df <- colClean(df)

colClean <- function(x){ colnames(x) <- gsub("009", "lango", colnames(x)); x }
df <- colClean(df)

colClean <- function(x){ colnames(x) <- gsub("010", "northbuganda", colnames(x)); x }
df <- colClean(df)

colClean <- function(x){ colnames(x) <- gsub("011", "southbuganda", colnames(x)); x }
df <- colClean(df)

colClean <- function(x){ colnames(x) <- gsub("012", "teso", colnames(x)); x }
df <- colClean(df)

colClean <- function(x){ colnames(x) <- gsub("013", "toro", colnames(x)); x }
df <- colClean(df)

colClean <- function(x){ colnames(x) <- gsub("014", "westnile", colnames(x)); x }
df <- colClean(df)

