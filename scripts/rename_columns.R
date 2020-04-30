

names(df)[names(df) == "_index.y"] <- "project_id"

colClean <- function(x){ colnames(x) <- gsub("C.", "", colnames(x)); x }
df <- colClean(df)

colClean <- function(x){ colnames(x) <- gsub("A.", "", colnames(x)); x }
df <- colClean(df)


names(df)[names(df) == "B.projects.district_activity_area"] <- "B.projects.district_activity_area_acholi"
names(df)[names(df) == "B.projects.b1.district_coordination"] <- "B.projects.b1.district_coordination_acholi"
names(df)[names(df) == "B.projects.b3.district_surveillance"] <- "B.projects.b3.district_surveillance_acholi"
names(df)[names(df) == "B.projects.b4.district_case_management"] <- "B.projects.b4.district_case_management_acholi"
names(df)[names(df) == "B.projects.b5.district_WASH"] <- "B.projects.b5.district_WASH_acholi"
names(df)[names(df) == "B.projects.b6.district_ict_innovation"] <- "B.projects.b6.district_ict_innovation_acholi"
names(df)[names(df) == "B.projects.b7.district_mental_health_psychosocial_support"] <- "B.projects.b7.district_mental_health_psychosocial_support_acholi"
names(df)[names(df) == "B.projects.b8.district_risk_communication"] <- "B.projects.b8.district_risk_communication_acholi"
names(df)[names(df) == "B.projects.b10.district_human_resources"] <- "B.projects.b10.district_human_resources_acholi"
names(df)[names(df) == "B.projects.b2.district_infection_prevention"] <- "B.projects.b2.district_infection_prevention_acholi"
names(df)[names(df) == "B.projects.b9.district_logistics"] <- "B.projects.b9.district_logistics_acholi"



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

