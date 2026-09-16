######### Data Cleaning #########

## Set working directory to "Replicable_Code" folder with all elements
setwd(".../Replicable_Code")


####### 1. Clean Daymet Exposure Data #######
### Read data
prcp <- read.csv("DATA/new Virginia weather data/prcp_VA_zctas_2015_2023.csv") #precipitation (mm)
tmax <- read.csv("DATA/new Virginia weather data/tmax_VA_zctas_2015_2023.csv") #max temperature (ºC)
tmin <- read.csv("DATA/new Virginia weather data/tmin_VA_zctas_2015_2023.csv") #min temperature (ºC)

#### Clean variables ####
# Keep variables of interest
tmax_clean <- tmax[,!names(tmax) %in% c("WKT","STATEFP10","GEOID10","CLASSFP10","MTFCC10","FUNCSTAT10","ALAND10",
                                        "AWATER10","INTPTLAT10","INTPTLON10","PARTFLG10","ID")]
tmin_clean <- tmin[,!names(tmin) %in% c("WKT","STATEFP10","GEOID10","CLASSFP10","MTFCC10","FUNCSTAT10","ALAND10",
                                        "AWATER10","INTPTLAT10","INTPTLON10","PARTFLG10","ID")]
prcp_clean <- prcp[,!names(prcp) %in% c("WKT","STATEFP10","GEOID10","CLASSFP10","MTFCC10","FUNCSTAT10","ALAND10",
                                        "AWATER10","INTPTLAT10","INTPTLON10","PARTFLG10","ID")]

# Rename variables
tmax_clean$ZCTA <- tmax_clean$ZCTA5CE10
tmax_clean$ZCTA5CE10 <- NULL
tmin_clean$ZCTA <- tmin_clean$ZCTA5CE10
tmin_clean$ZCTA5CE10 <- NULL
names(tmax_clean)[-3286] <- sub("tmax_","",names(tmax_clean)[-3286])
names(tmin_clean)[-3286] <- sub("tmin_","",names(tmin_clean)[-3286])
prcp_clean$ZCTA <- prcp_clean$ZCTA5CE10
prcp_clean$ZCTA5CE10 <- NULL
names(prcp_clean)[-3286] <- sub("prcp_","",names(prcp_clean)[-3286])


#### Format: wide to long, date, ZCTA_date ####
# Wide to Long
library(tidyr)
tmax_long <- pivot_longer(tmax_clean,starts_with("20"), names_to = "date", values_to = "tmax")
head(tmax_long)
tmin_long <- pivot_longer(tmin_clean,starts_with("20"), names_to = "date", values_to = "tmin")
head(tmin_long)
prcp_long <- pivot_longer(prcp_clean,starts_with("20"), names_to = "date", values_to = "prcp")
head(prcp_long)

# Date format and variables
library(lubridate)
tmax_long$date <- as.Date(tmax_long$date,format="%Y.%m.%d")
tmax_long$year <- year(tmax_long$date)
tmax_long$month <- month(tmax_long$date)
tmax_long$day <- day(tmax_long$date)
head(tmax_long)
tmin_long$date <- as.Date(tmin_long$date,format="%Y.%m.%d")
tmin_long$year <- year(tmin_long$date)
tmin_long$month <- month(tmin_long$date)
tmin_long$day <- day(tmin_long$date)
head(tmin_long)
prcp_long$date <- as.Date(prcp_long$date,format="%Y.%m.%d")
prcp_long$year <- year(prcp_long$date)
prcp_long$month <- month(prcp_long$date)
prcp_long$day <- day(prcp_long$date)
head(prcp_long)

# Variable with both date and ZCTA
tmax_final <- tmax_long
tmax_final$ZCTA_Date <- paste(tmax_long$ZCTA,as.character(tmax_long$date),sep="_")
head(tmax_final)
tmin_final <- tmin_long
tmin_final$ZCTA_Date <- paste(tmin_long$ZCTA,as.character(tmin_long$date),sep="_")
head(tmin_final)
prcp_final <- prcp_long
prcp_final$ZCTA_Date <- paste(prcp_long$ZCTA,as.character(prcp_long$date),sep="_")
head(prcp_final)


#### Interpolate missing data ####
table(tmax_final[tmax_final$month==12&tmax_final$day==31,]$year) #Missing for 2016 and 2020

#tmax
ZCTAs <- unique(tmax_final$ZCTA)
interpolated <- data.frame(ZCTA=rep(ZCTAs,2),
                           date=c(rep("2016-12-31",896),rep("2020-12-31",896)),
                           tmax=NA,
                           year=c(rep("2016",896),rep("2020",896)),
                           month="12",
                           day="31",
                           ZCTA_Date=NA)
interpolated$ZCTA_Date<- paste(interpolated$ZCTA,interpolated$date,sep="_")
tmax_new <- rbind(tmax_final,interpolated)
nrow(tmax_new[is.na(tmax_new$tmax),]) #896 ZCTAs x 2 days = 1792
tmax_new$date <- as.Date(tmax_new$date,format="%Y-%m-%d")
tmax_new <- tmax_new[order(tmax_new$ZCTA,tmax_new$date), ]
head(tmax_new)
interpolation_by_ZCTA <- function(tmax_new_zcta) {
  tmax_new_zcta$tmax <- approx(tmax_new_zcta$date,tmax_new_zcta$tmax,xout=tmax_new_zcta$date)$y
  return(tmax_new_zcta)
}
tmax_interpolated <- do.call(rbind,lapply(split(tmax_new,tmax_new$ZCTA),interpolation_by_ZCTA))
sum(is.na(tmax_interpolated$tmax)) #worked!!
nrow(tmax_final) + (896 *2) == nrow(tmax_interpolated) #good!

#tmin
ZCTAs <- unique(tmin_final$ZCTA)
interpolated <- data.frame(ZCTA=rep(ZCTAs,2),
                           date=c(rep("2016-12-31",896),rep("2020-12-31",896)),
                           tmin=NA,
                           year=c(rep("2016",896),rep("2020",896)),
                           month="12",
                           day="31",
                           ZCTA_Date=NA)
interpolated$ZCTA_Date<- paste(interpolated$ZCTA,interpolated$date,sep="_")
tmin_new <- rbind(tmin_final,interpolated)
nrow(tmin_new[is.na(tmin_new$tmin),]) #896 ZCTAs x 2 days = 1792
tmin_new$date <- as.Date(tmin_new$date,format="%Y-%m-%d")
tmin_new <- tmin_new[order(tmin_new$ZCTA,tmin_new$date), ]
interpolation_by_ZCTA <- function(tmin_new_zcta){
  tmin_new_zcta$tmin <- approx(tmin_new_zcta$date,tmin_new_zcta$tmin,xout=tmin_new_zcta$date)$y
  return(tmin_new_zcta)
}
tmin_interpolated <- do.call(rbind,lapply(split(tmin_new,tmin_new$ZCTA),interpolation_by_ZCTA))
nrow(tmin_interpolated[is.na(tmin_interpolated$tmin),]) #worked!!
nrow(tmin_final) + (896 *2) == nrow(tmin_interpolated) #good!

#prcp
ZCTAs <- unique(prcp_final$ZCTA)
interpolated <- data.frame(ZCTA=rep(ZCTAs,2),
                           date=c(rep("2016-12-31",896),rep("2020-12-31",896)),
                           prcp=NA,
                           year=c(rep("2016",896),rep("2020",896)),
                           month="12",
                           day="31",
                           ZCTA_Date=NA)
interpolated$ZCTA_Date<- paste(interpolated$ZCTA,interpolated$date,sep="_")
prcp_new <- rbind(prcp_final,interpolated)
nrow(prcp_new[is.na(prcp_new$prcp),]) #896 ZCTAs x 2 days = 1792
prcp_new$date <- as.Date(prcp_new$date,format="%Y-%m-%d")
prcp_new <- prcp_new[order(prcp_new$ZCTA,prcp_new$date), ]
interpolation_by_ZCTA <- function(prcp_new_zcta){
  prcp_new_zcta$prcp <- approx(prcp_new_zcta$date,prcp_new_zcta$prcp,xout=prcp_new_zcta$date)$y
  return(prcp_new_zcta)
}
prcp_interpolated <- do.call(rbind,lapply(split(prcp_new,prcp_new$ZCTA),interpolation_by_ZCTA))
nrow(prcp_interpolated[is.na(prcp_interpolated$prcp),]) #worked!!
nrow(prcp_final) + (896 *2) == nrow(prcp_interpolated) #good!





####### 2. Clean HRRR Exposure Data #######
### Read Datasets
# Home data
samarth_home <- data.frame(date=NA, zcta=NA,tmp=NA,rh=NA,prcp=NA)
for (n in c(2015:2023)) {
  print(n)
  samarth <- read.csv(paste0("DATA/Synthetic Population data/va_",n,"_home_daily_zcta_exposures_final.csv"))
  samarth_home <- rbind(samarth_home,samarth)
}
samarth_home <- samarth_home[!is.na(samarth_home$date),]

# Activity data
samarth_act <- data.frame(date=NA, zcta=NA,tmp=NA,rh=NA,prcp=NA)
for (n in c(2015:2023)) {
  print(n)
  samarth <- read.csv(paste0("DATA/Synthetic Population data/va_",n,"_act_daily_zcta_exposures_final.csv"))
  samarth_act <- rbind(samarth_act,samarth)
}
samarth_act <- samarth_act[!is.na(samarth_act$date),]

#### Add year and DOW ####
# Year
samarth_home$year <- substr(samarth_home$date,1,4)
head(samarth_home$year)
samarth_act$year <- substr(samarth_act$date,1,4)
head(samarth_act$year)

# DOW
samarth_home$DOW <- weekdays(as.Date(samarth_home$date,format="%Y-%m-%d"))
head(samarth_home$DOW)
samarth_act$DOW <- weekdays(as.Date(samarth_act$date,format="%Y-%m-%d"))
head(samarth_act$DOW)

#### Interpolate prcp for two missing days ####
### Home
samarth_home$date <- as.Date(samarth_home$date,format="%Y-%m-%d")
samarth_home <- samarth_home[order(samarth_home$zcta,samarth_home$date),]
interpolation_by_ZCTA <- function(interpolated_home){
  interpolated_home$prcp <- approx(interpolated_home$date,interpolated_home$prcp,xout=interpolated_home$date)$y
  return(interpolated_home)
}
samarth_home2 <- do.call(rbind,lapply(split(samarth_home,samarth_home$zcta),interpolation_by_ZCTA))
summary(samarth_home2)
nrow(samarth_home2[is.na(samarth_home2$prcp),]) #worked!!

### Activity
samarth_act$date <- as.Date(samarth_act$date,format="%Y-%m-%d")
samarth_act <- samarth_act[order(samarth_act$zcta,samarth_act$date),]
interpolation_by_ZCTA <- function(interpolated_act){
  interpolated_act$prcp <- approx(interpolated_act$date,interpolated_act$prcp,xout=interpolated_act$date)$y
  return(interpolated_act)
}
samarth_act2 <- do.call(rbind,lapply(split(samarth_act,samarth_act$zcta),interpolation_by_ZCTA))
summary(samarth_act2)
nrow(samarth_act2[is.na(samarth_act2$prcp),]) #worked!!


#### Temp in celsius ####
samarth_home2$tmp_C <- samarth_home2$tmp-273.15
summary(samarth_home2$tmp_C)
samarth_act2$tmp_C <- samarth_act2$tmp-273.15
summary(samarth_act2$tmp_C)


####### 3. Clean VDH Outcome Data #######
### Read data
vdh <- read.csv("DATA/2024 data/Condition_Analysis_Patient_Zipcode_All_Facilities_VT.csv")
head(vdh)

#### Clean variables #####
# Date format and variables
library(lubridate)
vdh_clean <- vdh
vdh_clean$date <- as.Date(vdh_clean$Date,format="%Y-%m-%d")
vdh_clean$Date <- NULL
vdh_clean$year <- year(vdh_clean$date)
vdh_clean$month <- month(vdh_clean$date)
vdh_clean$day <- day(vdh_clean$date)

#### Figure out zipcode to zcta matching ####
### Add zcta with zippeR\
# zippeR crosswalk
library(zippeR)
zippeR_2020 <- zi_load_crosswalk(zip_source = "UDS", year = 2020)

# add zippeR zctas and see
vdh_zcta1 <- merge(vdh_clean,zippeR_2020[zippeR_2020$STATE=="VA",c("ZIP","ZCTA")],by.x="Patient_Zip_Code",by.y="ZIP", all.x=TRUE)
sum(is.na(vdh_zcta1$ZCTA)) #170,924 -  some zipcodes have not matched to a zcta

# zipcodes with no zcta in zippeR
vdh_zcta1_nomatch <- vdh_zcta1[is.na(vdh_zcta1$ZCTA) & !duplicated(vdh_zcta1$Patient_Zip_Code),]$Patient_Zip_Code
length(vdh_zcta1_nomatch) #52 - same as Meredith

### Add good zipcodes found manually and re-do zippeR
### Meredith's crosswalk
library(readxl)
Meredith_crosswalk <- read_excel("DATA/13042026_lookup match_Meredith.xlsx")

### Add correct zipcodes to vdh
vdh_zcta2 <- merge(vdh_clean,Meredith_crosswalk,by.x="Patient_Zip_Code",by.y="invalid_visit_zip",all.x=TRUE)

### Final good zipcode variable
vdh_zcta2$zip_code_correct <- ifelse(is.na(vdh_zcta2$valid_visit_zip), vdh_zcta2$Patient_Zip_Code,vdh_zcta2$valid_visit_zip)
sum(is.na(vdh_zcta2$zip_code_correct)) #0 - al rows hav a valid zipcode

### add zippeR zctas and see
vdh_zcta2 <- merge(vdh_zcta2,zippeR_2020[zippeR_2020$STATE=="VA",c("ZIP","ZCTA")],by.x="zip_code_correct",by.y="ZIP", all.x=TRUE)
sum(is.na(vdh_zcta2$ZCTA)) #118,332 -  some zipcodes have not matched to a zcta

### zipcodes with no zcta in zippeR after adding correct zipcodes
vdh_zcta2_nomatch <- vdh_zcta2[is.na(vdh_zcta2$ZCTA) & !duplicated(vdh_zcta2$zip_code_correct),]$zip_code_correct
length(vdh_zcta2_nomatch) #36 - same as Meredith


#### ZCTA_date variable ####
vdh_final <- vdh_zcta2[!is.na(vdh_zcta2$ZCTA),]
vdh_final$ZCTA_Date <- paste(vdh_final$ZCTA,vdh_final$date,sep="_")
sum(is.na(vdh_final$ZCTA_Date))
head(vdh_final)

### Check for ZCTA_Date duplicates
nrow(vdh_final[duplicated(vdh_final$ZCTA_Date),]) ##1,127,441 duplicates
vdh_dups <- vdh_final[duplicated(vdh_final$ZCTA_Date),]
vdh_dups <- vdh_final[vdh_final$ZCTA_Date %in% vdh_dups$ZCTA_Date,]
length(unique(vdh_dups$ZCTA)) #181 ZCTAS
summary(vdh_dups$date) #spread across whole study period
sum(duplicated(vdh_dups$ZCTA_Date))

### See original zip codes of duplicated ZCTAs
vdh_zcta_dups <- unique(vdh_dups$ZCTA)
length(zippeR_2020[zippeR_2020$STATE=="VA" & zippeR_2020$ZCTA %in% vdh_zcta_dups,]$ZIP) # 509 zipcodes for 181 ZCTAs - this is the issue!
head(vdh_dups[order(vdh_dups$ZCTA_Date),]) #several zipcodes share same zcta

### Sum number of visits across duplicated ZCTA_Date
vdh_dups_fix <- vdh_dups[,c("Pregnancy_Related_Visits_Patient_Zip", "Diarrheal_Visits_Patient_Zip" ,"Total_Visits_Patient_Zip","ZCTA_Date")]
sum(duplicated(vdh_dups_fix$ZCTA_Date))
vdh_dups_fix <- aggregate(vdh_dups_fix[,c(1:3)],by=list(vdh_dups_fix$ZCTA_Date),FUN=sum)
head(vdh_dups_fix)
sum(duplicated(vdh_dups_fix$Group.1))
vdh_dups_fix$ZCTA_Date <- vdh_dups_fix$Group.1
vdh_dups_fix$Group.1 <- NULL
sum(duplicated(vdh_dups_fix$ZCTA_Date))

### Add back to the main dataset
# fixed rows:
vdh_dups_fix2 <- merge(vdh_dups_fix,vdh_dups[!duplicated(vdh_dups$ZCTA_Date),c("ZCTA_Date","ZCTA","date","year","month","day","VDH_Health_Region")],by="ZCTA_Date",all.x=TRUE)
sum(is.na(vdh_dups_fix2$date)) #0 - all zcta dates merged ok
head(vdh_dups_fix2)
# rest of rows:
vdh_final2 <- vdh_final[!vdh_final$ZCTA_Date %in% vdh_dups$ZCTA_Date,c("ZCTA_Date","Pregnancy_Related_Visits_Patient_Zip","Diarrheal_Visits_Patient_Zip","Total_Visits_Patient_Zip","ZCTA","date","year","month","day","VDH_Health_Region")]
nrow(vdh_final) - nrow(vdh_dups) == nrow(vdh_final2) #good!
# row bind:
names(vdh_final2) == names(vdh_dups_fix2)
vdh_final2 <- rbind(vdh_final2,vdh_dups_fix2)
nrow(vdh_final2) == nrow(vdh_final) - nrow(vdh_dups) + nrow(vdh_dups_fix2) #good!
head(vdh_final2)




####### 4. Match VDH (Outcome) and Daymet (Exposure) Data #######


#### Merge by ZCTA-date ####
vdh_tmax <- merge(vdh_final2,tmax_interpolated[,c("ZCTA_Date","tmax")],by="ZCTA_Date")
sum(is.na(vdh_tmax$tmax)) #0, good! No row without tmax value
vdh_tmax_tmin <- merge(vdh_tmax,tmin_interpolated[,c("ZCTA_Date","tmin")],by="ZCTA_Date")
sum(is.na(vdh_tmax_tmin$tmin)) #0, good! No row without tmin value
vdh_tmax_tmin_prcp <- merge(vdh_tmax_tmin,prcp_interpolated[,c("ZCTA_Date","prcp")],by="ZCTA_Date")
sum(is.na(vdh_tmax_tmin_prcp$prcp)) #0, good! No row without prcp value

### Check ZCTA_Date duplicates
sum(duplicated(vdh_tmax_tmin_prcp$ZCTA_Date))


#### Process visit variables ####
# Create binary variables
vdh_tmax_tmin_prcp_2 <- vdh_tmax_tmin_prcp
vdh_tmax_tmin_prcp_2$All.visits.binary <- ifelse(vdh_tmax_tmin_prcp_2$Total_Visits_Patient_Zip==0, 0, 1)
vdh_tmax_tmin_prcp_2$Pregnancy.visits.binary <- ifelse(vdh_tmax_tmin_prcp_2$Pregnancy_Related_Visits_Patient_Zip==0, 0, 1)
vdh_tmax_tmin_prcp_2$Diarrhea.visits.binary <- ifelse(vdh_tmax_tmin_prcp_2$Diarrheal_Visits_Patient_Zip==0, 0, 1)
vdh_tmax_tmin_prcp_2$DOW <- weekdays(as.Date(vdh_tmax_tmin_prcp_2$date,format="%Y-%m-%d"))
vdh_tmax_tmin_prcp_2$memorial <- ifelse(vdh_tmax_tmin_prcp_2$date=="2015-05-25"|vdh_tmax_tmin_prcp_2$date=="2016-05-30"|
                                          vdh_tmax_tmin_prcp_2$date=="2017-05-29"|vdh_tmax_tmin_prcp_2$date=="2018-05-27"|
                                          vdh_tmax_tmin_prcp_2$date=="2019-05-26"|vdh_tmax_tmin_prcp_2$date=="2020-05-25"|
                                          vdh_tmax_tmin_prcp_2$date=="2021-05-31"|vdh_tmax_tmin_prcp_2$date=="2022-05-30"|
                                          vdh_tmax_tmin_prcp_2$date=="2023-05-29", 1, 0)
vdh_tmax_tmin_prcp_2$independence <- ifelse(vdh_tmax_tmin_prcp_2$date=="2015-07-04"|vdh_tmax_tmin_prcp_2$date=="2016-07-04"|
                                              vdh_tmax_tmin_prcp_2$date=="2017-07-04"|vdh_tmax_tmin_prcp_2$date=="2018-07-04"|
                                              vdh_tmax_tmin_prcp_2$date=="2019-07-04"|vdh_tmax_tmin_prcp_2$date=="2020-07-04"|
                                              vdh_tmax_tmin_prcp_2$date=="2021-07-04"|vdh_tmax_tmin_prcp_2$date=="2022-07-04"|
                                              vdh_tmax_tmin_prcp_2$date=="2023-07-04", 1, 0)
vdh_tmax_tmin_prcp_2$labor <- ifelse(vdh_tmax_tmin_prcp_2$date=="2015-09-07"|vdh_tmax_tmin_prcp_2$date=="2016-09-05"|
                                       vdh_tmax_tmin_prcp_2$date=="2017-09-04"|vdh_tmax_tmin_prcp_2$date=="2018-09-03"|
                                       vdh_tmax_tmin_prcp_2$date =="2019-09-02"|vdh_tmax_tmin_prcp_2$date=="2020-09-07"|
                                       vdh_tmax_tmin_prcp_2$date=="2021-09-06"|vdh_tmax_tmin_prcp_2$date=="2022-09-05"|
                                       vdh_tmax_tmin_prcp_2$date=="2023-09-04", 1, 0)
vdh_tmax_tmin_prcp_2$holiday <- ifelse(vdh_tmax_tmin_prcp_2$memorial==1|vdh_tmax_tmin_prcp_2$independence==1|vdh_tmax_tmin_prcp_2$labor==1,1,0)


### Day Of Year variable ####
vdh_tmax_tmin_prcp_2$DOY <- strftime(vdh_tmax_tmin_prcp_2$date,format="%j")
vdh_tmax_tmin_prcp_2$DOY <- as.numeric(vdh_tmax_tmin_prcp_2$DOY)
summary(vdh_tmax_tmin_prcp_2$DOY)
head(vdh_tmax_tmin_prcp_2)

### Remove region in health region zone ####
head(vdh_tmax_tmin_prcp_2$VDH_Health_Region)
vdh_tmax_tmin_prcp_2$Region <- gsub(" Region","",vdh_tmax_tmin_prcp_2$VDH_Health_Region)
head(vdh_tmax_tmin_prcp_2$Region)


####### 5. Match VDH (Outcome) and HRRR (Exposure) Data #######
### Home
samarth_home3 <- merge(samarth_home2,vdh_final2[,c("ZCTA","date","Total_Visits_Patient_Zip","Pregnancy_Related_Visits_Patient_Zip","Diarrheal_Visits_Patient_Zip","VDH_Health_Region")],
                       by.x=c("zcta","date"),by.y=c("ZCTA","date"))
sum(is.na(samarth_home3$Total_Visits_Patient_Zip)) #0, good!
summary(samarth_home3)

### Activity
samarth_act3 <- merge(samarth_act2,vdh_final2[,c("ZCTA","date","Total_Visits_Patient_Zip","Pregnancy_Related_Visits_Patient_Zip","Diarrheal_Visits_Patient_Zip","VDH_Health_Region")],
                      by.x=c("zcta","date"),by.y=c("ZCTA","date"))
sum(is.na(samarth_act3$Total_Visits_Patient_Zip)) #0, good!
summary(samarth_act3)

### Remove region in health region zone ####
head(samarth_home3$VDH_Health_Region)
samarth_home3$Region <- gsub(" Region","",samarth_home3$VDH_Health_Region)
head(samarth_home3$Region)
head(samarth_act3$VDH_Health_Region)
samarth_act3$Region <- gsub(" Region","",samarth_act3$VDH_Health_Region)
head(samarth_act3$Region)




####### 6. Export Clean Data ####### (saved in the Replicable_Code folder that is the current working directory)
# write.csv(vdh_tmax_tmin_prcp_2,"vdh_tmax_tmin_prcp_clean.csv")
# write.csv(samarth_home3,"samarth_home_clean.csv")
# write.csv(samarth_act3,"samarth_act_clean.csv")



