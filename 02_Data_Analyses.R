######### Data Analyses #########

## Set working directory to "Replicable_Code" folder with all elements
setwd(".../Replicable_Code")


#### 0. Read Clean Data ####
### Daymet
FINAL <- read.csv("vdh_tmax_tmin_prcp_clean.csv")
names(FINAL)
FINAL<- FINAL[,-1]
FINAL$tmean <- (FINAL$tmax+FINAL$tmin)/2
head(FINAL)
FINAL$date <- as.Date(FINAL$date,format="%Y-%m-%d")
head(FINAL,3)

### HRRR
#Home
samarth_home3 <- read.csv("samarth_home_clean.csv")
head(samarth_home3)
samarth_home3<- samarth_home3[,-1]
samarth_home3$date <- as.Date(samarth_home3$date,format="%Y-%m-%d")
head(samarth_home3,3)

#Activity
samarth_act3 <- read.csv("samarth_act_clean.csv")
head(samarth_act3)
samarth_act3<- samarth_act3[,-1]
samarth_act3$date <- as.Date(samarth_act3$date,format="%Y-%m-%d")
head(samarth_act3,3)



####### 1. Descriptive Analyses - Figure 1 #######
library(ggplot2)
##### Daymet #####
FINAL_summarized <- aggregate(FINAL[,c("date","tmean","prcp","Total_Visits_Patient_Zip","Pregnancy_Related_Visits_Patient_Zip","Diarrheal_Visits_Patient_Zip")], by=list(FINAL$date), FUN=mean)
all_ED <- ggplot(data=FINAL_summarized,aes(x=date,y=Total_Visits_Patient_Zip))+
  geom_point() +
  theme_bw() +
  theme(panel.grid.minor=element_blank()) +
  xlab("") + ylab("") + labs(title="All-cause visits") +
  annotate("text", x=as.Date("2015-01-1=01"), y=23, label="a", fontface="bold", size=15)
preg_ED <- ggplot(data=FINAL_summarized,aes(x=date,y=Pregnancy_Related_Visits_Patient_Zip))+
  geom_point() +
  theme_bw() +
  theme(panel.grid.minor=element_blank()) +
  ylab("Average daily visits") + xlab("") + labs(title="Pregnancy-related visits") +
  annotate("text", x=as.Date("2015-01-01"), y=0.37, label="b", fontface="bold", size=15)
diar_ED <- ggplot(data=FINAL_summarized,aes(x=date,y=Diarrheal_Visits_Patient_Zip))+
  geom_point() +
  theme_bw() +
  theme(panel.grid.minor=element_blank()) +
  xlab("Date") + ylab("") + labs(title="Diarrhea-related visits") +
  annotate("text", x=as.Date("2015-01-01"), y=0.41, label="c", fontface="bold", size=15)
temp <- ggplot(data=FINAL_summarized,aes(x=date,y=tmean))+
  geom_point() +
  theme_bw() +
  theme(panel.grid.minor=element_blank()) +
  ylab("Average temperature (C)") + xlab("") + labs(title="Temperature (Daymet)") +
  annotate("text", x=as.Date("2015-01-01"), y=25, label="d", fontface="bold", size=15)
# prcp <- ggplot(data=FINAL_summarized,aes(x=date,y=prcp))+
#   geom_point() +
#   theme_bw() +
#   theme(panel.grid.minor=element_blank()) +
#   ylab("Average precipitation (mm)") + xlab("Date") + labs(title="Precipitation (Daymet)")

##### HRRR #####
### Home and activty colored
samarth_home_summarized <- aggregate(samarth_home3[,c("date","tmp_C","prcp")], by=list(samarth_home3$date), FUN=mean)
samarth_act_summarized <- aggregate(samarth_act3[,c("date","tmp_C","prcp")], by=list(samarth_act3$date), FUN=mean)
samarth_summarized <- merge(samarth_home_summarized[,-1], samarth_act_summarized[,-1],by="date")
names(samarth_summarized) <- c("date","tmp_C_home","prcp_home","tmp_C_act","prcp_act")
hrrr_nolegend <- ggplot(data=samarth_summarized)+
  geom_point(aes(x=date,y=tmp_C_home,color="Home")) +
  geom_point(aes(x=date,y=tmp_C_act,color="Activity"),alpha=0.75) +
  scale_color_manual(values=c("Home"="blue4", "Activity"="lightblue3"), name="Exposure") +
  theme_bw() +
  theme(panel.grid.minor=element_blank(),legend.position="none") +
  ylab("Average temperature (C)") + xlab("") + labs(title="Temperature (HRRR)") +
  annotate("text", x=as.Date("2015-01-01"), y=25, label="e", fontface="bold", size=15)
hrrr <- ggplot(data=samarth_summarized)+
  geom_point(aes(x=date,y=tmp_C_home,color="Home")) +
  geom_point(aes(x=date,y=tmp_C_act,color="Activity"),alpha=0.75) +
  scale_color_manual(values=c("Home"="blue4", "Activity"="lightblue3"), name="Exposure") +
  theme_bw() +
  theme(panel.grid.minor=element_blank()) +
  ylab("Average temperature (C)") + xlab("") + labs(title="Temperature (HRRR)")
hrrr_legend <- cowplot::get_legend(hrrr)


##### Plot Together #####
library(gridExtra)
grid.arrange(all_ED,temp,preg_ED,hrrr_nolegend,diar_ED,hrrr_legend,ncol=2)



####### 2. Daymet Data Models #######

##### 2.1. All Visits Model #####
#### Prepare Data ####
### Separate zctas into separate elements of a list ###
#that zcta I am not including
zctas <- as.character(unique(FINAL[!FINAL$ZCTA%in%c(24130),]$ZCTA))
data <- lapply(zctas, function(x) FINAL[FINAL$ZCTA==x,])
names(data) <- zctas
head(data[[1]]) #ordered by date
for(i in seq(data)) {
  data[[i]]$time <- c(1:nrow(data[[i]])) 
}

### Function to compute quasi AIC ###
fqaic <- function(model) {
  loglik <- sum(dpois(model$y,model$fitted.values,log=TRUE))
  phi <- summary(model)$dispersion
  qaic <- -2*loglik + 2*summary(model)$df[3]*phi
  return(qaic)
}


### ZCTA-level Threshold Variables ###
### Precipitation quantiles
precip_quantiles <- numeric()
for(i in seq(data)) {
  precip_quantiles[[i]] <- quantile(data[[i]]$prcp, 0.95)
}
precip_quantiles[[1]]
precip_quantiles[[100]]

### Determining absolute threshold (in mm) - 90% percentile average and 99% percentile average
# 90% quantiles
precip_quantiles_90 <- numeric()
for(i in seq(data)) {
  precip_quantiles_90[[i]] <- quantile(data[[i]]$prcp, 0.90)
}
summary(precip_quantiles_90) # mean = 11.77
# 99% quantiles
precip_quantiles_99 <- vector()
for(i in seq(data)) {
  precip_quantiles_99[[i]] <- quantile(data[[i]]$prcp, 0.99)
}
summary(precip_quantiles_99)# mean = 39.90

### Indicator variable for 95 percentile over 28 days AND over 11.77mm in 24h or 39.90mm over 48 hours
library(data.table)
for(i in seq_along(data)) {
  setDT(data[[i]])
  
  # 48-hour precip sum
  prcp_48hr <- frollsum(data[[i]]$prcp, n=2, fill=0)
  # Absolute rainfall thresholds test for each day
  absolute_threshold <- (data[[i]]$prcp > 11.77) | (prcp_48hr > 39.90)
  
  # Percentile rainfall thresholds test for each day
  percentile_threshold <- data[[i]]$prcp >= precip_quantiles[[i]]
  
  # Combine threshols:percentile threshold AND absolute threshold
  combined_threshold <- percentile_threshold & absolute_threshold
  
  # If any of the previous 28 days met the combined threshold, set to 1, else 0
  data[[i]][, ("prcp_95_28") := ifelse(frollsum(shift(combined_threshold, n=1, fill=0), n=28, fill=0)>0, 1, 0)]
  
}
head(data[[5]][,c("prcp","prcp_95_28")],30)


#### First-Stage Models ####
library(dlnm)
library(splines)
source("findmin.R")

### Prepare cross-basis elements ###
ranges <- t(sapply(data,function(x) range(x$tmean,na.rm=T))) # Temperature min and max at each zcta
bound <- colMeans(ranges)
lag <- 28
lagknots <- logknots(lag, nk=2)
arglag <- list(fun="ns",knots=lagknots)
varknots <- rowMeans(sapply(data,function(x) equalknots(x$tmean,fun="ns",nk=2)))
argvar <- list(fun="ns",knots=varknots,bound=bound)

### Objects to store the results
models <- list()
qaic <- 0
predictions <- list()
yall <- matrix(NA,length(zctas),3,dimnames=list(zctas,paste("b",seq(3),sep="")))
Sall <- vector("list",length(zctas))
names(Sall) <- zctas
cen <- numeric(length(zctas))
precip_coefs <- matrix(NA, length(zctas), 1, dimnames=list(zctas, "est"))
precip_var  <- vector("list",length(zctas))
names(precip_var) <- zctas

### Run the models and store results
for(i in zctas) {
  
  print(paste0(i, ", ", which(zctas==i), " zcta of ", length(zctas)," --> ", round(which(zctas==i)/length(zctas)*100,3), "% done." ))
  
  sub <- data[[i]]

  cb <- crossbasis(sub$tmean,lag=lag,argvar=argvar,arglag=arglag)
  model <- glm(Total_Visits_Patient_Zip ~ cb + DOW + ns(time, df=4*9) + prcp_95_28, family="quasipoisson", data=sub)
  
  models[[i]] <- model
  qaic[i] <- fqaic(model)
  predictions[[i]] <- crossreduce(cb, model,cen=findmin(cb,model))
  yall[i,] <- coef(predictions[[i]])
  Sall[[i]] <- vcov(predictions[[i]])
  cen[i]<- findmin(cb,model)
  
  precip_coefs[i, 1] <- coef(model)["prcp_95_28"]
  precip_var[[i]]  <- vcov(model)["prcp_95_28","prcp_95_28"]
  
}

#### Second-Stage Models ####
library(mvmeta)
### Multivariate meta-analytical model - Temp effect ###
method <- "reml"
mvall <- mvmeta(yall~1,Sall,method=method)
summary(mvall)

### Basis and predictions ####
## onebasis to create basis variables for prediction (attributes from 1st stage cross basis)
xvar <- seq(bound[1],bound[2],by=0.1)
bvar <- do.call("onebasis",c(list(x=xvar),attr(cb,"argvar")))
xlag <- 0:280/10
blag <- do.call("onebasis",c(list(x=xlag),attr(cb,"arglag")))

## First stage predictions (one per zcta)
regall <- lapply(seq(nrow(yall)),
                 function(i) crosspred(bvar,coef=yall[i,],vcov=Sall[[i]],model.link="log",cen=-12.6))
summary(regall)

## Second stage prediction (only one - the meta-analytical model)
cpall <- crosspred(bvar,coef=coef(mvall),vcov=vcov(mvall),
                   model.link="log",by=0.1,from=bound[1],to=bound[2],
                   cen=-12.6)
summary(cpall)

### MMT ###
cpall$predvar[which.min(cpall$allRRfit)] #-12.6
which.min(cpall$allRRfit)

### Overall exposure-response curve ###
plot(cpall,type="n",ylab="RR",ylim=c(0.9,1.6),xlim=c(-12.01,29.36),xlab="Temperature (C)",cex.axis=1.2,cex.lab=1.2)
for(i in seq(regall)) lines(regall[[i]],ptype="overall",col="grey50",lwd=0.5)
abline(h=1,col="black")
lines(cpall,col="red3",ci="lines",lwd=2.5)
legend ("topleft",c("Pooled dose-response","1st stage zcta-specific"),
        lty=c(1,2),lwd=2,col=c("red3","grey50"))


### Data for Table 1 ###
## MMT
which.min(cpall$allRRfit)

## RR at 10th and 99th percentile
## RR at 10th and 90th percentiles
round(cpall$allRRfit[as.character(round(quantile(FINAL$tmean,0.1),1))],2)
round(cpall$allRRlow[as.character(round(quantile(FINAL$tmean,0.1),1))],2)
round(cpall$allRRhigh[as.character(round(quantile(FINAL$tmean,0.1),1))],2)
round(cpall$allRRfit[as.character(round(quantile(FINAL$tmean,0.9),1))],2)
round(cpall$allRRlow[as.character(round(quantile(FINAL$tmean,0.9),1))],2)
round(cpall$allRRhigh[as.character(round(quantile(FINAL$tmean,0.9),1))],2)

## I2
summary(mvall)

### Multivariate meta-analytical model - Prcp effect ###
method <- "reml"
mvprecip <- mvmeta(precip_coefs~1,precip_var,method=method)
summary(mvprecip)

### Gather all RRs (exponentiated estimate), p values and CIs in a dataframe ###
precip_meta_results <- data.frame(model="Daymet-All",
                                  RR=exp(summary(mvprecip)$coefficients[,1]),
                                  CI_min=exp(summary(mvprecip)$coefficients[,5]),
                                  CI_max=exp(summary(mvprecip)$coefficients[,6]),
                                  p.value=summary(mvprecip)$coefficients[,4])

### Plot
library(ggplot2)
ggplot(precip_meta_results, aes(x=RR, y=model)) +
  geom_vline(xintercept=1, linetype="dashed") +
  geom_errorbarh(aes(xmin=CI_min, xmax=CI_max)) +
  geom_point(size=2) +
  scale_x_continuous(breaks=seq(0.975,1.025,by=0.025),limits=c(0.975, 1.025)) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank()) +
  labs(title="Pooled effect of precipitation indicator variables",
       x = "Relative Risk (RR)", y = "")



##### 2.2. Region-Level Data Aggregation #####
#### Number of visits: sum ####
FINAL_VDH <- aggregate(FINAL[,c("Total_Visits_Patient_Zip","Pregnancy_Related_Visits_Patient_Zip","Diarrheal_Visits_Patient_Zip")],
                       by=list(FINAL$VDH_Health_Region,FINAL$date), FUN=sum)
FINAL_VDH$VDH_Health_Region <- FINAL_VDH$Group.1
FINAL_VDH$date <- FINAL_VDH$Group.2
FINAL_VDH$Group.1 <- NULL
FINAL_VDH$Group.2 <- NULL
head(FINAL_VDH)

#### tmean: average ####
FINAL_VDH2 <- aggregate(FINAL[,c("tmean")], by=list(FINAL$VDH_Health_Region,FINAL$date), FUN=mean)
FINAL_VDH2$VDH_Health_Region <- FINAL_VDH2$Group.1
FINAL_VDH2$date <- FINAL_VDH2$Group.2
FINAL_VDH2$tmean <- FINAL_VDH2$x
FINAL_VDH2$Group.1 <- NULL
FINAL_VDH2$Group.2 <- NULL
FINAL_VDH2$x <- NULL
head(FINAL_VDH2)

#### prcp: area-weighted average ####
### Read zctas shapefiles and calculate region size ###
library(sf)
zctas_sf <- st_read("DATA/2020_zcta_census/tl_2020_us_zcta520/tl_2020_us_zcta520.shp")
head(zctas_sf)
## Area of each zcta
zctas_sf$area <- st_area(zctas_sf)
zctas_sf$area_m2 <- as.numeric(zctas_sf$area)
head(zctas_sf)
# Keep only zctas in our dataset
zctas_sf_va <- st_drop_geometry(zctas_sf[zctas_sf$ZCTA5CE20 %in% zctas,c("ZCTA5CE20","area_m2")])
zctas_sf_va$ZCTA <- zctas_sf_va$ZCTA5CE20
zctas_sf_va$ZCTA5CE20 <- NULL
head(zctas_sf_va)
nrow(zctas_sf_va) #883 (means 12 zctas not here - move forard without those)
# Add region variables and sum area to each reaion
zctas_sf_va_region <- merge(zctas_sf_va,FINAL[!duplicated(FINAL$ZCTA),c("ZCTA","VDH_Health_Region")],by.x="ZCTA", by.y="ZCTA", al.x=TRUE)
head(zctas_sf_va_region)
VDH_Regions_area <- aggregate(zctas_sf_va_region[,c("area_m2")],
                              by=list(zctas_sf_va_region$VDH_Health_Region), FUN=sum)
VDH_Regions_area$VDH_Health_Region <- VDH_Regions_area$Group.1
VDH_Regions_area$Group.1 <- NULL
VDH_Regions_area$area_m2 <- VDH_Regions_area$x
VDH_Regions_area$x <- NULL
head(VDH_Regions_area)

### Calculate area-weight for each zcta ###
# One row per zcta with region, zcta area and region area
zctas_sf_va_region <- merge(zctas_sf_va_region,VDH_Regions_area,by="VDH_Health_Region",all.x=TRUE)
zctas_sf_va_region$area_m2_zcta <- zctas_sf_va_region$area_m2.x
zctas_sf_va_region$area_m2.x <- NULL
zctas_sf_va_region$area_m2_region <- zctas_sf_va_region$area_m2.y
zctas_sf_va_region$area_m2.y <- NULL
head(zctas_sf_va_region)
# ZCTA area weight
zctas_sf_va_region$area_weight <- zctas_sf_va_region$area_m2_zcta/zctas_sf_va_region$area_m2_region
summary(zctas_sf_va_region$area_weight)

### Weighted prcp per zcta ###
# Add area weight to FINAL
FINAL_VDH3 <- merge(FINAL,zctas_sf_va_region[,c("ZCTA","area_weight")], by="ZCTA",all.x=TRUE)
sum(is.na(FINAL_VDH3$area_weight)) #42731 - the ones with no matching shapefile
FINAL_VDH3 <- FINAL_VDH3[!is.na(FINAL_VDH3$area_weight),]
# Weighted prcp
FINAL_VDH3$prcp_weighted <- FINAL_VDH3$prcp * FINAL_VDH3$area_weight
summary(FINAL_VDH3$prcp_weighted)
boxplot(FINAL_VDH3$prcp_weighted)

### Aggregation to region level: sum ###
FINAL_VDH3 <- aggregate(FINAL_VDH3[,c("prcp_weighted")], by=list(FINAL_VDH3$VDH_Health_Region,FINAL_VDH3$date), FUN=sum)
FINAL_VDH3$VDH_Health_Region <- FINAL_VDH3$Group.1
FINAL_VDH3$date <- FINAL_VDH3$Group.2
FINAL_VDH3$prcp <- FINAL_VDH3$x
FINAL_VDH3$Group.1 <- NULL
FINAL_VDH3$Group.2 <- NULL
FINAL_VDH3$x <- NULL
head(FINAL_VDH3)
summary(FINAL_VDH3$prcp)

#### Merge all and add DOW ####
FINAL_VDH <- merge(FINAL_VDH,FINAL_VDH2,by=c("VDH_Health_Region","date"))
FINAL_VDH <- merge(FINAL_VDH,FINAL_VDH3,by=c("VDH_Health_Region","date"))
FINAL_VDH <- merge(FINAL_VDH,FINAL[!duplicated(FINAL$date),c("date","DOW")],by="date",all.x=TRUE)
head(FINAL_VDH)


#### Prepare Data ####
### Separate zctas into separate elements of a list ###
regions <- as.character(unique(FINAL_VDH$VDH_Health_Region))
data_region <- lapply(regions, function(x) FINAL_VDH[FINAL_VDH$VDH_Health_Region==x,])
names(data_region) <- regions
head(data_region[[1]]) #ordered by date
for(i in seq(data_region)) {
  data_region[[i]]$time <- c(1:nrow(data_region[[i]])) 
}
m <- length(regions)

### FUNCTION TO COMPUTE THE Q-AIC ###
fqaic <- function(model) {
  loglik <- sum(dpois(model$y,model$fitted.values,log=TRUE))
  phi <- summary(model)$dispersion
  qaic <- -2*loglik + 2*summary(model)$df[3]*phi
  return(qaic)
}

### Region-level Threshold Variables ###
### Precipitation quantiles
precip_quantiles_region <- numeric()
for(i in seq(data_region)) {
  precip_quantiles_region[[i]] <- quantile(data_region[[i]]$prcp, 0.95) 
}
precip_quantiles_region[[1]]

### Indicator variables for 95 percentile over 28 days AND over 11.77mm in 24h or 39.90mm over 48 hours
library(data.table)
for(i in seq_along(data_region)) {
  setDT(data_region[[i]])
  
  # 48-hour precip sum
  prcp_48hr <- frollsum(data_region[[i]]$prcp, n=2, fill=0)
  
  # Absolute rainfall thresholds test for each day
  absolute_threshold <- (data_region[[i]]$prcp > 11.77) | (prcp_48hr > 39.90)
  
  # Percentile rainfall thresholds test for each day
  percentile_threshold <- data_region[[i]]$prcp >= precip_quantiles_region[[i]]
  
  # Combine threshols:percentile threshold AND absolute threshold
  combined_threshold <- percentile_threshold & absolute_threshold
  
  # If any of the previous 28 days met the combined threshold, set to 1, else 0
  data_region[[i]][, ("prcp_95_28") := ifelse(frollsum(shift(combined_threshold, n=1, fill=0), n=28, fill=0)>0, 1, 0)]
  
}
head(data_region[[5]][,c("prcp","prcp_95_28")],30)


##### 2.3. Pregnancy-Related Visits Model #####
#### First-Stage Models ####
library(dlnm)
library(splines)
source("findmin.R")

### Prepare cross-basis elements ###
ranges_preg <- t(sapply(data_region,function(x) range(x$tmean,na.rm=T))) # Temperature min and max at each zcta
bound_preg <- colMeans(ranges_preg)
lag <- 28
lagknots <- logknots(lag, nk=2)
arglag <- list(fun="ns",knots=lagknots)
varknots <- rowMeans(sapply(data,function(x) equalknots(x$tmean,fun="ns",nk=2)))
argvar <- list(fun="ns",knots=varknots,bound=bound)

### Objects to store the results
models_preg <- list()
qaic_preg <- 0
predictions_preg <- list()
yall_preg <- matrix(NA,length(regions),3,dimnames=list(regions,paste("b",seq(3),sep="")))
Sall_preg <- vector("list",length(regions))
names(Sall_preg) <- regions
cen_preg <- numeric(length(regions))
precip_coefs_preg <- matrix(NA, length(regions), 1, dimnames=list(regions, "est"))
precip_var_preg  <- vector("list",length(regions))
names(precip_var_preg) <- regions

### Run the models and store results
for(i in regions) {
  
  print(i)
  
  sub <- data_region[[i]]
  
  cb_preg <- crossbasis(sub$tmean,lag=lag,argvar=argvar,arglag=arglag)
  model <- glm(Pregnancy_Related_Visits_Patient_Zip ~ cb_preg + DOW + ns(time, df=4*9) + prcp_95_28 + offset(log(Total_Visits_Patient_Zip)),
               family="quasipoisson", data=sub)

  models_preg[[i]] <- model
  qaic_preg[i] <- fqaic(model)
  predictions_preg[[i]] <- crossreduce(cb_preg, model,cen=findmin(cb_preg,model))
  yall_preg[i,] <- coef(predictions_preg[[i]])
  Sall_preg[[i]] <- vcov(predictions_preg[[i]])
  cen_preg[i]<- findmin(cb_preg,model)
  
  precip_coefs_preg[i, 1] <- coef(model)["prcp_95_28"]
  precip_var_preg[[i]]  <- vcov(model)["prcp_95_28","prcp_95_28"]
  
}

#### Second-Stage Models ####
library(mvmeta)
### Multivariate meta-analytical model - Temp effect ###
method <- "reml"
mvall_preg <- mvmeta(yall_preg~1,Sall_preg,method=method)
summary(mvall_preg)

### Basis and predictions ####
## onebasis to create basis variables for prediction (attributes from 1st stage cross basis)
xvar_preg <- seq(bound_preg[1],bound_preg[2],by=0.1)
bvar_preg <- do.call("onebasis",c(list(x=xvar_preg),attr(cb_preg,"argvar")))
xlag_preg <- 0:280/10
blag_preg <- do.call("onebasis",c(list(x=xlag_preg),attr(cb_preg,"arglag")))

## First stage predictions (one per zcta)
regall_preg <- lapply(seq(nrow(yall_preg)),
                      function(i) crosspred(bvar_preg,coef=yall_preg[i,],vcov=Sall_preg[[i]],model.link="log",cen=15.2))
summary(regall_preg)

## Second stage prediction (only one - the meta-analytical model)
cpall_preg <- crosspred(bvar_preg,coef=coef(mvall_preg),vcov=vcov(mvall_preg),
                   model.link="log",by=0.1,from=bound_preg[1],to=bound_preg[2],
                   cen=15.2)
summary(cpall_preg)

### MMT ###
cpall_preg$predvar[which.min(cpall_preg$allRRfit)] #15.2
which.min(cpall_preg$allRRfit)

### Overall exposure-response curve ###
plot(cpall_preg,type="n",ylab="RR",ylim=c(0.9,1.6),xlim=c(-12.01,29.36),xlab="Temperature (C)",cex.axis=1.2,cex.lab=1.2)
for(i in seq(regall_preg)) lines(regall_preg[[i]],ptype="overall",col="grey50",lwd=0.5)
abline(h=1,col="black")
lines(cpall_preg,col="red3",ci="lines",lwd=2.5)
legend ("topleft",c("Pooled dose-response","1st stage zcta-specific"),
        lty=c(1,2),lwd=2,col=c("red3","grey50"))


### Data for Table 1 ###
## MMT
which.min(cpall_preg$allRRfit)

## RR at 10th and 99th percentile
## RR at 10th and 90th percentiles
round(cpall_preg$allRRfit[as.character(round(quantile(FINAL$tmean,0.1),1))],2)
round(cpall_preg$allRRlow[as.character(round(quantile(FINAL$tmean,0.1),1))],2)
round(cpall_preg$allRRhigh[as.character(round(quantile(FINAL$tmean,0.1),1))],2)
round(cpall_preg$allRRfit[as.character(round(quantile(FINAL$tmean,0.9),1))],2)
round(cpall_preg$allRRlow[as.character(round(quantile(FINAL$tmean,0.9),1))],2)
round(cpall_preg$allRRhigh[as.character(round(quantile(FINAL$tmean,0.9),1))],2)

## I2
summary(mvall_preg)

### Multivariate meta-analytical model - Prcp effect ###
method <- "reml"
mvprecip_preg <- mvmeta(precip_coefs_preg~1,precip_var_preg,method=method)
summary(mvprecip_preg)

### Gather all RRs (exponentiated estimate), p values and CIs in a dataframe ###
precip_meta_results_preg <- data.frame(model="Daymet-Pregnancy",
                                  RR=exp(summary(mvprecip_preg)$coefficients[,1]),
                                  CI_min=exp(summary(mvprecip_preg)$coefficients[,5]),
                                  CI_max=exp(summary(mvprecip_preg)$coefficients[,6]),
                                  p.value=summary(mvprecip_preg)$coefficients[,4])

### Plot
library(ggplot2)
ggplot(precip_meta_results_preg, aes(x=RR, y=model)) +
  geom_vline(xintercept=1, linetype="dashed") +
  geom_errorbarh(aes(xmin=CI_min, xmax=CI_max)) +
  geom_point(size=2) +
  scale_x_continuous(breaks=seq(0.975,1.025,by=0.025),limits=c(0.975, 1.025)) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank()) +
  labs(title="Pooled effect of precipitation indicator variables",
       x = "Relative Risk (RR)", y = "")



##### 2.4. Diarrhea-Related Visits Model #####
#### First-Stage Models ####
library(dlnm)
library(splines)
source("findmin.R")

### Prepare cross-basis elements ###
ranges_diar <- t(sapply(data_region,function(x) range(x$tmean,na.rm=T))) # Temperature min and max at each zcta
bound_diar <- colMeans(ranges_diar)
lag <- 28
lagknots <- logknots(lag, nk=2)
arglag <- list(fun="ns",knots=lagknots)
varknots <- rowMeans(sapply(data,function(x) equalknots(x$tmean,fun="ns",nk=2)))
argvar <- list(fun="ns",knots=varknots,bound=bound)

### Objects to store the results
models_diar <- list()
qaic_diar <- 0
predictions_diar <- list()
yall_diar <- matrix(NA,length(regions),3,dimnames=list(regions,paste("b",seq(3),sep="")))
Sall_diar <- vector("list",length(regions))
names(Sall_diar) <- regions
cen_diar <- numeric(length(regions))
precip_coefs_diar <- matrix(NA, length(regions), 1, dimnames=list(regions, "est"))
precip_var_diar  <- vector("list",length(regions))
names(precip_var_diar) <- regions

### Run the models and store results
for(i in regions) {
  
  print(i)
  
  sub <- data_region[[i]]
  
  cb_diar <- crossbasis(sub$tmean,lag=lag,argvar=argvar,arglag=arglag)
  model <- glm(Diarrheal_Visits_Patient_Zip ~ cb_diar + DOW + ns(time, df=4*9) + prcp_95_28 + offset(log(Total_Visits_Patient_Zip)),
               family="quasipoisson", data=sub)
  
  models_diar[[i]] <- model
  qaic_diar[i] <- fqaic(model)
  predictions_diar[[i]] <- crossreduce(cb_diar, model,cen=findmin(cb_diar,model))
  yall_diar[i,] <- coef(predictions_diar[[i]])
  Sall_diar[[i]] <- vcov(predictions_diar[[i]])
  cen_diar[i]<- findmin(cb_diar,model)
  
  precip_coefs_diar[i, 1] <- coef(model)["prcp_95_28"]
  precip_var_diar[[i]]  <- vcov(model)["prcp_95_28","prcp_95_28"]
  
}

#### Second-Stage Models ####
library(mvmeta)
### Multivariate meta-analytical model - Temp effect ###
method <- "reml"
mvall_diar <- mvmeta(yall_diar~1,Sall_diar,method=method)
summary(mvall_diar)

### Basis and predictions ####
## onebasis to create basis variables for prediction (attributes from 1st stage cross basis)
xvar_diar <- seq(bound_diar[1],bound_diar[2],by=0.1)
bvar_diar <- do.call("onebasis",c(list(x=xvar_diar),attr(cb_diar,"argvar")))
xlag_diar <- 0:280/10
blag_diar <- do.call("onebasis",c(list(x=xlag_diar),attr(cb_diar,"arglag")))

## First stage predictions (one per zcta)
regall_diar <- lapply(seq(nrow(yall_diar)),
                      function(i) crosspred(bvar_diar,coef=yall_diar[i,],vcov=Sall_diar[[i]],model.link="log",cen=-12.2))
summary(regall_diar)

## Second stage prediction (only one - the meta-analytical model)
cpall_diar <- crosspred(bvar_diar,coef=coef(mvall_diar),vcov=vcov(mvall_diar),
                        model.link="log",by=0.1,from=bound_diar[1],to=bound_diar[2],
                        cen=-12.2)
summary(cpall_diar)

### MMT ###
cpall_diar$predvar[which.min(cpall_diar$allRRfit)] #-12.2
which.min(cpall_diar$allRRfit)

### Overall exposure-response curve ###
plot(cpall_diar,type="n",ylab="RR",ylim=c(0.9,1.6),xlim=c(-12.01,29.36),xlab="Temperature (C)",cex.axis=1.2,cex.lab=1.2)
for(i in seq(regall_diar)) lines(regall_diar[[i]],ptype="overall",col="grey50",lwd=0.5)
abline(h=1,col="black")
lines(cpall_diar,col="red3",ci="lines",lwd=2.5)
legend ("topleft",c("Pooled dose-response","1st stage zcta-specific"),
        lty=c(1,2),lwd=2,col=c("red3","grey50"))


### Data for Table 1 ###
## MMT
which.min(cpall_diar$allRRfit)

## RR at 10th and 99th percentile
## RR at 10th and 90th percentiles
round(cpall_diar$allRRfit[as.character(round(quantile(FINAL$tmean,0.1),1))],2)
round(cpall_diar$allRRlow[as.character(round(quantile(FINAL$tmean,0.1),1))],2)
round(cpall_diar$allRRhigh[as.character(round(quantile(FINAL$tmean,0.1),1))],2)
round(cpall_diar$allRRfit[as.character(round(quantile(FINAL$tmean,0.9),1))],2)
round(cpall_diar$allRRlow[as.character(round(quantile(FINAL$tmean,0.9),1))],2)
round(cpall_diar$allRRhigh[as.character(round(quantile(FINAL$tmean,0.9),1))],2)

## I2
summary(mvall_diar)

### Multivariate meta-analytical model - Prcp effect ###
method <- "reml"
mvprecip_diar <- mvmeta(precip_coefs_diar~1,precip_var_diar,method=method)
summary(mvprecip_diar)

### Gather all RRs (exponentiated estimate), p values and CIs in a dataframe ###
precip_meta_results_diar <- data.frame(model="Daymet-Diarrhea",
                                       RR=exp(summary(mvprecip_diar)$coefficients[,1]),
                                       CI_min=exp(summary(mvprecip_diar)$coefficients[,5]),
                                       CI_max=exp(summary(mvprecip_diar)$coefficients[,6]),
                                       p.value=summary(mvprecip_diar)$coefficients[,4])

### Plot
library(ggplot2)
ggplot(precip_meta_results_diar, aes(x=RR, y=model)) +
  geom_vline(xintercept=1, linetype="dashed") +
  geom_errorbarh(aes(xmin=CI_min, xmax=CI_max)) +
  geom_point(size=2) +
  scale_x_continuous(breaks=seq(0.975,1.025,by=0.025),limits=c(0.975, 1.025)) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank()) +
  labs(title="Pooled effect of precipitation indicator variables",
       x = "Relative Risk (RR)", y = "")





####### 3. HRRR Data Models #######
##### 3.1. All Visits Models #####
#### Prepare Data ####
### Separate zctas into separate elements of a list ###
### Home
#that zcta I am not including
zctas_home <- as.character(unique(samarth_home3[!samarth_home3$zcta%in%c(24130),]$zcta))
data_home <- lapply(zctas_home, function(x) samarth_home3[samarth_home3$zcta==x,])
names(data_home) <- zctas_home
head(data_home[[1]]) #ordered by date
for(i in seq(data_home)) {
  data_home[[i]]$time <- c(1:nrow(data_home[[i]])) 
}

### Activity
zctas_act <- as.character(unique(samarth_act3[!samarth_act3$zcta%in%c(24130),]$zcta))
data_act <- lapply(zctas_act, function(x) samarth_act3[samarth_act3$zcta==x,])
names(data_act) <- zctas_act
head(data_act[[1]]) #ordered by date
for(i in seq(data_act)) {
  data_act[[i]]$time <- c(1:nrow(data_act[[i]])) 
}


### ZCTA-level Threshold Variables ###
### Precipitation quantiles
### Home
precip_quantiles_home <- numeric()
for(i in seq(data_home)) {
  precip_quantiles_home[[i]] <- quantile(data_home[[i]]$prcp, 0.95) 
}
precip_quantiles_home[[1]]
precip_quantiles_home[[109]]

### Activity
precip_quantiles_act <- numeric()
for(i in seq(data_act)) {
  precip_quantiles_act[[i]] <- quantile(data_act[[i]]$prcp, 0.95) 
}
precip_quantiles_act[[1]]
precip_quantiles_act[[109]]

### Indicator variable for 95 percentile over 28 days AND over 11.77mm in 24h or 39.90mm over 48 hours
library(data.table)
### Home
for(i in seq_along(data_home)) {
  setDT(data_home[[i]])
  
  # 48-hour precip sum
  prcp_48hr <- frollsum(data_home[[i]]$prcp, n=2, fill=0)
  # Absolute rainfall thresholds test for each day
  absolute_threshold <- (data_home[[i]]$prcp > 11.77) | (prcp_48hr > 39.90)
  
  # Percentile rainfall thresholds test for each day
  percentile_threshold <- data_home[[i]]$prcp >= precip_quantiles_home[[i]]
  
  # Combine threshols:percentile threshold AND absolute threshold
  combined_threshold <- percentile_threshold & absolute_threshold
  
  # If any of the previous 28 days met the combined threshold, set to 1, else 0
  data_home[[i]][, ("prcp_95_28") := ifelse(frollsum(shift(combined_threshold, n=1, fill=0), n=28, fill=0)>0, 1, 0)]
  
}

### Activity
for(i in seq_along(data_act)) {
  setDT(data_act[[i]])
  
  # 48-hour precip sum
  prcp_48hr <- frollsum(data_act[[i]]$prcp, n=2, fill=0)
  # Absolute rainfall thresholds test for each day
  absolute_threshold <- (data_act[[i]]$prcp > 11.77) | (prcp_48hr > 39.90)
  
  # Percentile rainfall thresholds test for each day
  percentile_threshold <- data_act[[i]]$prcp >= precip_quantiles_act[[i]]
  
  # Combine threshols:percentile threshold AND absolute threshold
  combined_threshold <- percentile_threshold & absolute_threshold
  
  # If any of the previous 28 days met the combined threshold, set to 1, else 0
  data_act[[i]][, ("prcp_95_28") := ifelse(frollsum(shift(combined_threshold, n=1, fill=0), n=28, fill=0)>0, 1, 0)]
  
}


#### First-Stage Models ####
library(dlnm)
library(splines)
source("findmin.R")

### Prepare cross-basis elements ###
### Home
ranges_home <- t(sapply(data_home,function(x) range(x$tmp_C,na.rm=T))) # Temperature min and max at each zcta
bound_home <- colMeans(ranges_home)
lag <- 28
lagknots_home <- logknots(lag, nk=2)
arglag_home <- list(fun="ns",knots=lagknots_home)
varknots_home <- rowMeans(sapply(data_home,function(x) equalknots(x$tmp_C,fun="ns",nk=2)))
argvar_home <- list(fun="ns",knots=varknots_home,bound=bound_home)

### Activity
ranges_act <- t(sapply(data_act,function(x) range(x$tmp_C,na.rm=T))) # Temperature min and max at each zcta
bound_act <- colMeans(ranges_act)
lag <- 28
lagknots_act <- logknots(lag, nk=2)
arglag_act <- list(fun="ns",knots=lagknots_act)
varknots_act <- rowMeans(sapply(data_act,function(x) equalknots(x$tmp_C,fun="ns",nk=2)))
argvar_act <- list(fun="ns",knots=varknots_act,bound=bound_act)

### Objects to store the results
### Home
# lists
models_home <- list()
qaic_home <- 0
predictions_home <- list()
yall_home <- matrix(NA,length(zctas_home),3,dimnames=list(zctas_home,paste("b",seq(3),sep="")))
Sall_home <- vector("list",length(zctas_home))
names(Sall_home) <- zctas_home
cen_home <- numeric(length(zctas_home))
precip_coefs_home <- matrix(NA, length(zctas_home), 1, dimnames=list(zctas_home, "est"))
precip_var_home  <- vector("list",length(zctas_home))
names(precip_var_home) <- zctas_home

### Activity
# lists
models_act <- list()
qaic_act <- 0
predictions_act <- list()
yall_act <- matrix(NA,length(zctas_act),3,dimnames=list(zctas_act,paste("b",seq(3),sep="")))
Sall_act <- vector("list",length(zctas_act))
names(Sall_act) <- zctas_act
cen_act <- numeric(length(zctas_act))
precip_coefs_act <- matrix(NA, length(zctas_act), 1, dimnames=list(zctas_act, "est"))
precip_var_act  <- vector("list",length(zctas_act))
names(precip_var_act) <- zctas_act

### Run the models and store results
### Home
for(i in zctas_home) {
  
  print(paste0(i, ", ", which(zctas_home==i), " zcta of ", length(zctas_home)," --> ", round(which(zctas_home==i)/length(zctas_home)*100,3), "% done." ))
  
  sub <- data_home[[i]]
  
  cb_home <- crossbasis(sub$tmp_C,lag=lag,argvar=argvar_home,arglag=arglag_home)
  model <- glm(Total_Visits_Patient_Zip ~ cb_home + DOW + ns(time, df=4*9) + prcp_95_28, family="quasipoisson", data=sub)
  
  models_home[[i]] <- model
  qaic_home[i] <- fqaic(model)
  predictions_home[[i]] <- crossreduce(cb_home, model,cen=findmin(cb_home,model))
  yall_home[i,] <- coef(predictions_home[[i]])
  Sall_home[[i]] <- vcov(predictions_home[[i]])
  cen_home[i]<- findmin(cb_home,model)
  
  precip_coefs_home[i, 1] <- coef(model)["prcp_95_28"]
  precip_var_home[[i]]  <- vcov(model)["prcp_95_28","prcp_95_28"]
  
}

### Activity
for(i in zctas_act) {
  
  print(paste0(i, ", ", which(zctas_act==i), " zcta of ", length(zctas_act)," --> ", round(which(zctas_act==i)/length(zctas_act)*100,3), "% done." ))
  
  sub <- data_act[[i]]
  
  cb_act <- crossbasis(sub$tmp_C,lag=lag,argvar=argvar_act,arglag=arglag_act)
  model <- glm(Total_Visits_Patient_Zip ~ cb_act + DOW + ns(time, df=4*9) + prcp_95_28, family="quasipoisson", data=sub)
  
  models_act[[i]] <- model
  qaic_act[i] <- fqaic(model)
  predictions_act[[i]] <- crossreduce(cb, model,cen=findmin(cb_act,model))
  yall_act[i,] <- coef(predictions_act[[i]])
  Sall_act[[i]] <- vcov(predictions_act[[i]])
  cen_act[i]<- findmin(cb_act,model)
  
  precip_coefs_act[i, 1] <- coef(model)["prcp_95_28"]
  precip_var_act[[i]]  <- vcov(model)["prcp_95_28","prcp_95_28"]
  
}


#### Second-Stage Models ####
library(mvmeta)
### Multivariate meta-analytical model - Temp effect ###
### Home
method <- "reml"
mvall_home <- mvmeta(yall_home~ 1, Sall_home, method=method)
summary(mvall_home)

### Activity
method <- "reml"
mvall_act <- mvmeta(yall_act~ 1, Sall_act, method=method)
summary(mvall_act)

### Basis and predictions ####
### Home
## onebasis to create basis variables for prediction (attributes from 1st stage cross basis)
xvar_home <- seq(bound_home[1],bound_home[2],by=0.1)
bvar_home <- do.call("onebasis",c(list(x=xvar_home),attr(cb_home,"argvar")))
xlag_home <- 0:280/10
blag_home <- do.call("onebasis",c(list(x=xlag_home),attr(cb_home,"arglag")))
## First stage predictions (one per zcta)
regall_home <- lapply(seq(nrow(yall_home)),
                      function(i) crosspred(bvar_home,coef=yall_home[i,],vcov=Sall_home[[i]],model.link="log",cen=-13))
summary(regall_home)
## Second stage prediction (only one - the meta-analytical model)
cpall_home <- crosspred(bvar_home,coef=coef(mvall_home),vcov=vcov(mvall_home),
                        model.link="log",by=0.1,from=bound_home[1],to=bound_home[2],
                        cen=-13 #the one below
)
summary(cpall_home)

### Activity
## onebasis to create basis variables for prediction (attributes from 1st stage cross basis)
xvar_act <- seq(bound_act[1],bound_act[2],by=0.1)
bvar_act <- do.call("onebasis",c(list(x=xvar_act),attr(cb_act,"argvar")))
xlag_act <- 0:280/10
blag_act <- do.call("onebasis",c(list(x=xlag_act),attr(cb_act,"arglag")))
## First stage predictions (one per zcta)
regall_act <- lapply(seq(nrow(yall_act)),
                     function(i) crosspred(bvar_act,coef=yall_act[i,],vcov=Sall_act[[i]],model.link="log",cen=-13))
summary(regall_act)
## Second stage prediction (only one per prcp indicator - the meta-analytical model)
cpall_act <- crosspred(bvar_act,coef=coef(mvall_act),vcov=vcov(mvall_act),
                       model.link="log",by=0.1,from=bound_act[1],to=bound_act[2],
                       cen=-13 #the one below
)
summary(cpall_act)

### MMT ###
### Home
cpall_home$predvar[which.min(cpall_home$allRRfit)] #-13
which.min(cpall_home$allRRfit)

### Activity
cpall_act$predvar[which.min(cpall_act$allRRfit)] #-13
which.min(cpall_act$allRRfit)


### Overall exposure-response curve ###
# ### Home
# plot(cpall_home,type="n",ylab="RR",ylim=c(0.9,1.6),xlim=c(-15,33),xlab="Temperature (C)",cex.axis=1.2,cex.lab=1.2)
# for(i in seq(regall_home)) lines(regall_home[[i]],ptype="overall",col="grey50",lwd=0.5)
# abline(h=1,col="black")
# lines(cpall_home,col="red3",ci="lines",lwd=2.5)
# legend ("topleft",c("Pooled dose-response","1st stage zcta-specific"),
#         lty=c(1,2),lwd=2,col=c("red3","grey50"))
# 
# ### Activity
# plot(cpall_act,type="n",ylab="RR",ylim=c(0.9,1.6),xlim=c(-15,33),xlab="Temperature (C)",cex.axis=1.2,cex.lab=1.2)
# for(i in seq(regall_act)) lines(regall_act[[i]],ptype="overall",col="grey50",lwd=0.5)
# abline(h=1,col="black")
# lines(cpall_act,col="red3",ci="lines",lwd=2.5)
# legend ("topleft",c("Pooled dose-response","1st stage zcta-specific"),
#         lty=c(1,2),lwd=2,col=c("red3","grey50"))

### Together
plot(cpall_home,col="blue4",ci="lines",lwd=5,ylab="RR",ylim=c(0.9,1.6),xlim=c(-15,33),xlab="Temperature (C)",main="All Visits",cex.axis=1.2,cex.lab=1.2)
lines(cpall_act,col=adjustcolor("lightblue3", alpha.f=1),ci="lines",lwd=2.5)
legend ("topleft",c("Home","Activity"),
        lty=c(1,1),lwd=c(5,2),col=c("blue4","lightblue3"))



### Data for Table 1 ###
## MMT
# Home
which.min(cpall_home$allRRfit)
# Activity
which.min(cpall_act$allRRfit)

## RR at 10th and 99th percentile
# Home
round(cpall_home$allRRfit[as.character(round(quantile(samarth_home3$tmp_C,0.1),1))],2)
round(cpall_home$allRRlow[as.character(round(quantile(samarth_home3$tmp_C,0.1),1))],2)
round(cpall_home$allRRhigh[as.character(round(quantile(samarth_home3$tmp_C,0.1),1))],2)
round(cpall_home$allRRfit[as.character(round(quantile(samarth_home3$tmp_C,0.9),1))],2)
round(cpall_home$allRRlow[as.character(round(quantile(samarth_home3$tmp_C,0.9),1))],2)
round(cpall_home$allRRhigh[as.character(round(quantile(samarth_home3$tmp_C,0.9),1))],2)

# Activity
round(cpall_act$allRRfit[as.character(round(quantile(samarth_act3$tmp_C,0.1),1))],2)
round(cpall_act$allRRlow[as.character(round(quantile(samarth_act3$tmp_C,0.1),1))],2)
round(cpall_act$allRRhigh[as.character(round(quantile(samarth_act3$tmp_C,0.1),1))],2)
round(cpall_act$allRRfit[as.character(round(quantile(samarth_act3$tmp_C,0.9),1))],2)
round(cpall_act$allRRlow[as.character(round(quantile(samarth_act3$tmp_C,0.9),1))],2)
round(cpall_act$allRRhigh[as.character(round(quantile(samarth_act3$tmp_C,0.9),1))],2)

## I2
summary(mvall_home)
summary(mvall_act)

### Multivariate meta-analytical model - Prcp effect ###
### Home
method <- "reml"
mvprecip_home <- mvmeta(precip_coefs_home ~ 1, precip_var_home, method=method)
summary(mvprecip_home)

### Activity
method <- "reml"
mvprecip_act <- mvmeta(precip_coefs_act ~ 1, precip_var_act, method=method)
summary(mvprecip_act)

### Gather all RRs (exponentiated estimate), p values and CIs in a dataframe ###
precip_meta_results_homeact <- data.frame(model=c("Home","Activity"),
                                  RR=c(exp(summary(mvprecip_home)$coefficients[,1]),
                                       exp(summary(mvprecip_act)$coefficients[,1])),
                                  CI_min=c(exp(summary(mvprecip_home)$coefficients[,5]),
                                           exp(summary(mvprecip_act)$coefficients[,5])),
                                  CI_max=c(exp(summary(mvprecip_home)$coefficients[,6]),
                                           exp(summary(mvprecip_act)$coefficients[,6])),
                                  p.value=c(summary(mvprecip_home)$coefficients[,4],
                                            summary(mvprecip_act)$coefficients[,4]))

### Plot
library(ggplot2)
ggplot(precip_meta_results_homeact, aes(x=RR, y=model)) +
  geom_vline(xintercept=1, linetype="dashed") +
  geom_errorbarh(aes(xmin=CI_min, xmax=CI_max)) +
  geom_point(size=2) +
  scale_x_continuous(breaks=seq(0.99,1.01,by=0.01),limits=c(0.99, 1.01)) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank()) +
  labs(title="Pooled effect of precipitation indicator variables",
       x = "Relative Risk (RR)", y = "")



##### 3.2. Region-Level Data Aggregation #####
#### Number of visits: sum ####
### Home
samarth_home_VDH1 <- aggregate(samarth_home3[,c("Total_Visits_Patient_Zip","Pregnancy_Related_Visits_Patient_Zip","Diarrheal_Visits_Patient_Zip")],
                               by=list(samarth_home3$VDH_Health_Region,samarth_home3$date), FUN=sum)
samarth_home_VDH1$VDH_Health_Region <- samarth_home_VDH1$Group.1
samarth_home_VDH1$date <- samarth_home_VDH1$Group.2
samarth_home_VDH1$Group.1 <- NULL
samarth_home_VDH1$Group.2 <- NULL
head(samarth_home_VDH1)

### Activity
samarth_act_VDH1 <- aggregate(samarth_act3[,c("Total_Visits_Patient_Zip","Pregnancy_Related_Visits_Patient_Zip","Diarrheal_Visits_Patient_Zip")],
                              by=list(samarth_act3$VDH_Health_Region,samarth_act3$date), FUN=sum)
samarth_act_VDH1$VDH_Health_Region <- samarth_act_VDH1$Group.1
samarth_act_VDH1$date <- samarth_act_VDH1$Group.2
samarth_act_VDH1$Group.1 <- NULL
samarth_act_VDH1$Group.2 <- NULL
head(samarth_act_VDH1)


#### tmean: average ####
### Home
samarth_home_VDH2 <- aggregate(samarth_home3[,c("tmp_C")], by=list(samarth_home3$VDH_Health_Region,samarth_home3$date), FUN=mean)
samarth_home_VDH2$VDH_Health_Region <- samarth_home_VDH2$Group.1
samarth_home_VDH2$date <- samarth_home_VDH2$Group.2
samarth_home_VDH2$tmp_C <- samarth_home_VDH2$x
samarth_home_VDH2$Group.1 <- NULL
samarth_home_VDH2$Group.2 <- NULL
samarth_home_VDH2$x <- NULL
head(samarth_home_VDH2)

### Activity
samarth_act_VDH2 <- aggregate(samarth_act3[,c("tmp_C")], by=list(samarth_act3$VDH_Health_Region,samarth_act3$date), FUN=mean)
samarth_act_VDH2$VDH_Health_Region <- samarth_act_VDH2$Group.1
samarth_act_VDH2$date <- samarth_act_VDH2$Group.2
samarth_act_VDH2$tmp_C <- samarth_act_VDH2$x
samarth_act_VDH2$Group.1 <- NULL
samarth_act_VDH2$Group.2 <- NULL
samarth_act_VDH2$x <- NULL
head(samarth_act_VDH2)


#### prcp: area-weighted average ####
### Read zctas shapefiles and calculate region size ###
library(sf)
zctas_sf <- st_read("2020_zcta_census/tl_2020_us_zcta520/tl_2020_us_zcta520.shp")
head(zctas_sf)
## Area of each zcta
zctas_sf$area <- st_area(zctas_sf)
zctas_sf$area_m2 <- as.numeric(zctas_sf$area)
head(zctas_sf)
# Keep only zctas in our dataset
zctas_sf_va <- st_drop_geometry(zctas_sf[zctas_sf$ZCTA5CE20 %in% zctas,c("ZCTA5CE20","area_m2")])
zctas_sf_va$zcta <- zctas_sf_va$ZCTA5CE20
zctas_sf_va$ZCTA5CE20 <- NULL
head(zctas_sf_va)
nrow(zctas_sf_va) #883 (1 mopre than samarths data)
# Add region variables and sum area to each reaion
zctas_sf_va_region <- merge(zctas_sf_va,samarth_home3[!duplicated(samarth_home3$zcta),c("zcta","VDH_Health_Region")],by="zcta", al.x=TRUE)
head(zctas_sf_va_region)
VDH_Regions_area <- aggregate(zctas_sf_va_region[,c("area_m2")],
                              by=list(zctas_sf_va_region$VDH_Health_Region), FUN=sum)
VDH_Regions_area$VDH_Health_Region <- VDH_Regions_area$Group.1
VDH_Regions_area$Group.1 <- NULL
VDH_Regions_area$area_m2 <- VDH_Regions_area$x
VDH_Regions_area$x <- NULL
head(VDH_Regions_area)

### Calculate area-weight for each zcta ###
# One row per zcta with region, zcta area and region area
zctas_sf_va_region <- merge(zctas_sf_va_region,VDH_Regions_area,by="VDH_Health_Region",all.x=TRUE)
zctas_sf_va_region$area_m2_zcta <- zctas_sf_va_region$area_m2.x
zctas_sf_va_region$area_m2.x <- NULL
zctas_sf_va_region$area_m2_region <- zctas_sf_va_region$area_m2.y
zctas_sf_va_region$area_m2.y <- NULL
head(zctas_sf_va_region)
# ZCTA area weight
zctas_sf_va_region$area_weight <- zctas_sf_va_region$area_m2_zcta/zctas_sf_va_region$area_m2_region
summary(zctas_sf_va_region$area_weight)
head(zctas_sf_va_region)

### Weighted prcp per zcta - Home ###
#Add area weight to homne data
samarth_home_VDH3 <- merge(samarth_home3,zctas_sf_va_region[,c("zcta","area_weight")], by="zcta",all.x=TRUE)
sum(is.na(samarth_home_VDH3$area_weight)) #32870 - the ones with no matching shapefile
samarth_home_VDH3 <- samarth_home_VDH3[!is.na(samarth_home_VDH3$area_weight),]
# Weighted prcp
samarth_home_VDH3$prcp_weighted <- samarth_home_VDH3$prcp * samarth_home_VDH3$area_weight
summary(samarth_home_VDH3$prcp_weighted)
boxplot(samarth_home_VDH3$prcp_weighted)

### Weighted prcp per zcta - Activity ###
#Add area weight to act data
samarth_act_VDH3 <- merge(samarth_act3,zctas_sf_va_region[,c("zcta","area_weight")], by="zcta",all.x=TRUE)
sum(is.na(samarth_act_VDH3$area_weight)) #32870 - the ones with no matching shapefile
samarth_act_VDH3 <- samarth_act_VDH3[!is.na(samarth_act_VDH3$area_weight),]
# Weighted prcp
samarth_act_VDH3$prcp_weighted <- samarth_act_VDH3$prcp * samarth_act_VDH3$area_weight
summary(samarth_act_VDH3$prcp_weighted)
boxplot(samarth_act_VDH3$prcp_weighted)

### Aggregation to region level: sum - Home ###
samarth_home_VDH3 <- aggregate(samarth_home_VDH3[,c("prcp_weighted")], by=list(samarth_home_VDH3$VDH_Health_Region,samarth_home_VDH3$date), FUN=sum)
samarth_home_VDH3$VDH_Health_Region <- samarth_home_VDH3$Group.1
samarth_home_VDH3$date <- samarth_home_VDH3$Group.2
samarth_home_VDH3$prcp <- samarth_home_VDH3$x
samarth_home_VDH3$Group.1 <- NULL
samarth_home_VDH3$Group.2 <- NULL
samarth_home_VDH3$x <- NULL
head(samarth_home_VDH3)
summary(samarth_home_VDH3$prcp)

### Aggregation to region level: sum - Activity ###
samarth_act_VDH3 <- aggregate(samarth_act_VDH3[,c("prcp_weighted")], by=list(samarth_act_VDH3$VDH_Health_Region,samarth_act_VDH3$date), FUN=sum)
samarth_act_VDH3$VDH_Health_Region <- samarth_act_VDH3$Group.1
samarth_act_VDH3$date <- samarth_act_VDH3$Group.2
samarth_act_VDH3$prcp <- samarth_act_VDH3$x
samarth_act_VDH3$Group.1 <- NULL
samarth_act_VDH3$Group.2 <- NULL
samarth_act_VDH3$x <- NULL
head(samarth_act_VDH3)
summary(samarth_act_VDH3$prcp)

#### Merge all and add DOW ####
### Home
samarth_home_VDH <- merge(samarth_home_VDH1,samarth_home_VDH2,by=c("VDH_Health_Region","date"))
samarth_home_VDH <- merge(samarth_home_VDH,samarth_home_VDH3,by=c("VDH_Health_Region","date"))
samarth_home_VDH <- merge(samarth_home_VDH,samarth_home3[!duplicated(samarth_home3$date),c("date","DOW")],by="date",all.x=TRUE)
head(samarth_home_VDH)

### Activity
samarth_act_VDH <- merge(samarth_act_VDH1,samarth_act_VDH2,by=c("VDH_Health_Region","date"))
samarth_act_VDH <- merge(samarth_act_VDH,samarth_act_VDH3,by=c("VDH_Health_Region","date"))
samarth_act_VDH <- merge(samarth_act_VDH,samarth_act3[!duplicated(samarth_act3$date),c("date","DOW")],by="date",all.x=TRUE)
head(samarth_act_VDH)


#### Prepare Data ####
### Separate zctas into separate elements of a list ###
regions <- as.character(unique(samarth_act_VDH$VDH_Health_Region))
### Home
data_home_region <- lapply(regions, function(x) samarth_home_VDH[samarth_home_VDH$VDH_Health_Region==x,])
names(data_home_region) <- regions
head(data_home_region[[1]]) #ordered by date
for(i in seq(data_home_region)) {
  data_home_region[[i]]$time <- c(1:nrow(data_home_region[[i]])) 
}
m <- length(regions)

### Activity
data_act_region <- lapply(regions, function(x) samarth_act_VDH[samarth_act_VDH$VDH_Health_Region==x,])
names(data_act_region) <- regions
head(data_act_region[[1]]) #ordered by date
for(i in seq(data_act_region)) {
  data_act_region[[i]]$time <- c(1:nrow(data_act_region[[i]])) 
}
m <- length(regions)


### Region-level Threshold Variables ###
### Precipitation quantiles
### Home
precip_quantiles_home_region <- list()
for(i in seq(data_home_region)) {
  precip_quantiles_home_region[[i]] <- quantile(data_home_region[[i]]$prcp, 0.95) 
}
precip_quantiles_home_region[[1]]

### Activity
precip_quantiles_act_region <- list()
for(i in seq(data_act_region)) {
  precip_quantiles_act_region[[i]] <- quantile(data_act_region[[i]]$prcp, 0.95) 
}
precip_quantiles_act_region[[1]]


### Indicator variables for 95 percentile over 28 days AND over 11.77mm in 24h or 39.90mm over 48 hours
### Home
for(i in seq_along(data_home_region)) {
  setDT(data_home_region[[i]])
  
  # 48-hour precip sum
  prcp_48hr <- frollsum(data_home_region[[i]]$prcp, n=2, fill=0)
  # Absolute rainfall thresholds test for each day
  absolute_threshold <- (data_home_region[[i]]$prcp > 11.77) | (prcp_48hr > 39.90)
  
  # Percentile rainfall thresholds test for each day
  percentile_threshold <- data_home_region[[i]]$prcp >= precip_quantiles_home_region[[i]]
  
  # Combine threshols:percentile threshold AND absolute threshold
  combined_threshold <- percentile_threshold & absolute_threshold
  
  # If any of the previous 28 days met the combined threshold, set to 1, else 0
  data_home_region[[i]][, ("prcp_95_28") := ifelse(frollsum(shift(combined_threshold, n=1, fill=0), n=28, fill=0)>0, 1, 0)]
  
}

### Activity
for(i in seq_along(data_act_region)) {
  setDT(data_act_region[[i]])
  
  # 48-hour precip sum
  prcp_48hr <- frollsum(data_act_region[[i]]$prcp, n=2, fill=0)
  # Absolute rainfall thresholds test for each day
  absolute_threshold <- (data_act_region[[i]]$prcp > 11.77) | (prcp_48hr > 39.90)
  
  # Percentile rainfall thresholds test for each day
  percentile_threshold <- data_act_region[[i]]$prcp >= precip_quantiles_act_region[[i]]
  
  # Combine threshols:percentile threshold AND absolute threshold
  combined_threshold <- percentile_threshold & absolute_threshold
  
  # If any of the previous 28 days met the combined threshold, set to 1, else 0
  data_act_region[[i]][, ("prcp_95_28") := ifelse(frollsum(shift(combined_threshold, n=1, fill=0), n=28, fill=0)>0, 1, 0)]
  
}



##### 3.3. Pregnancy-Related Visits Models #####
#### First-Stage Models ####
library(dlnm)
library(splines)
source("findmin.R")

### Prepare cross-basis elements ###
### Home
ranges_home_region <- t(sapply(data_home_region,function(x) range(x$tmp_C,na.rm=T))) # Temperature min and max at each zcta
bound_home_region <- colMeans(ranges_home_region)
lag <- 28
lagknots_home_region <- logknots(lag, nk=2)
arglag_home_region <- list(fun="ns",knots=lagknots_home_region)
varknots_home_region <- rowMeans(sapply(data_home_region,function(x) equalknots(x$tmp_C,fun="ns",nk=2)))
argvar_home_region <- list(fun="ns",knots=varknots_home_region,bound=bound_home_region)

### Activity
ranges_act_region <- t(sapply(data_act_region,function(x) range(x$tmp_C,na.rm=T))) # Temperature min and max at each zcta
bound_act_region <- colMeans(ranges_act_region)
lag <- 28
lagknots_act_region <- logknots(lag, nk=2)
arglag_act_region <- list(fun="ns",knots=lagknots_act_region)
varknots_act_region <- rowMeans(sapply(data_act_region,function(x) equalknots(x$tmp_C,fun="ns",nk=2)))
argvar_act_region <- list(fun="ns",knots=varknots_act_region,bound=bound_act_region)


### Objects to store the results
### Home
# lists
models_home_preg <- list()
qaic_home_preg <- 0
predictions_home_preg <- list()
yall_home_preg <- matrix(NA,length(regions),3,dimnames=list(regions,paste("b",seq(3),sep="")))
Sall_home_preg <- vector("list",length(regions))
names(Sall_home_preg) <- regions
cen_home_preg <- numeric(length(regions))
precip_coefs_home_preg <- matrix(NA, length(regions), 1, dimnames=list(regions, "est"))
precip_var_home_preg  <- vector("list",length(regions))
names(precip_var_home_preg) <- regions

### Activity
# lists
models_act_preg <- list()
qaic_act_preg <- 0
predictions_act_preg <- list()
yall_act_preg <- matrix(NA,length(regions),3,dimnames=list(regions,paste("b",seq(3),sep="")))
Sall_act_preg <- vector("list",length(regions))
names(Sall_act_preg) <- regions
cen_act_preg <- numeric(length(regions))
precip_coefs_act_preg <- matrix(NA, length(regions), 1, dimnames=list(regions, "est"))
precip_var_act_preg  <- vector("list",length(regions))
names(precip_var_act_preg) <- regions


### Run the models and store results
## Home
for(i in regions) {
  
  sub <- data_home_region[[i]]
  
  cb_home_preg <- crossbasis(sub$tmp_C,lag=lag,argvar=argvar_home_region,arglag=arglag_home_region)
  model <- glm(Pregnancy_Related_Visits_Patient_Zip ~ cb_home_preg + DOW + ns(time, df=4*9) + prcp_95_28 + offset(log(Total_Visits_Patient_Zip)),
               family="quasipoisson", data=sub)
  
  models_home_preg[[i]] <- model
  qaic_home_preg[i] <- fqaic(model)
  predictions_home_preg[[i]] <- crossreduce(cb_home_preg, model,cen=findmin(cb_home_preg,model))
  yall_home_preg[i,] <- coef(predictions_home_preg[[i]])
  Sall_home_preg[[i]] <- vcov(predictions_home_preg[[i]])
  cen_home_preg[i]<- findmin(cb_home_preg,model)
  
  precip_coefs_home_preg[i, 1] <- coef(model)["prcp_95_28"]
  precip_var_home_preg[[i]]  <- vcov(model)["prcp_95_28","prcp_95_28"]
  
}

## Activity
for(i in regions) {
  
  sub <- data_act_region[[i]]
  
  cb_act_preg <- crossbasis(sub$tmp_C,lag=lag,argvar=argvar_act_region,arglag=arglag_act_region)
  model <- glm(Pregnancy_Related_Visits_Patient_Zip ~ cb_act_preg + DOW + ns(time, df=4*9) + prcp_95_28 + offset(log(Total_Visits_Patient_Zip)),
               family="quasipoisson", data=sub)
  
  models_act_preg[[i]] <- model
  qaic_act_preg[i] <- fqaic(model)
  predictions_act_preg[[i]] <- crossreduce(cb, model,cen=findmin(cb_act_preg,model))
  yall_act_preg[i,] <- coef(predictions_act_preg[[i]])
  Sall_act_preg[[i]] <- vcov(predictions_act_preg[[i]])
  cen_act_preg[i]<- findmin(cb_act_preg,model)
  
  precip_coefs_act_preg[i, 1] <- coef(model)["prcp_95_28"]
  precip_var_act_preg[[i]]  <- vcov(model)["prcp_95_28","prcp_95_28"]
  
}


#### Second-Stage Models ####
library(mvmeta)
### Multivariate meta-analytical model - Temp effect ###
### Home
method <- "reml"
mvall_home_preg <- mvmeta(yall_home_preg~ 1, Sall_home_preg, method=method)
summary(mvall_home_preg)

### Activity
method <- "reml"
mvall_act_preg <- mvmeta(yall_act_preg~ 1, Sall_act_preg, method=method)
summary(mvall_act_preg)


### Basis and predictions ####
### Home
## onebasis to create basis variables for prediction (attributes from 1st stage cross basis)
xvar_home_preg <- seq(bound_home_region[1],bound_home_region[2],by=0.1)
bvar_home_preg <- do.call("onebasis",c(list(x=xvar_home_preg),attr(cb_home_preg,"argvar")))
xlag_home_preg <- 0:280/10
blag_home_preg <- do.call("onebasis",c(list(x=xlag_home_preg),attr(cb_home_preg,"arglag")))
## First stage predictions (one per zcta)
regall_home_preg <- lapply(seq(nrow(yall_home_preg)),
                           function(i) crosspred(bvar_home_preg,coef=yall_home_preg[i,],vcov=Sall_home_preg[[i]],model.link="log",cen=16.2))
summary(regall_home_preg)
## Second stage prediction (only one - the meta-analytical model)
cpall_home_preg <- crosspred(bvar_home_preg,coef=coef(mvall_home_preg),vcov=vcov(mvall_home_preg),
                             model.link="log",by=0.1,from=bound_home_region[1],to=bound_home_region[2],
                             cen=16.2)
summary(cpall_home_preg)

### Activity
## onebasis to create basis variables for prediction (attributes from 1st stage cross basis)
xvar_act_preg <- seq(bound_act_region[1],bound_act_region[2],by=0.1)
bvar_act_preg <- do.call("onebasis",c(list(x=xvar_act_preg),attr(cb_act_preg,"argvar")))
xlag_act_preg <- 0:280/10
blag_act_preg <- do.call("onebasis",c(list(x=xlag_act_preg),attr(cb_act_preg,"arglag")))
## First stage predictions (one per zcta)
regall_act_preg <- lapply(seq(nrow(yall_act_preg)),
                          function(i) crosspred(bvar_act_preg,coef=yall_act_preg[i,],vcov=Sall_act_preg[[i]],model.link="log",cen=16.2))
summary(regall_act_preg)
## Second stage prediction (only one per prcp indicator - the meta-analytical model)
cpall_act_preg <- crosspred(bvar_act_preg,coef=coef(mvall_act_preg),vcov=vcov(mvall_act_preg),
                            model.link="log",by=0.1,from=bound_act_region[1],to=bound_act_region[2],
                            cen=16.2)
summary(cpall_act_preg)


### MMT ###
### Home
cpall_home_preg$predvar[which.min(cpall_home_preg$allRRfit)] #16.2
which.min(cpall_home_preg$allRRfit)

### Activity
cpall_act_preg$predvar[which.min(cpall_act_preg$allRRfit)] #16.2
which.min(cpall_act_preg$allRRfit)


### Overall exposure-response curve ###
# ### Home
# plot(cpall_home_preg,type="n",ylab="RR",ylim=c(0.9,1.6),xlim=c(-15,33),xlab="Temperature (C)",cex.axis=1.2,cex.lab=1.2)
# for(i in seq(regall_home_preg)) lines(regall_home_preg[[i]],ptype="overall",col="grey50",lwd=0.5)
# abline(h=1,col="black")
# lines(cpall_home_preg,col="red3",ci="lines",lwd=2.5)
# legend ("topleft",c("Pooled dose-response","1st stage zcta-specific"),
#         lty=c(1,2),lwd=2,col=c("red3","grey50"))
# 
# ### Activity
# plot(cpall_act_preg,type="n",ylab="RR",ylim=c(0.9,1.6),xlim=c(-15,33),xlab="Temperature (C)",cex.axis=1.2,cex.lab=1.2)
# for(i in seq(regall_act_preg)) lines(regall_act_preg[[i]],ptype="overall",col="grey50",lwd=0.5)
# abline(h=1,col="black")
# lines(cpall_act_preg,col="red3",ci="lines",lwd=2.5)
# legend ("topleft",c("Pooled dose-response","1st stage zcta-specific"),
#         lty=c(1,2),lwd=2,col=c("red3","grey50"))

### Together
plot(cpall_home_preg,col="blue4",ci="lines",lwd=5,ylab="RR",ylim=c(0.9,1.6),xlim=c(-15,33),xlab="Temperature (C)",main="Pregnancy",cex.axis=1.2,cex.lab=1.2)
lines(cpall_act_preg,col=adjustcolor("lightblue3", alpha.f=1),ci="lines",lwd=2.5)
legend ("topleft",c("Home","Activity"),
        lty=c(1,1),lwd=c(5,2),col=c("blue4","lightblue3"))


### Data for Table 1 ###
## MMT
which.min(cpall_home_preg$allRRfit)
which.min(cpall_act_preg$allRRfit)

## RR at 10th and 99th percentile
# Home
round(cpall_home_preg$allRRfit[as.character(round(quantile(samarth_home3$tmp_C,0.1),1))],2)
round(cpall_home_preg$allRRlow[as.character(round(quantile(samarth_home3$tmp_C,0.1),1))],2)
round(cpall_home_preg$allRRhigh[as.character(round(quantile(samarth_home3$tmp_C,0.1),1))],2)
round(cpall_home_preg$allRRfit[as.character(round(quantile(samarth_home3$tmp_C,0.9),1))],2)
round(cpall_home_preg$allRRlow[as.character(round(quantile(samarth_home3$tmp_C,0.9),1))],2)
round(cpall_home_preg$allRRhigh[as.character(round(quantile(samarth_home3$tmp_C,0.9),1))],2)
# Activity
round(cpall_act_preg$allRRfit[as.character(round(quantile(samarth_act3$tmp_C,0.1),1))],2)
round(cpall_act_preg$allRRlow[as.character(round(quantile(samarth_act3$tmp_C,0.1),1))],2)
round(cpall_act_preg$allRRhigh[as.character(round(quantile(samarth_act3$tmp_C,0.1),1))],2)
round(cpall_act_preg$allRRfit[as.character(round(quantile(samarth_act3$tmp_C,0.9),1))],2)
round(cpall_act_preg$allRRlow[as.character(round(quantile(samarth_act3$tmp_C,0.9),1))],2)
round(cpall_act_preg$allRRhigh[as.character(round(quantile(samarth_act3$tmp_C,0.9),1))],2)

## I2
summary(mvall_home_preg)
summary(mvall_act_preg)


### Multivariate meta-analytical model - Prcp effect ###
### Home
method <- "reml"
mvprecip_home_preg <- mvmeta(precip_coefs_home_preg ~ 1, precip_var_home_preg, method=method)
summary(mvprecip_home_preg)

### Activity
method <- "reml"
mvprecip_act_preg <- mvmeta(precip_coefs_act_preg ~ 1, precip_var_act_preg, method=method)
summary(mvprecip_act_preg)


### Gather all RRs (exponentiated estimate), p values and CIs in a dataframe ###
precip_meta_results_homeact_preg <- data.frame(model=c("Home","Activity"),
                                       RR=c(exp(summary(mvprecip_home_preg)$coefficients[,1]),
                                            exp(summary(mvprecip_act_preg)$coefficients[,1])),
                                       CI_min=c(exp(summary(mvprecip_home_preg)$coefficients[,5]),
                                                exp(summary(mvprecip_act_preg)$coefficients[,5])),
                                       CI_max=c(exp(summary(mvprecip_home_preg)$coefficients[,6]),
                                                exp(summary(mvprecip_act_preg)$coefficients[,6])),
                                       p.value=c(summary(mvprecip_home_preg)$coefficients[,4],
                                                 summary(mvprecip_act_preg)$coefficients[,4]))

### Plot
library(ggplot2)
ggplot(precip_meta_results_homeact_preg, aes(x=RR, y=model)) +
  geom_vline(xintercept=1, linetype="dashed") +
  geom_errorbarh(aes(xmin=CI_min, xmax=CI_max)) +
  geom_point(size=2) +
  scale_x_continuous(breaks=seq(0.975,1.025,by=0.025),limits=c(0.975, 1.025)) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank()) +
  labs(title="Pooled effect of precipitation indicator variables",
       x = "Relative Risk (RR)", y = "")



##### 3.4. Diarrhea-Related Visits Models #####
#### First-Stage Models ####
library(dlnm)
library(splines)
source("findmin.R")

### Prepare cross-basis elements ###
### Home
ranges_home_region <- t(sapply(data_home_region,function(x) range(x$tmp_C,na.rm=T))) # Temperature min and max at each zcta
bound_home_region <- colMeans(ranges_home_region)
lag <- 28
lagknots_home_region <- logknots(lag, nk=2)
arglag_home_region <- list(fun="ns",knots=lagknots_home_region)
varknots_home_region <- rowMeans(sapply(data_home_region,function(x) equalknots(x$tmp_C,fun="ns",nk=2)))
argvar_home_region <- list(fun="ns",knots=varknots_home_region,bound=bound_home_region)

### Activity
ranges_act_region <- t(sapply(data_act_region,function(x) range(x$tmp_C,na.rm=T))) # Temperature min and max at each zcta
bound_act_region <- colMeans(ranges_act_region)
lag <- 28
lagknots_act_region <- logknots(lag, nk=2)
arglag_act_region <- list(fun="ns",knots=lagknots_act_region)
varknots_act_region <- rowMeans(sapply(data_act_region,function(x) equalknots(x$tmp_C,fun="ns",nk=2)))
argvar_act_region <- list(fun="ns",knots=varknots_act_region,bound=bound_act_region)


### Objects to store the results
### Home
# lists
models_home_diar <- list()
qaic_home_diar <- 0
predictions_home_diar <- list()
yall_home_diar <- matrix(NA,length(regions),3,dimnames=list(regions,paste("b",seq(3),sep="")))
Sall_home_diar <- vector("list",length(regions))
names(Sall_home_diar) <- regions
cen_home_diar <- numeric(length(regions))
precip_coefs_home_diar <- matrix(NA, length(regions), 1, dimnames=list(regions, "est"))
precip_var_home_diar  <- vector("list",length(regions))
names(precip_var_home_diar) <- regions

### Activity
# lists
models_act_diar <- list()
qaic_act_diar <- 0
predictions_act_diar <- list()
yall_act_diar <- matrix(NA,length(regions),3,dimnames=list(regions,paste("b",seq(3),sep="")))
Sall_act_diar <- vector("list",length(regions))
names(Sall_act_diar) <- regions
cen_act_diar <- numeric(length(regions))
precip_coefs_act_diar <- matrix(NA, length(regions), 1, dimnames=list(regions, "est"))
precip_var_act_diar  <- vector("list",length(regions))
names(precip_var_act_diar) <- regions


### Run the models and store results
## Home
for(i in regions) {
  
  sub <- data_home_region[[i]]
  
  cb_home_diar <- crossbasis(sub$tmp_C,lag=lag,argvar=argvar_home_region,arglag=arglag_home_region)
  model <- glm(Diarrheal_Visits_Patient_Zip ~ cb_home_diar + DOW + ns(time, df=4*9) + prcp_95_28 + offset(log(Total_Visits_Patient_Zip)),
               family="quasipoisson", data=sub)
  
  models_home_diar[[i]] <- model
  qaic_home_diar[i] <- fqaic(model)
  predictions_home_diar[[i]] <- crossreduce(cb_home_diar, model,cen=findmin(cb_home_diar,model))
  yall_home_diar[i,] <- coef(predictions_home_diar[[i]])
  Sall_home_diar[[i]] <- vcov(predictions_home_diar[[i]])
  cen_home_diar[i]<- findmin(cb_home_diar,model)
  
  precip_coefs_home_diar[i, 1] <- coef(model)["prcp_95_28"]
  precip_var_home_diar[[i]]  <- vcov(model)["prcp_95_28","prcp_95_28"]
  
}

## Activity
for(i in regions) {
  
  sub <- data_act_region[[i]]
  
  cb_act_diar <- crossbasis(sub$tmp_C,lag=lag,argvar=argvar_act_region,arglag=arglag_act_region)
  model <- glm(Diarrheal_Visits_Patient_Zip ~ cb_act_diar + DOW + ns(time, df=4*9) + prcp_95_28 + offset(log(Total_Visits_Patient_Zip)),
               family="quasipoisson", data=sub)
  
  models_act_diar[[i]] <- model
  qaic_act_diar[i] <- fqaic(model)
  predictions_act_diar[[i]] <- crossreduce(cb, model,cen=findmin(cb_act_diar,model))
  yall_act_diar[i,] <- coef(predictions_act_diar[[i]])
  Sall_act_diar[[i]] <- vcov(predictions_act_diar[[i]])
  cen_act_diar[i]<- findmin(cb_act_diar,model)
  
  precip_coefs_act_diar[i, 1] <- coef(model)["prcp_95_28"]
  precip_var_act_diar[[i]]  <- vcov(model)["prcp_95_28","prcp_95_28"]
  
}


#### Second-Stage Models ####
library(mvmeta)
### Multivariate meta-analytical model - Temp effect ###
### Home
method <- "reml"
mvall_home_diar <- mvmeta(yall_home_diar~ 1, Sall_home_diar, method=method)
summary(mvall_home_diar)

### Activity
method <- "reml"
mvall_act_diar <- mvmeta(yall_act_diar~ 1, Sall_act_diar, method=method)
summary(mvall_act_diar)


### Basis and predictions ####
### Home
## onebasis to create basis variables for prediction (attributes from 1st stage cross basis)
xvar_home_diar <- seq(bound_home_region[1],bound_home_region[2],by=0.1)
bvar_home_diar <- do.call("onebasis",c(list(x=xvar_home_diar),attr(cb_home_diar,"argvar")))
xlag_home_diar <- 0:280/10
blag_home_diar <- do.call("onebasis",c(list(x=xlag_home_diar),attr(cb_home_diar,"arglag")))
## First stage predictions (one per zcta)
regall_home_diar <- lapply(seq(nrow(yall_home_diar)),
                           function(i) crosspred(bvar_home_diar,coef=yall_home_diar[i,],vcov=Sall_home_diar[[i]],model.link="log",cen=-12.5))
summary(regall_home_diar)
## Second stage prediction (only one - the meta-analytical model)
cpall_home_diar <- crosspred(bvar_home_diar,coef=coef(mvall_home_diar),vcov=vcov(mvall_home_diar),
                             model.link="log",by=0.1,from=bound_home_region[1],to=bound_home_region[2],
                             cen=-12.5)
summary(cpall_home_diar)

### Activity
## onebasis to create basis variables for prediction (attributes from 1st stage cross basis)
xvar_act_diar <- seq(bound_act_region[1],bound_act_region[2],by=0.1)
bvar_act_diar <- do.call("onebasis",c(list(x=xvar_act_diar),attr(cb_act_diar,"argvar")))
xlag_act_diar <- 0:280/10
blag_act_diar <- do.call("onebasis",c(list(x=xlag_act_diar),attr(cb_act_diar,"arglag")))
## First stage predictions (one per zcta)
regall_act_diar <- lapply(seq(nrow(yall_act_diar)),
                          function(i) crosspred(bvar_act_diar,coef=yall_act_diar[i,],vcov=Sall_act_diar[[i]],model.link="log",cen=-12.5))
summary(regall_act_diar)
## Second stage prediction (only one per prcp indicator - the meta-analytical model)
cpall_act_diar <- crosspred(bvar_act_diar,coef=coef(mvall_act_diar),vcov=vcov(mvall_act_diar),
                            model.link="log",by=0.1,from=bound_act_region[1],to=bound_act_region[2],
                            cen=-12.5)
summary(cpall_act_diar)


### MMT ###
### Home
cpall_home_diar$predvar[which.min(cpall_home_diar$allRRfit)] #-12.5
which.min(cpall_home_diar$allRRfit)

### Activity
cpall_act_diar$predvar[which.min(cpall_act_diar$allRRfit)] #-12.5
which.min(cpall_act_diar$allRRfit)


### Overall exposure-response curve ###
# ### Home
# plot(cpall_home_diar,type="n",ylab="RR",ylim=c(0.9,1.6),xlim=c(-15,33),xlab="Temperature (C)",cex.axis=1.2,cex.lab=1.2)
# for(i in seq(regall_home_diar)) lines(regall_home_diar[[i]],ptype="overall",col="grey50",lwd=0.5)
# abline(h=1,col="black")
# lines(cpall_home_diar,col="red3",ci="lines",lwd=2.5)
# legend ("topleft",c("Pooled dose-response","1st stage zcta-specific"),
#         lty=c(1,2),lwd=2,col=c("red3","grey50"))
# 
# ### Activity
# plot(cpall_act_diar,type="n",ylab="RR",ylim=c(0.9,1.6),xlim=c(-15,33),xlab="Temperature (C)",cex.axis=1.2,cex.lab=1.2)
# for(i in seq(regall_act_diar)) lines(regall_act_diar[[i]],ptype="overall",col="grey50",lwd=0.5)
# abline(h=1,col="black")
# lines(cpall_act_diar,col="red3",ci="lines",lwd=2.5)
# legend ("topleft",c("Pooled dose-response","1st stage zcta-specific"),
#         lty=c(1,2),lwd=2,col=c("red3","grey50"))

### Together
plot(cpall_home_diar,col="blue4",ci="lines",lwd=5,ylab="RR",ylim=c(0.9,2.2),xlim=c(-15,33),xlab="Temperature (C)",main="Pregnancy",cex.axis=1.2,cex.lab=1.2)
lines(cpall_act_diar,col=adjustcolor("lightblue3", alpha.f=1),ci="lines",lwd=2.5)
legend ("topleft",c("Home","Activity"),
        lty=c(1,1),lwd=c(5,2),col=c("blue4","lightblue3"))


### Data for Table 1 ###
## MMT
which.min(cpall_home_diar$allRRfit)
which.min(cpall_act_diar$allRRfit)

## RR at 10th and 99th percentile
# Home
round(cpall_home_diar$allRRfit[as.character(round(quantile(samarth_home3$tmp_C,0.1),1))],2)
round(cpall_home_diar$allRRlow[as.character(round(quantile(samarth_home3$tmp_C,0.1),1))],2)
round(cpall_home_diar$allRRhigh[as.character(round(quantile(samarth_home3$tmp_C,0.1),1))],2)
round(cpall_home_diar$allRRfit[as.character(round(quantile(samarth_home3$tmp_C,0.9),1))],2)
round(cpall_home_diar$allRRlow[as.character(round(quantile(samarth_home3$tmp_C,0.9),1))],2)
round(cpall_home_diar$allRRhigh[as.character(round(quantile(samarth_home3$tmp_C,0.9),1))],2)
# Activity
round(cpall_act_diar$allRRfit[as.character(round(quantile(samarth_act3$tmp_C,0.1),1))],2)
round(cpall_act_diar$allRRlow[as.character(round(quantile(samarth_act3$tmp_C,0.1),1))],2)
round(cpall_act_diar$allRRhigh[as.character(round(quantile(samarth_act3$tmp_C,0.1),1))],2)
round(cpall_act_diar$allRRfit[as.character(round(quantile(samarth_act3$tmp_C,0.9),1))],2)
round(cpall_act_diar$allRRlow[as.character(round(quantile(samarth_act3$tmp_C,0.9),1))],2)
round(cpall_act_diar$allRRhigh[as.character(round(quantile(samarth_act3$tmp_C,0.9),1))],2)

## I2
summary(mvall_home_diar)
summary(mvall_act_diar)


### Multivariate meta-analytical model - Prcp effect ###
### Home
method <- "reml"
mvprecip_home_diar <- mvmeta(precip_coefs_home_diar ~ 1, precip_var_home_diar, method=method)
summary(mvprecip_home_diar)

### Activity
method <- "reml"
mvprecip_act_diar <- mvmeta(precip_coefs_act_diar ~ 1, precip_var_act_diar, method=method)
summary(mvprecip_act_diar)


### Gather all RRs (exponentiated estimate), p values and CIs in a dataframe ###
precip_meta_results_homeact_diar <- data.frame(model=c("Home","Activity"),
                                               RR=c(exp(summary(mvprecip_home_diar)$coefficients[,1]),
                                                    exp(summary(mvprecip_act_diar)$coefficients[,1])),
                                               CI_min=c(exp(summary(mvprecip_home_diar)$coefficients[,5]),
                                                        exp(summary(mvprecip_act_diar)$coefficients[,5])),
                                               CI_max=c(exp(summary(mvprecip_home_diar)$coefficients[,6]),
                                                        exp(summary(mvprecip_act_diar)$coefficients[,6])),
                                               p.value=c(summary(mvprecip_home_diar)$coefficients[,4],
                                                         summary(mvprecip_act_diar)$coefficients[,4]))

### Plot
library(ggplot2)
ggplot(precip_meta_results_homeact_diar, aes(x=RR, y=model)) +
  geom_vline(xintercept=1, linetype="dashed") +
  geom_errorbarh(aes(xmin=CI_min, xmax=CI_max)) +
  geom_point(size=2) +
  scale_x_continuous(breaks=seq(0.975,1.025,by=0.025),limits=c(0.975, 1.025)) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank()) +
  labs(title="Pooled effect of precipitation indicator variables",
       x = "Relative Risk (RR)", y = "")




####### 4. Models Figures #######

##### 4.1. Temp dose responses #####
par(mfrow=c(3,2))

### Dayment-All visits
plot(cpall,type="n",ylim=c(0.9,1.9),xlim=c(-12.01,29.36),cex.axis=1.2,cex.lab=1.2,
     main="Daymet - All visits",ylab=" ",xlab="")
for(i in seq(regall)) lines(regall[[i]],ptype="overall",col="grey50",lwd=0.5)
abline(h=1,col="black")
lines(cpall,col="red3",ci="lines",lwd=2.5)
legend ("topleft",c("Pooled dose-response","1st stage ZCTA-specific"),
        lty=c(1,1),lwd=2,col=c("red3","grey50"))
text(x=27, y=1.8, labels="a", font=2, cex=5)

### HRRR-All visits
plot(cpall_home,col="blue4",ci="lines",lwd=5,ylim=c(0.9,1.9),xlim=c(-12.01,29.36),cex.axis=1.2,cex.lab=1.2,
     main="HRRR - All visits",ylab=" ",xlab="")
lines(cpall_act,col=adjustcolor("lightblue3", alpha.f=1),ci="lines",lwd=2.5)
legend ("topleft",c("Home","Activity"),
        lty=c(1,1),lwd=c(5,2),col=c("blue4","lightblue3"))
text(x=27, y=1.8, labels="b", font=2, cex=5)

### Daymet-Pregnancy
plot(cpall_preg,type="n",ylim=c(0.9,1.9),xlim=c(-12.01,29.36),cex.axis=1.2,cex.lab=1.2,
     main="Daymet - Pregnancy-related visits",ylab="RR",xlab="")
for(i in seq(regall_preg)) lines(regall_preg[[i]],ptype="overall",col="grey50",lwd=0.5)
abline(h=1,col="black")
lines(cpall_preg,col="red3",ci="lines",lwd=2.5)
legend ("topleft",c("Pooled dose-response","1st stage Region-specific"),
        lty=c(1,1),lwd=2,col=c("red3","grey50"))
text(x=27, y=1.8, labels="c", font=2, cex=5)

### HRRR-Pregnancy
plot(cpall_home_preg,col="blue4",ci="lines",lwd=5,ylab="RR",ylim=c(0.9,1.9),xlim=c(-12.01,29.36),cex.axis=1.2,cex.lab=1.2,
     main="HRRR - Pregnancy-related visits",ylab="RR",xlab="")
lines(cpall_act_preg,col=adjustcolor("lightblue3", alpha.f=1),ci="lines",lwd=2.5)
text(x=27, y=1.8, labels="d", font=2, cex=5)

### Daymet-Diarrhea
plot(cpall_diar,type="n",ylim=c(0.9,1.9),xlim=c(-12.01,29.36),cex.axis=1.2,cex.lab=1.2,
     main="Daymet - Diarrhea-Related visits",ylab=" ",xlab="Temperature (C)")
for(i in seq(regall_diar)) lines(regall_diar[[i]],ptype="overall",col="grey50",lwd=0.5)
abline(h=1,col="black")
lines(cpall_diar,col="red3",ci="lines",lwd=2.5)
legend ("topleft",c("Pooled dose-response","1st stage Region-specific"),
        lty=c(1,1),lwd=2,col=c("red3","grey50"))
text(x=27, y=1.12, labels="e", font=2, cex=5)

### HRRR-diarrhea
plot(cpall_home_diar,col="blue4",ci="lines",lwd=5,ylim=c(0.9,1.9),xlim=c(-12.01,29.36),cex.axis=1.2,cex.lab=1.2,
     main="HRRR - Diarrhea-related visits",ylab=" ",xlab="Temperature (C)")
lines(cpall_act_diar,col=adjustcolor("lightblue3", alpha.f=1),ci="lines",lwd=2.5)
text(x=27, y=1.12, labels="f", font=2, cex=5)


##### 4.2. Prcp forest plot #####
### Make one dataframe only
precip_meta_results_all <- rbind(precip_meta_results,precip_meta_results_homeact,
                                 precip_meta_results_preg,precip_meta_results_homeact_preg,
                                 precip_meta_results_diar,precip_meta_results_homeact_diar)
precip_meta_results_all$model <- c("Daymet-All Visits","HRRR-All Visits-Home","HRRR-All Visits-Activity",
                                   "Daymet-Pregnancy","HRRR-Pregnancy-Home","HRRR-Pregnancy-Activity",
                                   "Daymet-Diarrhea","HRRR-Diarrhea-Home","HRRR-Diarrhea-Activity")

### Plot
library(ggplot2)
precip_meta_results_all$model <- factor(precip_meta_results_all$model,levels=rev(c("Daymet-All Visits","HRRR-All Visits-Home","HRRR-All Visits-Activity",
                                                                               "Daymet-Pregnancy","HRRR-Pregnancy-Home","HRRR-Pregnancy-Activity",
                                                                               "Daymet-Diarrhea","HRRR-Diarrhea-Home","HRRR-Diarrhea-Activity")))
ggplot(precip_meta_results_all, aes(x=RR, y=model)) +
  geom_vline(xintercept=1, linetype="dashed") +
  geom_errorbarh(aes(xmin=CI_min, xmax=CI_max)) +
  geom_point(size=2) +
  scale_x_continuous(breaks=seq(0.975,1.025,by=0.005),limits=c(0.975, 1.025)) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank()) +
  labs(title="Pooled effect of precipitation indicator variables",
       x = "Relative Risk (RR)", y = "")

###WITHOUT ACITIVITY:
ggplot(precip_meta_results_all[!precip_meta_results_all$model%in%c("HRRR-All Visits-Activity","HRRR-Pregnancy-Activity","HRRR-Diarrhea-Activity"),],
       aes(x=RR, y=model)) +
  geom_vline(xintercept=1, linetype="dashed") +
  geom_errorbarh(aes(xmin=CI_min, xmax=CI_max)) +
  geom_point(size=2) +
  scale_x_continuous(breaks=seq(0.975,1.025,by=0.005),limits=c(0.975, 1.025)) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank()) +
  labs(title="Pooled effect of precipitation indicator variables",
       x = "Relative Risk (RR)", y = "")





