
library(MASS)
library(car)
library(rgl)
options(rgl.printRglwidget = TRUE)
library(ellipse)
library(faraway)
library(leaps)
library(GGally)
library(BAS)
options(rgl.debug = TRUE)
library(rgl)
library(corrplot)
library(MVN)
library(Matrix)
library(RColorBrewer)
library(mvtnorm)
library(mvnormtest)
library(caTools) 
library(tidyverse) 
library(ggplot2)
library(heplots)
library(class)
library(e1071)
library(ISLR)
library(SphericalCubature)
library(dbscan)
library(cluster)
library(fields)
library(DMwR2)
library(rms)
library(arm)
library(ResourceSelection)
library(pROC)
library(PRROC)
library(glmnet)
library(caret)
library(ExhaustiveSearch)
library(geosphere)
library(CausalImpact)
library(bsts)
library(zoo)

##### LOADING THE NEW DATASETS
#NOx_values  <- read.csv("NOx_values.csv")
NOx_sensors <- read.csv("./Data/NOx_sensors.csv")
#daily_meteo <- read.csv("Daily_meteo.csv")
#hourly_meteo<- read.csv("Hourly_meteo.csv")

All_values <- read.csv("./Data/All_values.csv")
All_values <- All_values[,-1]


##### !!!! ######
# data merging,filtering, and initalization,  do NOT run, rely on All_values.csv,
# All_values_clean.csv  and NOx_sensors.csv


###########################################
######### NOx Sensors filtering ###########
###########################################
# ### Load the datasets
# NOx_values1 <- read.csv("Dati_sensori_aria_2010_2017.csv")
# NOx_values2 <- read.csv("Dati_sensori_aria_2018_2022.csv")
# NOx_values3 <- read.csv("Dati_sensori_aria_2023.csv")
# 
# Air_sensors <- read.csv("Stazioni_sensori_aria.csv")
# pm10 <- read.csv("PM10_LOMBARDIA.csv")
# # deleting useless columns:
# NOx_values1 <- NOx_values1[,1:3]
# NOx_values2 <- NOx_values2[,1:3]
# NOx_values3 <- NOx_values3[,1:3]
# 
# # uniforming column names
# colnames(NOx_values1) <-c("IdSensore", "Data","Valore")
# colnames(NOx_values2) <-c("IdSensore", "Data","Valore")
# colnames(NOx_values3) <-c("IdSensore", "Data","Valore")
# 
# #uniforming dates
# NOx_values1$Data <- as.Date(NOx_values1$Data, "%m/%d/%Y")
# NOx_values2$Data <- as.Date(NOx_values2$Data, "%m/%d/%Y")
# NOx_values3$Data <- as.Date(NOx_values3$Data, "%d/%m/%Y")
# 
# 
# # merging
# NOx_values <- rbind(NOx_values1, NOx_values2, NOx_values3)
# 
# 
# ###############################################
# ####### Extract the only the NOx sensors
# ####### operating on time window (01/01/2016 - 01/11/2023)
# ###############################################
# 
# 
# ### NOx Sensors
# id_NOx <- Air_sensors$IdSensore[which(Air_sensors$NomeTipoSensore == "Ossidi di Azoto")] # -> 166 sensors
# 
# aaa <-Air_sensors[which(Air_sensors$IdSensore %in% id_NOx),]
# ### Time check (sensor was available in the whole time period 2016-2023)
# # conversion of "date"
# startDates <- Air_sensors$DataStart
# startDates[which(startDates=="")] <-"01/01/2000"
# startDates <- as.Date(startDates, "%d/%m/%Y")
# startDates
# Air_sensors$DataStart <-startDates
# 
# stopDates  <- Air_sensors$DataStop
# stopDates[which(stopDates=="")] <-"01/01/2099"
# stopDates  <- as.Date(stopDates, "%d/%m/%Y")
# stopDates
# Air_sensors$DataStop <-stopDates
# # id
# id_time <- Air_sensors$IdSensore[which(startDates < "2016-01-01" & stopDates > "2023-11-01")]
# 
# 
# ### Valid id (NOx & time)
# valid_id <- id_NOx[which(id_NOx %in% id_time)] #->88 valid sensors
# 
# # Check on how many valid sensor have their time series registered
# valid_NOx_id <- valid_id[(which(valid_id %in% NOx_values$IdSensore))]
# # -> ALL 88 have data recordered
# 
# 
# ### extract the NOx datasets 2016-2023
# valid_rows <- which(NOx_values$IdSensore %in%valid_NOx_id)
# NOx_values <-NOx_values[valid_rows,]
# 
# # check on NA
# length(unique(NOx_values$IdSensore))  # 88 sensors
# length(unique(NOx_values$Data))  # 121600 time stamps
# # !!! NB:  - there are some invalid numbers (-9999)
# #          - there are some missing times stamps (121600*88 > 10053764)
# 
# 
# ### extract the NOx sensors data
# NOx_sensors <- Air_sensors[which(Air_sensors$IdSensore %in% valid_NOx_id),]
# NOx_values <- NOx_values[which(NOx_values$Data >"2015-12-31"),]
# 
# unique(NOx_values$IdSensore) #88 sensors
# unique(NOx_values$Data)      #2872 days
# 
# ### Combining with PM10_LOMBARDIA
# NOx_sensors <- cbind(NOx_sensors,NA,NA)
# colnames(NOx_sensors)[c(18,19)]<-c("Type","Area")
# 
# 
# manual_check <- NULL
# pm10_station <- unique(pm10$NomeStazione)
# 
# for (i in 1:length(pm10_station)) {
#   for (j in 1:dim(pm10)[1]) {
#     print(j)
#     if(pm10_station[i] == pm10$NomeStazione[j]){
#       manual_check <- rbind(manual_check,pm10[j,c(3,10,11,13)])
#       break
#     }
#   }
# }
# 
# bbb<- NOx_sensors[,c(6,7,18,19)]
# 
# 
# NOx_sensors[71 ,c(18,19)] <- c("","")
# NOx_sensors[87 ,c(18,19)] <- c("","")
# NOx_sensors[67 ,c(18,19)] <- c("T","U")
# NOx_sensors[58 ,c(18,19)] <- c("B","U")
# NOx_sensors[85 ,c(18,19)] <- c("B","R")
# NOx_sensors[75 ,c(18,19)] <- c("B","S")
# NOx_sensors[81 ,c(18,19)] <- c("B","U")
# NOx_sensors[16 ,c(18,19)] <- c("","")
# NOx_sensors[48 ,c(18,19)] <- c("T","U")
# NOx_sensors[57 ,c(18,19)] <- c("","")
# NOx_sensors[68 ,c(18,19)] <- c("","")
# 
# NOx_sensors[17 ,c(18,19)] <- c("B","U")
# NOx_sensors[62 ,c(18,19)] <- c("B","S")
# NOx_sensors[18 ,c(18,19)] <- c("B","S")
# NOx_sensors[78 ,c(18,19)] <- c("B","S")
# NOx_sensors[10 ,c(18,19)] <- c("B","R")
# NOx_sensors[6 ,c(18,19)] <- c("T","U")
# NOx_sensors[43 ,c(18,19)] <- c("","")
# NOx_sensors[9 ,c(18,19)] <- c("T","U")
# 
# NOx_sensors[61 ,c(18,19)] <- c("","")
# NOx_sensors[45 ,c(18,19)] <- c("T","U")
# NOx_sensors[39 ,c(18,19)] <- c("","")
# NOx_sensors[29 ,c(18,19)] <- c("","")
# NOx_sensors[84 ,c(18,19)] <- c("","")
# NOx_sensors[76 ,c(18,19)] <- c("B","S")
# NOx_sensors[14 ,c(18,19)] <- c("T","U")
# NOx_sensors[59 ,c(18,19)] <- c("B","U")
# NOx_sensors[42 ,c(18,19)] <- c("T","U")
# NOx_sensors[70 ,c(18,19)] <- c("B","S")
# 
# NOx_sensors[74 ,c(18,19)] <- c("B","U")
# NOx_sensors[69 ,c(18,19)] <- c("B","U")
# NOx_sensors[41 ,c(18,19)] <- c("","")
# NOx_sensors[12 ,c(18,19)] <- c("B","U")
# NOx_sensors[20 ,c(18,19)] <- c("","")
# NOx_sensors[33 ,c(18,19)] <- c("T","U")
# NOx_sensors[24 ,c(18,19)] <- c("B","U")
# NOx_sensors[19 ,c(18,19)] <- c("B","U")
# 
# NOx_sensors[46 ,c(18,19)] <- c("T","U")
# NOx_sensors[26 ,c(18,19)] <- c("","")
# NOx_sensors[27 ,c(18,19)] <- c("B","U")
# NOx_sensors[11 ,c(18,19)] <- c("T","U")
# NOx_sensors[86 ,c(18,19)] <- c("B","U")
# NOx_sensors[64 ,c(18,19)] <- c("I","U")
# NOx_sensors[15 ,c(18,19)] <- c("T","U")
# NOx_sensors[34 ,c(18,19)] <- c("","")
# NOx_sensors[7 ,c(18,19)] <- c("","")
# NOx_sensors[13 ,c(18,19)] <- c("T","U")
# 
# NOx_sensors[31 ,c(18,19)] <- c("B","U")
# NOx_sensors[35 ,c(18,19)] <- c("","")
# NOx_sensors[66 ,c(18,19)] <- c("","")
# NOx_sensors[82 ,c(18,19)] <- c("T","U")
# NOx_sensors[32 ,c(18,19)] <- c("T","U")
# NOx_sensors[60 ,c(18,19)] <- c("B","R")
# NOx_sensors[83 ,c(18,19)] <- c("B","S")
# NOx_sensors[65 ,c(18,19)] <- c("B","U")
# NOx_sensors[49 ,c(18,19)] <- c("B","U")
# NOx_sensors[80 ,c(18,19)] <- c("","")
# 
# NOx_sensors[73 ,c(18,19)] <- c("B","S")
# NOx_sensors[44 ,c(18,19)] <- c("B","S")
# NOx_sensors[40 ,c(18,19)] <- c("I","U")
# NOx_sensors[55 ,c(18,19)] <- c("T","U")
# NOx_sensors[30 ,c(18,19)] <- c("B","U")
# NOx_sensors[53 ,c(18,19)] <- c("","")
# NOx_sensors[54 ,c(18,19)] <- c("","")
# NOx_sensors[8 ,c(18,19)] <- c("B","S")
# NOx_sensors[51 ,c(18,19)] <- c("I","S")
# NOx_sensors[28 ,c(18,19)] <- c("","")
# 
# NOx_sensors[38 ,c(18,19)] <- c("","")
# NOx_sensors[4 ,c(18,19)] <- c("B","S")
# NOx_sensors[21 ,c(18,19)] <- c("I","U")
# NOx_sensors[52 ,c(18,19)] <- c("B","U")
# NOx_sensors[3 ,c(18,19)] <- c("B","U")
# NOx_sensors[37 ,c(18,19)] <- c("B","R")
# NOx_sensors[5 ,c(18,19)] <- c("","")
# NOx_sensors[88 ,c(18,19)] <- c("T","U")
# NOx_sensors[1 ,c(18,19)] <- c("B","U")
# NOx_sensors[22 ,c(18,19)] <- c("T","S")
# 
# NOx_sensors[36 ,c(18,19)] <- c("I","R")
# NOx_sensors[23 ,c(18,19)] <- c("B","S")
# NOx_sensors[50 ,c(18,19)] <- c("","")
# NOx_sensors[77 ,c(18,19)] <- c("T","U")
# NOx_sensors[79 ,c(18,19)] <- c("B","U")
# NOx_sensors[2 ,c(18,19)] <- c("B","S")
# NOx_sensors[25 ,c(18,19)] <- c("T","U")
# NOx_sensors[63 ,c(18,19)] <- c("","")
# NOx_sensors[72 ,c(18,19)] <- c("","")
# NOx_sensors[47 ,c(18,19)] <- c("B","U")
# NOx_sensors[56 ,c(18,19)] <- c("B","U")
# 
# 
# 
# ### save results
# write.csv(NOx_values, file = "NOx_values.csv")
# write.csv(NOx_sensors, file = "NOx_sensors.csv")
# write.csv(manual_check, file ="manual_check.csv")
# 
# 
# 
# 
# ### Find the stations with missing/invalid values
# 
# # missing data
# NOx_values$Data <- as.Date(NOx_values$Data, "%Y-%m-%d")
# 
# 
# k <- length(unique(NOx_values$Data)) # 2872 different day
# tot_id<-unique(NOx_values$IdSensore) 
# missing <- NULL
# for (i in tot_id) {
#   num <- length(unique(NOx_values$Data[which(NOx_values$IdSensore == i)]))
#   missing  <- rbind(missing ,c(i,k - num))
# 
# }
# missing  # ->85 over 88 miss at least one day
# 
# 
# # invalid data
# tot_id<-unique(NOx_values$IdSensore) 
# invalid <- NULL
# for (i in tot_id) {
#   num <- length(which((NOx_values$Valore[which(NOx_values$IdSensore ==i)] < 0)))
#   invalid  <- rbind(invalid ,c(i,num))
# }
# invalid  # ->All have at least 1 invalid









################################################################################
####################   CREATING METEO DATASETS   ###############################
################################################################################



# ##### DAILY VALUES
# m2016 <- read.csv("2016TEST.csv")
# m2017 <- read.csv("2017TEST.csv")
# m2018 <- read.csv("2018TEST.csv")
# m2019 <- read.csv("2019TEST.csv")
# m2020 <- read.csv("2020TEST.csv")
# m2021 <- read.csv("2021TEST.csv")
# m2022 <- read.csv("2022TEST.csv")
# m2023 <- read.csv("2023TEST.csv")
# 
# daily_meteo <- rbind(m2016,m2017,m2018,m2019,m2020,m2021,m2022,m2023)
# daily_meteo$location_id <- NOx_sensors$IdSensore[daily_meteo$location_id+1]
#
#length(unique(daily_meteo$location_id)) 
#
#colnames(daily_meteo)[2] <- c("Data")
#
#write.csv(daily_meteo, file = "Daily_meteo.csv")



# #### HOURLY VALUES
# h2016 <- read.csv("test2016.csv")
# h2017 <- read.csv("test2017.csv")
# h2018 <- read.csv("test2018.csv")
# h2019 <- read.csv("test2019.csv")
# h2020 <- read.csv("test2020.csv")
# h2021 <- read.csv("test2021.csv")
# h2022 <- read.csv("test2022.csv")
# h2023 <- read.csv("test2023.csv")
# 
# hourly_meteo <- rbind(h2016,h2017,h2018,h2019,h2020,h2021,h2022,h2023)
# hourly_meteo$location_id <- NOx_sensors$IdSensore[hourly_meteo$location_id+1]
# length(unique(hourly_meteo$location_id))
# write.csv(hourly_meteo, file = "Hourly_meteo.csv")
# hourly_meteo<- read.csv("Hourly_meteo.csv")
# 
# hourly_meteo[,1] <- hourly_meteo[,2]
# hourly_meteo[,2] <- substr(hourly_meteo[,3],1,10)
# hourly_meteo[,3] <- substr(hourly_meteo[,3],12,16)
# 
# colnames(hourly_meteo)[1:3] <- c("location_id","Data","hour")
# 
# write.csv(hourly_meteo, file = "Hourly_meteo.csv")










# ###########################################
# ######### COMPUTING NOX MONTHLY AVERAGE ###
# ###########################################
# NOx_values$Data <- as.Date(NOx_values$Data)
# 
# #### Sensors subgroup:  only sensors s.t.  type="T"
# sens_id <- NOx_sensors$IdSensore  #[which(NOx_sensors$Type == "B")]
# 
# 
# 
# #### Creating the result matrix
# df1 <- unique(NOx_values$Data)
# df1<-as.data.frame(df1)
# 
# df1$Month <- months(df1[,1])
# df1$Year <- format(df1[,1],format="%y")
# df1$X3<-0
# 
# month_order <-c(5,4,9,2,8,6,7,1,12,11,10,3) 
# month_index <- NULL
# for (i in 0:6) {
#   month_index <-c(month_index,month_order+12*i)
# }
# month_index <-c(month_index,c(88,87,92,86,91,89,90,85,95,94,93))
# 
# a <- NULL
# a <- aggregate( X3 ~ Month + Year , df1 , mean )
# a <- a[month_index,]
# a$NOx <- NA
# a[,1] <- paste(a[,1],a[,2])
# a <- a[,-c(2,3)]
# 
# 
# 
# 
# #### Computing NOx average
# NOx_values$Month  <- months(NOx_values$Data)
# NOx_values$Year <- format(NOx_values$Data,format="%y")
# 
# All_values <- NULL
# for (i in sens_id) {
#   temp_index   <- NULL
#   temp_dataset <- NULL
#   temp_result  <- NULL 
# 
#   temp_index   <- which((NOx_values$IdSensore==i) &  # i-sensor
#                          NOx_values$Valore > -1 )     # condition on the values
#   temp_dataset <- NOx_values[temp_index,]
#   temp_result  <- aggregate( Valore ~ Month + Year , temp_dataset , FUN = mean )
#   
#   temp_result[,1] <- paste(temp_result[,1],temp_result[,2])
#   
#   result <- a
#   for (j in 1:95) {
#     if(result$Month[j] %in% temp_result[,1]){
#       result$NOx[j] <- temp_result[which(temp_result[,1] == result$Month[j]),3]
#     }
#   }
#   result<- cbind(i,result)
#   
#   
#   All_values<-rbind(All_values,result)
# }
# colnames(All_values)[1] <- "Id_sensor"
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ################################################################################
# ####################   EXTRACTING COVARIATES   #################################
# ################################################################################
# hourly_meteo$Data <- as.Date(hourly_meteo$Data)
# daily_meteo$Data <- as.Date(daily_meteo$Data)
# 
# hourly_meteo$Month  <- months(hourly_meteo$Data)
# hourly_meteo$Year <- format(hourly_meteo$Data,format="%y")
# 
# daily_meteo$Month  <- months(daily_meteo$Data)
# daily_meteo$Year <- format(daily_meteo$Data,format="%y")
# 
# 
# ###### MAX WIND10 SPEED
# covariate <- NULL
# for (i in sens_id) {
#   temp_index   <- NULL
#   temp_dataset <- NULL
#   temp_result  <- NULL
# 
#   temp_index   <- which(hourly_meteo$location_id == i)     # condition on the values
#   temp_dataset <- hourly_meteo[temp_index,]
#   temp_result  <- aggregate( wind_speed_10m..km.h. ~ Month + Year , temp_dataset , FUN = max )
# 
#   temp_result[,1] <- paste(temp_result[,1],temp_result[,2])
# 
#   result <- a
#   for (j in 1:95) {
#     if(result$Month[j] %in% temp_result[,1]){
#       result$NOx[j] <- temp_result[which(temp_result[,1] == result$Month[j]),3]
#     }
#   }
# 
#   result<- cbind(i,result)
#   covariate<-rbind(covariate,result)
# }
# 
# # consistency check ->OK
# max(hourly_meteo$wind_speed_10m..km.h.[which(
#   (hourly_meteo$location_id == 9918) &  # 10001  6204 9918
#   (hourly_meteo$Month == "gennaio")& 
#   (hourly_meteo$Year == 16) )])
# 
# # Add to the dataset
# All_values$max_wind10 <- covariate[,3]
# 
# 
# 
# ###### AVG WIND10 SPEED
# covariate <- NULL
# for (i in sens_id) {
#   temp_index   <- NULL
#   temp_dataset <- NULL
#   temp_result  <- NULL
#   
#   temp_index   <- which(hourly_meteo$location_id == i)     # condition on the values
#   temp_dataset <- hourly_meteo[temp_index,]
#   temp_result  <- aggregate( wind_speed_10m..km.h. ~ Month + Year , temp_dataset , FUN = mean )
#   
#   temp_result[,1] <- paste(temp_result[,1],temp_result[,2])
#   
#   result <- a
#   for (j in 1:95) {
#     if(result$Month[j] %in% temp_result[,1]){
#       result$NOx[j] <- temp_result[which(temp_result[,1] == result$Month[j]),3]
#     }
#   }
#   
#   result<- cbind(i,result)
#   covariate<-rbind(covariate,result)
# }
# 
# # consistency check ->OK
# mean(hourly_meteo$wind_speed_10m..km.h.[which(
#   (hourly_meteo$location_id == 6204) &    # 10001   6204 9918
#     (hourly_meteo$Month == "gennaio")& 
#     (hourly_meteo$Year == 16) )])
# 
# # Add to the dataset
# All_values$avg_wind10 <- covariate[,3]
# 
# 
# ##############################################################################
# 
# 
# ###### MAX WIND100 SPEED
# covariate <- NULL
# for (i in sens_id) {
#   temp_index   <- NULL
#   temp_dataset <- NULL
#   temp_result  <- NULL
#   
#   temp_index   <- which(hourly_meteo$location_id == i)     # condition on the values
#   temp_dataset <- hourly_meteo[temp_index,]
#   temp_result  <- aggregate( wind_speed_100m..km.h. ~ Month + Year , temp_dataset , FUN = max )
#   
#   temp_result[,1] <- paste(temp_result[,1],temp_result[,2])
#   
#   result <- a
#   for (j in 1:95) {
#     if(result$Month[j] %in% temp_result[,1]){
#       result$NOx[j] <- temp_result[which(temp_result[,1] == result$Month[j]),3]
#     }
#   }
#   
#   result<- cbind(i,result)
#   covariate<-rbind(covariate,result)
# }
# 
# # consistency check ->OK
# max(hourly_meteo$wind_speed_100m..km.h.[which(
#   (hourly_meteo$location_id == 6204) &  # 10001  6204 9918
#     (hourly_meteo$Month == "gennaio")& 
#     (hourly_meteo$Year == 16) )])
# 
# # Add to the dataset
# All_values$max_wind100 <- covariate[,3]
# 
# 
# 
# ###### AVG WIND100 SPEED
# covariate <- NULL
# for (i in sens_id) {
#   temp_index   <- NULL
#   temp_dataset <- NULL
#   temp_result  <- NULL
#   
#   temp_index   <- which(hourly_meteo$location_id == i)     # condition on the values
#   temp_dataset <- hourly_meteo[temp_index,]
#   temp_result  <- aggregate( wind_speed_100m..km.h. ~ Month + Year , temp_dataset , FUN = mean )
#   
#   temp_result[,1] <- paste(temp_result[,1],temp_result[,2])
#   
#   result <- a
#   for (j in 1:95) {
#     if(result$Month[j] %in% temp_result[,1]){
#       result$NOx[j] <- temp_result[which(temp_result[,1] == result$Month[j]),3]
#     }
#   }
#   
#   result<- cbind(i,result)
#   covariate<-rbind(covariate,result)
# }
# 
# # consistency check ->OK
# mean(hourly_meteo$wind_speed_100m..km.h.[which(
#   (hourly_meteo$location_id == 6204) &    # 10001   6204 9918
#     (hourly_meteo$Month == "gennaio")& 
#     (hourly_meteo$Year == 16) )])
# 
# # Add to the dataset
# All_values$avg_wind100 <- covariate[,3]
# 
# 
# ##############################################################################
# 
# 
# ###### MAX Humidity
# covariate <- NULL
# for (i in sens_id) {
#   temp_index   <- NULL
#   temp_dataset <- NULL
#   temp_result  <- NULL
#   
#   temp_index   <- which(hourly_meteo$location_id == i)     # condition on the values
#   temp_dataset <- hourly_meteo[temp_index,]
#   temp_result  <- aggregate( relative_humidity_2m.... ~ Month + Year , temp_dataset , FUN = max )
#   
#   temp_result[,1] <- paste(temp_result[,1],temp_result[,2])
#   
#   result <- a
#   for (j in 1:95) {
#     if(result$Month[j] %in% temp_result[,1]){
#       result$NOx[j] <- temp_result[which(temp_result[,1] == result$Month[j]),3]
#     }
#   }
#   
#   result<- cbind(i,result)
#   covariate<-rbind(covariate,result)
# }
# 
# # consistency check ->OK
# max(hourly_meteo$relative_humidity_2m....[which(
#   (hourly_meteo$location_id == 6204) &  # 10001  6204 9918
#     (hourly_meteo$Month == "gennaio")& 
#     (hourly_meteo$Year == 16) )])
# 
# # Add to the dataset
# All_values$max_humidity <- covariate[,3]
# 
# 
# 
# ###### AVG Humidity
# covariate <- NULL
# for (i in sens_id) {
#   temp_index   <- NULL
#   temp_dataset <- NULL
#   temp_result  <- NULL
#   
#   temp_index   <- which(hourly_meteo$location_id == i)     # condition on the values
#   temp_dataset <- hourly_meteo[temp_index,]
#   temp_result  <- aggregate( relative_humidity_2m.... ~ Month + Year , temp_dataset , FUN = mean )
#   
#   temp_result[,1] <- paste(temp_result[,1],temp_result[,2])
#   
#   result <- a
#   for (j in 1:95) {
#     if(result$Month[j] %in% temp_result[,1]){
#       result$NOx[j] <- temp_result[which(temp_result[,1] == result$Month[j]),3]
#     }
#   }
#   
#   result<- cbind(i,result)
#   covariate<-rbind(covariate,result)
# }
# 
# # consistency check ->OK
# mean(hourly_meteo$relative_humidity_2m....[which(
#   (hourly_meteo$location_id == 6204) &    # 10001   6204 9918
#     (hourly_meteo$Month == "gennaio")& 
#     (hourly_meteo$Year == 16) )])
# 
# # Add to the dataset
# All_values$avg_humidity  <- covariate[,3]
# 
# 
# ##############################################################################
# 
# 
# ###### MAX Precipitation
# covariate <- NULL
# for (i in sens_id) {
#   temp_index   <- NULL
#   temp_dataset <- NULL
#   temp_result  <- NULL
#   
#   temp_index   <- which(hourly_meteo$location_id == i)     # condition on the values
#   temp_dataset <- hourly_meteo[temp_index,]
#   temp_result  <- aggregate( precipitation..mm. ~ Month + Year , temp_dataset , FUN = max )
#   
#   temp_result[,1] <- paste(temp_result[,1],temp_result[,2])
#   
#   result <- a
#   for (j in 1:95) {
#     if(result$Month[j] %in% temp_result[,1]){
#       result$NOx[j] <- temp_result[which(temp_result[,1] == result$Month[j]),3]
#     }
#   }
#   
#   result<- cbind(i,result)
#   covariate<-rbind(covariate,result)
# }
# 
# # consistency check ->OK
# max(hourly_meteo$precipitation..mm.[which(
#   (hourly_meteo$location_id == 6204) &  # 10001  6204 9918
#     (hourly_meteo$Month == "gennaio")& 
#     (hourly_meteo$Year == 16) )])
# 
# # Add to the dataset
# All_values$max_precipitation <- covariate[,3]
# 
# 
# 
# ###### AVG Precipitation
# covariate <- NULL
# for (i in sens_id) {
#   temp_index   <- NULL
#   temp_dataset <- NULL
#   temp_result  <- NULL
#   
#   temp_index   <- which(hourly_meteo$location_id == i)     # condition on the values
#   temp_dataset <- hourly_meteo[temp_index,]
#   temp_result  <- aggregate( precipitation..mm.~ Month + Year , temp_dataset , FUN = mean )
#   
#   temp_result[,1] <- paste(temp_result[,1],temp_result[,2])
#   
#   result <- a
#   for (j in 1:95) {
#     if(result$Month[j] %in% temp_result[,1]){
#       result$NOx[j] <- temp_result[which(temp_result[,1] == result$Month[j]),3]
#     }
#   }
#   
#   result<- cbind(i,result)
#   covariate<-rbind(covariate,result)
# }
# 
# # consistency check ->OK
# mean(hourly_meteo$precipitation..mm.[which(
#   (hourly_meteo$location_id == 6204) &    # 10001   6204 9918
#     (hourly_meteo$Month == "gennaio")& 
#     (hourly_meteo$Year == 16) )])
# 
# # Add to the dataset
# All_values$avg_precipitation  <- covariate[,3]
# 
# 
# ##############################################################################
# 
# 
# ###### MAX Temperature
# covariate <- NULL
# for (i in sens_id) {
#   temp_index   <- NULL
#   temp_dataset <- NULL
#   temp_result  <- NULL
#   
#   temp_index   <- which(hourly_meteo$location_id == i)     # condition on the values
#   temp_dataset <- hourly_meteo[temp_index,]
#   temp_result  <- aggregate( temperature_2m...C. ~ Month + Year , temp_dataset , FUN = max )
#   
#   temp_result[,1] <- paste(temp_result[,1],temp_result[,2])
#   
#   result <- a
#   for (j in 1:95) {
#     if(result$Month[j] %in% temp_result[,1]){
#       result$NOx[j] <- temp_result[which(temp_result[,1] == result$Month[j]),3]
#     }
#   }
#   
#   result<- cbind(i,result)
#   covariate<-rbind(covariate,result)
# }
# 
# # consistency check ->OK
# max(hourly_meteo$temperature_2m...C.[which(
#   (hourly_meteo$location_id == 6204) &  # 10001  6204 9918
#     (hourly_meteo$Month == "gennaio")& 
#     (hourly_meteo$Year == 16) )])
# 
# # Add to the dataset
# All_values$max_temperature <- covariate[,3]
# 
# 
# 
# ###### AVG Temperature
# covariate <- NULL
# for (i in sens_id) {
#   temp_index   <- NULL
#   temp_dataset <- NULL
#   temp_result  <- NULL
#   
#   temp_index   <- which(hourly_meteo$location_id == i)     # condition on the values
#   temp_dataset <- hourly_meteo[temp_index,]
#   temp_result  <- aggregate( temperature_2m...C.~ Month + Year , temp_dataset , FUN = mean )
#   
#   temp_result[,1] <- paste(temp_result[,1],temp_result[,2])
#   
#   result <- a
#   for (j in 1:95) {
#     if(result$Month[j] %in% temp_result[,1]){
#       result$NOx[j] <- temp_result[which(temp_result[,1] == result$Month[j]),3]
#     }
#   }
#   
#   result<- cbind(i,result)
#   covariate<-rbind(covariate,result)
# }
# 
# # consistency check ->OK
# mean(hourly_meteo$temperature_2m...C.[which(
#   (hourly_meteo$location_id == 6204) &    # 10001   6204 9918
#     (hourly_meteo$Month == "gennaio")& 
#     (hourly_meteo$Year == 16) )])
# 
# # Add to the dataset
# All_values$avg_temperature  <- covariate[,3]
# 
# 
# 
# 
# 
# ###### MIN Temperature
# covariate <- NULL
# for (i in sens_id) {
#   temp_index   <- NULL
#   temp_dataset <- NULL
#   temp_result  <- NULL
#   
#   temp_index   <- which(hourly_meteo$location_id == i)     # condition on the values
#   temp_dataset <- hourly_meteo[temp_index,]
#   temp_result  <- aggregate( temperature_2m...C.~ Month + Year , temp_dataset , FUN = min )
#   
#   temp_result[,1] <- paste(temp_result[,1],temp_result[,2])
#   
#   result <- a
#   for (j in 1:95) {
#     if(result$Month[j] %in% temp_result[,1]){
#       result$NOx[j] <- temp_result[which(temp_result[,1] == result$Month[j]),3]
#     }
#   }
#   
#   result<- cbind(i,result)
#   covariate<-rbind(covariate,result)
# }
# 
# # consistency check ->OK
# min(hourly_meteo$temperature_2m...C.[which(
#   (hourly_meteo$location_id == 6204) &    # 10001   6204 9918
#     (hourly_meteo$Month == "gennaio")& 
#     (hourly_meteo$Year == 16) )])
# 
# # Add to the dataset
# All_values$min_temperature  <- covariate[,3]
# 
# 
# 
# ##############################################################################
# 
# 
# ###### Precipitation theshold
# k <- mean(daily_meteo$precipitation_sum..mm.) # theshold
# covariate <- NULL
# for (i in sens_id) {
#   temp_index   <- NULL
#   temp_dataset <- NULL
#   temp_result  <- NULL
#   
#   temp_index   <- which(daily_meteo$location_id == i  &
#                           daily_meteo$precipitation_sum..mm. > k)     # condition on the values
#   temp_dataset <- daily_meteo[temp_index,]
#   temp_result  <- aggregate( precipitation_sum..mm. ~ Month + Year , temp_dataset , FUN = length )
#   
#   temp_result[,1] <- paste(temp_result[,1],temp_result[,2])
#   
#   result <- a
#   for (j in 1:95) {
#     if(result$Month[j] %in% temp_result[,1]){
#       result$NOx[j] <- temp_result[which(temp_result[,1] == result$Month[j]),3]
#     }
#   }
#   
#   result<- cbind(i,result)
#   covariate<-rbind(covariate,result)
# }
# 
# # consistency check ->OK
# length(daily_meteo$precipitation_sum..mm.[which(
#     (daily_meteo$location_id == 6204) &  # 10001  6204 9918
#     (daily_meteo$Month == "gennaio")&
#     (daily_meteo$precipitation_sum..mm. > k)&
#     (daily_meteo$Year == 16) )])
# 
# # Add to the dataset
# covariate[which(is.na(covariate[,3])),3] <- 0
# All_values$k_precipitation <- covariate[,3]
# 
# 
# 
# ######  wind10 theshold
# k <- mean(daily_meteo$wind_speed_10m_max..km.h.) # theshold
# covariate <- NULL
# for (i in sens_id) {
#   temp_index   <- NULL
#   temp_dataset <- NULL
#   temp_result  <- NULL
#   
#   temp_index   <- which(daily_meteo$location_id == i  &
#                           daily_meteo$wind_speed_10m_max..km.h. > k)     # condition on the values
#   temp_dataset <- daily_meteo[temp_index,]
#   temp_result  <- aggregate( wind_speed_10m_max..km.h. ~ Month + Year , temp_dataset , FUN = length )
#   
#   temp_result[,1] <- paste(temp_result[,1],temp_result[,2])
#   
#   result <- a
#   for (j in 1:95) {
#     if(result$Month[j] %in% temp_result[,1]){
#       result$NOx[j] <- temp_result[which(temp_result[,1] == result$Month[j]),3]
#     }
#   }
#   
#   result<- cbind(i,result)
#   covariate<-rbind(covariate,result)
# }
# 
# # consistency check ->OK
# length(daily_meteo$wind_speed_10m_max..km.h.[which(
#   (daily_meteo$location_id == 6204) &  # 10001  6204 9918
#     (daily_meteo$Month == "gennaio")&
#     (daily_meteo$wind_speed_10m_max..km.h. > k)&
#     (daily_meteo$Year == 16) )])
# 
# # Add to the dataset
# covariate[which(is.na(covariate[,3])),3] <- 0
# All_values$k_wind10 <- covariate[,3]
# 
# 
# colnames(All_values)[2]<-"Time"
# colnames(All_values)[15]<-"k_precipitation"
# 
# ##############################################################################
# 
# ##### group 
# All_values$type <- "???"
# All_values$area <- "???"
# 
# for (i in 1:dim(All_values)[1]) {
#   id_temp <- All_values$Id_sensor[i]
#   All_values$type[i] <- NOx_sensors$Type[which(NOx_sensors$IdSensore == id_temp )]
#   All_values$area[i] <- NOx_sensors$Area[which(NOx_sensors$IdSensore == id_temp )]
# }
# All_values$type[which(All_values$type=="")] <- "???"
# All_values$area[which(All_values$area=="")] <- "???"
# # check sensor
# length(unique(All_values$Id_sensor))
# 
# 
# ##### removing november23 
# All_values <- All_values[-which(All_values$Time == "novembre 23"),]
# 
# 
# #write.csv(All_values, file = "./Data/All_values.csv")


################################################################################

##### transforming the variables

# log concentration
All_values$NOx <- log(All_values$NOx)
gg <- colnames(All_values)

# standardize covariates
for (j in 4:16) {
  
  temp <- scale(All_values[,j], center = T,scale = T)
  All_values[,j] = temp
  
}
colnames(All_values) <- gg
  
################################################################################

##### adding the cos/sin

t = 1:94
All_values$sin12<- round(sin( 1*(2*pi/12)*t), digits = 4)
All_values$cos12<- round(cos( 1*(2*pi/12)*t), digits = 4)

All_values$sin6<- round(sin( 2*(2*pi/12)*t), digits = 4)
All_values$cos6<- round(cos( 2*(2*pi/12)*t), digits = 4)

All_values$sin4<- round(sin( 3*(2*pi/12)*t), digits = 4)
All_values$cos4<- round(cos( 3*(2*pi/12)*t), digits = 4)
  
All_values$sin3<- round(sin( 4*(2*pi/12)*t), digits = 4)
All_values$cos3<- round(cos( 4*(2*pi/12)*t), digits = 4)

# check for correctness
all(All_values$sin12[1:94]== All_values$sin12[95:188])   # first-second sensore
all(All_values$sin12[1:94]== All_values$sin12[8179:8272])# first-last sensor


x11()
matplot(y0, type = "l", ylim = c(-10,5))
lines(All_values$sin12)
lines(All_values$cos12)
lines(All_values$sin6-3)
lines(All_values$cos6-3)
lines(All_values$sin4-6)
lines(All_values$cos4-6)
################################################################################

##### removing collinear variables


# 
# All_values<-All_values[,-c(6,7,8,10,11,12,14)]
# 
# 
# - k_precip, avg_temp, avg_humidity, avgwind10,maxwind10,kwind10
# 


#write.csv(All_values, file = "./Data/All_values.csv")


################################################################################

##### adding coordinates

All_values$lat <-0
All_values$lng <-0

for (i in 1:length(All_values$lat)) {
  All_values$lat[i]=NOx_sensors$lat[which(NOx_sensors$IdSensore==All_values$Id_sensor[i])] 
  All_values$lng[i]=NOx_sensors$lng[which(NOx_sensors$IdSensore==All_values$Id_sensor[i])] 
}



#write.csv(All_values, file = "./Data/All_values.csv")


################################################################################

##### creating dataset without NA

ID_sens <- sort(NOx_sensors$IdSensore)

All_values_Clean <- NULL
for (i in ID_sens) {
  temp <- NULL
  index1 <- which(All_values$Id_sensor == i)
  
  if (sum(is.na(All_values$NOx[index1]))==0){
    temp <- All_values[index1,]
    All_values_Clean <- rbind(All_values_Clean,temp)
  }
}


#write.csv(All_values_Clean, file = "./Data/All_values_Clean.csv")

sum(is.na(All_values_Clean))
