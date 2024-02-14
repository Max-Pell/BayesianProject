### ANALYSIS LIKE SAHU'S BOOK ###
library(bmstdr)
library(ggplot2)

#### DATA POLISHING

alldata <- read.csv("./Data/All_values.csv")
head(alldata)

# remove stations that cannot be classified
alldata<-alldata[-which(alldata$type=='???'),]
# remove stations for which we have nans
nas<-unique(alldata[which(is.na(alldata$NOx)),2])
alldata<-alldata[-which(alldata$Id_sensor%in%nas),]
# in the end we have 54 stations on which we can perform the analysis

# Plot of the time series one for each station (log scale)
colScale<- c('red','blue','yellow')
matplot((alldata$NOx[c(1:94)]), type = "l", 
        ylim = c(min((alldata$NOx), na.rm = T), max((alldata$NOx), na.rm = T))
       ,col=colScale[as.factor(alldata$type)][1] )
for(i in 1:54){
  matlines((alldata$NOx[c(1:94)+94*i]), type = "l", 
           col=colScale[as.factor(alldata$type)][i*94] )
}

# identify the lowest one and remove it since it is an outlier
which(alldata$NOx==min(alldata$NOx,na.rm = T))
alldata[3562,]$Id_sensor #20441
alldata<-alldata[-which(alldata$Id_sensor=='20441'),]

colScale<- c('red','blue','yellow')
matplot((alldata$NOx[c(1:94)]), type = "l", 
        ylim = c(min((alldata$NOx), na.rm = T), max((alldata$NOx), na.rm = T))
        ,col=colScale[as.factor(alldata$type)][1] )
for(i in 1:54){
  matlines((alldata$NOx[c(1:94)+94*i]), type = "l", 
           col=colScale[as.factor(alldata$type)][i*94] )
}

traffic<- alldata[which(alldata$type=='T'),]
industrial<-alldata[which(alldata$type=='I'),]
background<-alldata[which(alldata$type=='B'),]

par(mfrow=c(3,1))
matplot(industrial$NOx[c(1:94)], type = "l", 
        ylim = c(min((industrial$NOx), na.rm = T), max((industrial$NOx), na.rm = T)))
for(i in 1:4){
  matlines((industrial$NOx[c(1:94)+94*i]), type = "l")
}

matplot(traffic$NOx[c(1:94)], type = "l", 
        ylim = c(min((traffic$NOx), na.rm = T), max((traffic$NOx), na.rm = T)))
for(i in 1:17){
  matlines((traffic$NOx[c(1:94)+94*i]), type = "l")
}

matplot(background$NOx[c(1:94)], type = "l", 
        ylim = c(min((background$NOx), na.rm = T), max((background$NOx), na.rm = T)))
for(i in 1:32){
  matlines((background$NOx[c(1:94)+94*i]), type = "l")
}

rural<- alldata[which(alldata$area=='R'),]
urban<-alldata[which(alldata$area=='U'),]
suburban<-alldata[which(alldata$area=='S'),]

par(mfrow=c(3,1))
matplot(rural$NOx[c(1:94)], type = "l", 
        ylim = c(min((rural$NOx), na.rm = T), max((rural$NOx), na.rm = T)))
for(i in 1:4){
  matlines((rural$NOx[c(1:94)+94*i]), type = "l")
}

matplot(urban$NOx[c(1:94)], type = "l", 
        ylim = c(min((urban$NOx), na.rm = T), max((urban$NOx), na.rm = T)))
for(i in 1:37){
  matlines((urban$NOx[c(1:94)+94*i]), type = "l")
}

matplot(suburban$NOx[c(1:94)], type = "l", 
        ylim = c(min((suburban$NOx), na.rm = T), max((suburban$NOx), na.rm = T)))
for(i in 1:12){
  matlines((suburban$NOx[c(1:94)+94*i]), type = "l")
}
dev.off()

# from this plot we can conclude that the seasonality is common between types
# and that the only thing that changes is that traffic ones are higher
# moreover wrt the area variable there is no significant effect on the shape or
# in the intercept

alldata$sin12<- round(sin( 1*(2*pi/12)*c(4:97)), digits = 4)
alldata$cos12<- round(cos( 1*(2*pi/12)*c(4:97)), digits = 4)

# creating the "dummy" for the station specific effect
alldata$Id_sensor <- as.factor(alldata$Id_sensor)
alldata$type <- as.factor(alldata$type)

alldata$istraffic <- as.numeric(alldata$type=='T')
alldata$nottraffic <- as.numeric(alldata$type!='T')

levels(alldata$Id_sensor)
levels(alldata$type)

# splitting in training set (fitting) and test (forecasting)
trains <- getvalidrows(53, 94, valids=1:53, validt=61:94) 
dfit <- alldata[-trains, ] # data for fitting 
dfore <- alldata[trains, ] # data for forecasting

# just traffic
traffic<- alldata[which(alldata$type=='T'),]
vst <- getvalidrows(17, 94, valids=1:17, validt=61:94) 
dfitt <- traffic[-vst, ] # data for fitting 
dforet <- traffic[vst, ] # data for forecasting

# FORMULA for the model: we want to express the NOx variable in terms of covariates
# and a seasonality terms which depends on the station.
# bmstdr models one station at the time so to create the same effect I think 
# we should treat in some sense each station separately

f1 <- NOx ~ max_wind10 + avg_humidity + avg_temperature + k_precipitation + 
  k_wind10 + (sin12+ cos12 + 1)*type

# bayesian linear regression model
M1 <- Bsptime(model="lm", formula=f1, data=dfit, scale.transform = "NONE", 
              N=1500, mchoice = T)
summary(M1)
quartz()
plot(M1)

# bayesian spatio-temporal model with a separable covariance structure
M2 <- Bsptime(model="separable", formula=f1, data=dfit, 
              scale.transform = "NONE", coordtype="lonlat", coords=c(29,28),  
              N=1500, burn.in = 500 , mchoice = T)
summary(M2)
M2$phi.s
M2$phi.t
residuals(M2) # residuals shouldn't show any pattern
quartz()
plot(M2)

tab1<- cbind(M1$params, M2$params)

# gaussian process model
library(spTimer)
M3 <- Bsptime(package="spTimer",model="GP", formula=f1, data = dfit, 
              coordtype="lonlat", coords=c(29,28), scale.transform = "NONE", 
              N=1500, burn.in = 500, n.report=1, mchoice = T)
sn<-M3$sn
tn<-M3$tn
summary(M3)
residuals(M3) # quite better
quartz()
plot(M3)
tab2<- M3$params

tab3<-cbind(M1$mchoice, M2$mchoice, M3$mchoice)
library(loo)
tab4<- cbind(loo(M1$logliks$loglik),
loo(M2$logliks$loglik),
loo(M3$logliks$loglik))

# auto regressive model
M4 <- Bsptime(package="spTimer", model="AR", formula=f1, data=dfit, 
                coordtype="lonlat", coords=c(29,28), scale.transform = "NONE", 
                mchoice=T, n.report=1, N=1500, burn.in = 500)
summary(M4)
residuals(M4)

quartz()
plot(M4)

tab5<- cbind(M1$mchoice[9:11], M2$mchoice[9:11], M3$mchoice[9:11], M4$mchoice)
print(tab5)
# probably the autoregressive one could be fine

# validations
stat<- sample(unique(dfit$Id_sensor), 5)
vstat<- which(dfit$Id_sensor%in%stat)

M1v <- Bsptime(model="lm", formula=f1, data=dfit, scale.transform = "NONE", 
              N=1500, validrows = vstat)
M2v <- Bsptime(model="separable", formula=f1, data=dfit, 
              scale.transform = "NONE", coordtype="lonlat", coords=c(29,28),  
              N=1500, burn.in = 500 ,validrows = vstat)
M3v <- Bsptime(package="spTimer",model="GP", formula=f1, data = dfit, 
              coordtype="lonlat", coords=c(29,28), scale.transform = "NONE", 
              N=1500, burn.in = 500, n.report=1, validrows = vstat)
M4v <- Bsptime(package="spTimer", model="AR", formula=f1, data=dfit, 
              coordtype="lonlat", coords=c(29,28), scale.transform = "NONE", 
              validrows = vstat, n.report=1, N=1500, burn.in = 500)

tab6<- cbind(M1v$stats, M2v$stats, M3v$stats, M4v$stats)


# dynamic model: our betas are varying in space: instead of putting the spatial
# effect as a dummy depending on the location of the sensor we say that the 
# seasonality is space-dependent

library(spTDyn)

# spatially varying coefficients for the seasonality
f2 <- NOx ~ avg_temperature + sin12 + cos12 + 
   + sp(sin12) + sp(cos12) + type

M5 <- Bsptime(package="sptDyn", model="GP", formula=f2, data= dfit, 
              coordtype="lonlat", coords=c(29,28), scale.transform = "NONE", 
              mchoice = T)

summary(M5)
residuals(M5)

tab<-cbind(tab2, M5$mchoice)
print(tab)

M5v <- Bsptime(package="sptDyn", model="GP", formula=f2, data= dfit, 
              coordtype="lonlat", coords=c(29,28), scale.transform = "NONE", 
              validrows = vstat)
M5v$stats

# Model 8
M8 <- Bsptime(package="spBayes", formula=f1, data=dfit,coordtype="lonlat", 
              coords=c(29,28), scale.transform = "NONE", mchoice=T) 
summary(M8)
M8$tn
M8$sn
residuals(M8)
plot(M8)

# bayesian spatio-temporal model with a separable covariance structure
M9 <- Bsptime(package ="spTimer", model = "GPP", g_size=5, formula=f1, 
              data =dfit, coordtype ="lonlat", coords=c(29,28), 
              scale.transform = "NONE", mchoice = T)

tabf<-cbind(tab2, M8$mchoice, M9$mchoice)


# PREDICTIONS: 
# we train the model on the whole dataset, but with the argument
# validrows it set aside the rows of the years after the change-point.
# In this way we can see the performances on those fresh new datas, and understand
# wether the change had an impact or not based on our previous knowledge of the values
# of NOx
# splitting in training set (fitting) and test (forecasting)
M3s <- Bsptime(package="spTimer",formula = f1, data = alldata, 
               coordtype = "lonlat", coords=c(29,28),
               validrows = trains  ,model="GP", 
               mchoice=F, n.report = 1, scale.transform = "NONE", N=1500, burn.in= 500)

summary(M3s)
M3s$validationplots
names(M3s$fit)

meanpred<-M3s$yobs_preds$meanpred
lower_bound<- M3s$yobs_preds$low
upper_bound<- M3s$yobs_preds$up

matplot(meanpred[1:33], type='l', ylim = c(0,10))
matlines(lower_bound[1:33], type='l')
matlines(upper_bound[1:33], type='l')
matlines(alldata$NOx[61:94], col='red')

mean_diff <- meanpred - alldata[trains,]$NOx
mean_diff<- as.data.frame(mean_diff)
mean_diff$Id_sensors<- alldata[trains,]$Id_sensor
final<-rowsum(mean_diff$mean_diff, group = mean_diff$Id_sensor) # difference
# between the prediction and true value! The stations with cumulated higher 
# difference in absolute value are the ones in which the effect was higher! 

# confront the predicted time series vs the true one for the sensor 10431
dev.off()

par(mfrow=c(3,1))
quartz()
matplot(alldata$NOx[1:94], type='l', col = 'blue', ylab = 'NOx',ylim = c(2,5))
matlines(M3s$fit$fitted[1:94,1], col='red')
legend('bottomright', legend = c('measured', 'estimated'), fill = c('blue', 'red'))
title(main='plot for sensor 10431 (B)' )

matplot(alldata$NOx[2163:2256], type='l', col = 'blue', ylab = 'NOx',ylim = c(2,5))
matlines(M3s$fit$fitted[2163:2256,1], col='red')
title(main='plot for sensor 9968 (I)' )

matplot(alldata$NOx[471:564], type='l', col = 'blue', ylab = 'NOx',ylim = c(2,5))
matlines(M3s$fit$fitted[471:564], col='red')
title(main='plot for sensor 6398 (T)' )



