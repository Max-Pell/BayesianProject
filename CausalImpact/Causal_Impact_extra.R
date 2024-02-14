library(ape)
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
library(sf)
library(coda)


##### LOADING THE NEW DATASETS

NOx_sensors <- read.csv("NOx_sensors.csv")
NOx_sensors <- NOx_sensors[,-1]
All_values <- read.csv("All_values_Clean.csv")
All_values <- All_values[,-1]


### NB open/close the {} for an easier reading (ex the one at line 63)


################################################################################
##################################   PLOTS   ###################################
################################################################################
#  some initial plot
{
NOx_T   <- NULL
NOx_I   <- NULL
NOx_B   <- NULL
NOx_boh <- NULL

for(i in 0:(length(unique(All_values$Id_sensor))-1) ){
  if (All_values$type[i*94+1] == "T")
    NOx_T <- cbind(NOx_T,All_values$NOx[(i*94+1):((i+1)*94)])
  if (All_values$type[i*94+1] == "I")
    NOx_I <- cbind(NOx_I,All_values$NOx[(i*94+1):((i+1)*94)])
  if (All_values$type[i*94+1] == "B")
    NOx_B <- cbind(NOx_B,All_values$NOx[(i*94+1):((i+1)*94)])
  if (All_values$type[i*94+1] == "???")
    NOx_boh <- cbind(NOx_boh,All_values$NOx[(i*94+1):((i+1)*94)])
}



###### NOX plot
x11()
#par(mfrow = c(2,2))
matplot(cbind(NOx_T,NOx_I,NOx_B), type ="l", main="Log-Concentration of the NOX", xlab="month")
grid()
#abline(v=c(0,12,24,36,48,60,72,84))
abline(v=c(45,61), col = 1, lw= 3)


matplot(NOx_I, type ="l", main="Industrial")
#abline(v=c(0,12,24,36,48,60,72,84))
abline(v=c(45,61), col = "red")
matplot(NOx_B, type ="l", main="Background")
#abline(v=c(0,12,24,36,48,60,72,84))
abline(v=c(45,61), col = "red")
matplot(NOx_boh, type ="l", main="Boh")
#abline(v=c(0,12,24,36,48,60,72,84))
abline(v=c(45,61), col = "red")



# ###### log(NOX) plot
# x11()
# par(mfrow = c(2,2))
# matplot(log(NOx_T), type ="l", main="Traffic")
# #abline(v=c(0,12,24,36,48,60,72,84))
# abline(v=c(45,61), col = "red")
# matplot(log(NOx_I), type ="l", main="Industrial")
# #abline(v=c(0,12,24,36,48,60,72,84))
# abline(v=c(45,61), col = "red")
# matplot(log(NOx_B), type ="l", main="Background")
# #abline(v=c(0,12,24,36,48,60,72,84))
# abline(v=c(45,61), col = "red")
# matplot(log(NOx_boh), type ="l", main="Boh")
# #abline(v=c(0,12,24,36,48,60,72,84))
# abline(v=c(45,61), col = "red")



}

################################################################################
###########################  Causal impact   ###################################
################################################################################
# first causal impact model
{
  
pre.period <- c(1, 45)
post.period <- c(46, 94) #94


######## creating the "zoo object"
y <- All_values[,3]

data1 <- NULL
x1 <-  All_values[,4]
data1 <- cbind(y,x1)
xxx <- NULL

j = 5
for(i in c(5:16,19:24)){
  xxx <- All_values[,i]
  data1 <- cbind(data1,xxx)
  
  colnames(data1)[j-2] <- paste0("x",(j-3))
  xxx <- NULL
  j <- j +1
}

#index1 <-(95:188)
#data1 <- data1[index1,]
#impact <- CausalImpact(data1, pre.period, post.period)
#x11()
#plot(impact)


########################### single sensor
#effect_T[1,]
index1 <- which(All_values$Id_sensor == 6204)

# matplot
x11()
#par(mfrow = c(1,2))
matplot(All_values$NOx[index1], type ="l", main="Single", col = "blue", lw=2,
        xlab = "month",ylab = "log(NOX)")
#abline(v=c(0,12,24,36,48,60,72,84))
abline(v=c(45), col = "red", lw=2)
grid()

#matplot(log(All_values$NOx[index1]), type ="l", main="Single")
##abline(v=c(0,12,24,36,48,60,72,84))
#abline(v=c(45,61), col = "red")

# impact

impact <- CausalImpact(data1[index1,], pre.period, post.period,
                       ## uncomment to see issues with local level
                       # model.args = list(prior.level.sd=0.05)
                       )
x11()
plot(impact)

 #a <- impact$model$bsts.model

#plot(bsttt,"components")

bsttt <- impact$model$bsts.model
##################################  spatial plot

}

################################################################################
##################################   BSTS MODEL   ##############################
################################################################################
################################################################################
#  model comparisons
{
  
##### Data preparation
pre.period <- c(1, 45)
post.period <- c(46, 94) #94

# Selecting one sensor
#effect_T[1,]
index1 <- which(All_values$Id_sensor == 6204)

# Formatting the target variable
y0  <- All_values[index1,3]
post.period.response <- y0[post.period[1] : post.period[2]]
y<-y0
y[post.period[1] : post.period[2]] <- NA


# Removing linear correlated
df2 <- All_values[index1,c(4:16)]

#x11()
#ggpairs(All_values[,c(4:16)])

df2 <- All_values[index1,c(4:16)]
df2 <- df2[,-c(3,4,9,11)]
# 
# y0  <- All_values[index1,3]
# y1  <-log(10^y0)
# x11()
# matplot(cbind(y0,y1),type="l")

################################################################################

###### BSTS model

##### seasonality
ss <- NULL
ss  <- AddStaticIntercept(list(),y)
# bsts0 <- bsts(y,state.specification = ss,niter = 1500) 


##### Covariates
bsts1 <- bsts(y ~ .,
              state.specification = ss,
              niter = 1500,
              data = df2,expected.model.size = 1)  
bsts2 <- bsts(y ~ .,
              state.specification = ss,
              niter = 1500,
              data = df2,
              expected.model.size = 2) 
bsts3 <- bsts(y ~ .,
              state.specification = ss,
              niter = 1500,
              data = df2,
              expected.model.size = 3) 
bsts4 <- bsts(y ~ .,
              state.specification = ss,
              niter = 1500,
              data = df2,
              expected.model.size = 4) 
bsts5 <- bsts(y ~ .,
              state.specification = ss,
              niter = 1500,
              data = df2,
              expected.model.size = 5) 
bsts6 <- bsts(y ~ .,
              state.specification = ss,
              niter = 1500,
              data = df2,
              expected.model.size = 6) 
#comparing models
x11()
CompareBstsModels(list("Model 1" = bsts1,
                       "Model 2" = bsts2,
                       "Model 3" = bsts3,
                       "Model 4" = bsts4,
                       "Model 5" = bsts5,
                       "Model 6" = bsts6),
                  colors = c(1,2,3,4,5,6),burn = 500)
abline(v=45)

x11()
par(mfrow = c(3,2))
plot(bsts1,burn = 500)
plot(bsts2,burn = 500)
plot(bsts3,burn = 500)
plot(bsts4,burn = 500)
plot(bsts5,burn = 500)
plot(bsts6,burn = 500)

x11()
par(mfrow = c(3,2))
plot(bsts1,"coef",burn = 500)
plot(bsts2,"coef",burn = 500)
plot(bsts3,"coef",burn = 500)
plot(bsts4,"coef",burn = 500)
plot(bsts5,"coef",burn = 500)
plot(bsts6,"coef",burn = 500)

x11()
par(mfrow = c(3,2))
plot(bsts1,"comp",burn = 500)
plot(bsts2,"comp",burn = 500)
plot(bsts3,"comp",burn = 500)
plot(bsts4,"comp",burn = 500)
plot(bsts5,"comp",burn = 500)
plot(bsts6,"comp",burn = 500)

x11()
acf(bsts2$final.state)

bsts2$final.state

# => we selected 3, since it improve from 1 and the three covariates have
#    similar inclusion probability

##### trends
ss1 <- AddLocalLinearTrend(list(), y)
ss2 <- AddSemilocalLinearTrend(list(), y)
ss5 <- AddLocalLevel(list(),y)
ss3 <- AddAr(ss,y,lags = 1)
ss4 <- AddAutoAr(ss,y,lags = 2)

bsts2 <- bsts(y ~ .,
               state.specification = ss,
               niter = 1500,
               data = df2,
               expected.model.size = 3)       
bsts21 <- bsts(y ~ .,
              state.specification = ss1,
              niter = 1500,
              data = df2,
              expected.model.size = 3) 
bsts22 <- bsts(y ~ .,
              state.specification = ss2,
              niter = 1500,
              data = df2,
              expected.model.size = 3)
bsts23 <- bsts(y ~ .,
              state.specification = ss3,
              niter = 1500,
              data = df2,
              expected.model.size = 3)
bsts24 <- bsts(y ~ .,
               state.specification = ss4,
               niter = 1500,
               data = df2,
               expected.model.size = 3)
bsts25 <- bsts(y ~ .,
               state.specification = ss5,
               niter = 1500,
               data = df2,
               expected.model.size = 3)
#comparing models
x11()
CompareBstsModels(list("intercept" = bsts2,
                       "local" = bsts25,
                       "local linear" = bsts21,
                       "semilocal linear" = bsts22,
                       "AR(1)" = bsts23,
                       "AR(2)" = bsts24),
                  colors = c(1,2,3,4,5,6), burn = 500)

x11()
par(mfrow = c(3,2))
plot(bsts2 ,burn = 500,  main ="intercept")
plot(bsts25,burn = 500, main ="local")
plot(bsts21,burn = 500, main ="local linear")
plot(bsts22,burn = 500, main ="semilocal linear")
plot(bsts23,burn = 500, main ="AR(1)")
plot(bsts24,burn = 500, main ="AR(2)")


x11()
plot(bsts2,"comp",burn = 500)
x11()
plot(bsts25,"comp",burn = 500)
x11()
plot(bsts21,"comp",burn = 500)
x11()
plot(bsts22,"comp",burn = 500)
x11()
plot(bsts23,"comp",burn = 500)
x11()
plot(bsts24,"comp",burn = 500)

## => - due to the fast grow in uncertainty in forehead prediction we discard locallinear
#     and semilocallinear
#     - due to the worse prediction of the future we discard Ar(1)

#  => ar(2),intercept,local


##### Seasonality vs Harmonic
ss31 <- AddSeasonal(list(), y, nseasons = 12, season.duration = 1)
ss32 <- AddTrig(list(), y, period = 12, frequencies = 1)
ss33 <- AddSeasonal(ss, y, nseasons = 12, season.duration = 1)
ss34 <- AddTrig(ss, y, period = 12, frequencies = 1)
ss35 <- AddTrig(ss3, y, period = 12, frequencies = 1)

bsts2 <- bsts(y ~ .,
              state.specification = ss,
              niter = 1500,
              data = df2,
              expected.model.size = 3) 
bsts31 <- bsts(y ~ .,
               niter = 1500,
               state.specification = ss31,
               data = df2,
               expected.model.size = 3)
bsts32 <- bsts(y ~ .,
               niter = 1500,
               state.specification = ss32,
               data = df2,
               expected.model.size = 3)

bsts33 <- bsts(y ~ .,
               niter = 1500,
               state.specification = ss33,
               data = df2,
               expected.model.size = 3)
bsts34 <- bsts(y ~ .,
               niter = 1500,
               state.specification = ss34,
               data = df2,
               expected.model.size = 3)
bsts35 <- bsts(y ~ .,
               niter = 1500,
               state.specification = ss35,
               data = df2,
               expected.model.size = 3)
#comparing models
x11()
CompareBstsModels(list("intercept"    = bsts2,
                       "seasonality"  = bsts31,
                       "harmonic"     = bsts32,
                       "int+seasonality" = bsts33,
                       "int+harmonic" = bsts34,
                       "all" = bsts35),
                  colors = c(1,2,3,4,5,6), burn = 500)
x11()
par(mfrow = c(3,2))
plot(bsts2 ,burn = 500, main = "intercept")
plot(bsts35,burn = 500, main = "all")
plot(bsts31,burn = 500, main = "seasonality")
plot(bsts32,burn = 500, main = "harmonic")
plot(bsts33,burn = 500, main = "int+seasonality")
plot(bsts34,burn = 500, main = "int+harmonic")

# best is are harmonic and int+ armonic



##### Harmonic frequencies
ss41 <- AddTrig(list(), y, period = 12, frequencies = 1)
ss42 <- AddTrig(ss, y, period = 12, frequencies = 1)
ss43 <- AddTrig(list(), y, period = 12, frequencies = 1:4)
ss44 <- AddTrig(ss, y, period = 12, frequencies = 1:4)

bsts41 <- bsts(y ~ .,
              state.specification = ss41,
              niter = 1500,
              data = df2,
              expected.model.size = 3) 
bsts42 <- bsts(y ~ .,
               niter = 1500,
               state.specification = ss42,
               data = df2,
               expected.model.size = 3)
bsts43 <- bsts(y ~ .,
               niter = 1500,
               state.specification = ss43,
               data = df2,
               expected.model.size = 3)

bsts44 <- bsts(y ~ .,
               niter = 1500,
               state.specification = ss44,
               data = df2,
               expected.model.size = 3)

#comparing models
x11()
CompareBstsModels(list("intercept"     = bsts2,
                       "AR(2)"         = bsts24,
                       "harmonic1"     = bsts41,
                       "int+harmonic1" = bsts42,
                       "harmonic2"     = bsts43,
                       "int+harmonic2" = bsts44),
                  colors = c(1,2,3,4,5,6), burn = 500)
x11()
par(mfrow = c(3,2))
plot(bsts2 ,burn = 500, main = "intercept")
plot(bsts24,burn = 500, main = "AR(2)")
plot(bsts41,burn = 500, main = "harmonic1")
plot(bsts42,burn = 500, main = "int+harmonic1")
plot(bsts43,burn = 500, main = "harmonic2")
plot(bsts44,burn = 500, main = "int+harmonic2")

# best are harmonic1 and int+harmonic1 and AR(1)



##### Harmonic vs direct
ss51 <- AddTrig(list(), y, period = 12, frequencies = 1, method = "harmonic")
ss52 <- AddTrig(list(), y, period = 12, frequencies = 1, method = "direct")
ss53 <- AddTrig(ss, y, period = 12, frequencies = 1, method = "harmonic")
ss54 <- AddTrig(ss, y, period = 12, frequencies = 1, method = "direct")
ss55 <- AddTrig(list(), y, period = 12, frequencies = 1:2, method = "harmonic")
ss56 <- AddTrig(list(), y, period = 12, frequencies = 1:2, method = "direct")

bsts51 <- bsts(y ~ .,
               state.specification = ss51,
               niter = 1500,
               data = df2,
               expected.model.size = 3) 
bsts52 <- bsts(y ~ .,
               niter = 1500,
               state.specification = ss52,
               data = df2,
               expected.model.size = 3)
bsts53 <- bsts(y ~ .,
               niter = 1500,
               state.specification = ss53,
               data = df2,
               expected.model.size = 3)

bsts54 <- bsts(y ~ .,
               niter = 1500,
               state.specification = ss54,
               data = df2,
               expected.model.size = 3)
bsts55 <- bsts(y ~ .,
               niter = 1500,
               state.specification = ss55,
               data = df2,
               expected.model.size = 3)
bsts56 <- bsts(y ~ .,
               niter = 1500,
               state.specification = ss56,
               data = df2,
               expected.model.size = 3)

#comparing models
x11()
CompareBstsModels(list("h1"     = bsts51,
                       "d1"         = bsts52,
                       "int+h1"     = bsts53,
                       "int+d1" = bsts54,
                       "h2"     = bsts55,
                       "d2" = bsts56),
                  colors = c(1,2,3,4,5,6), burn = 500)
x11()
par(mfrow = c(3,2))
plot(bsts51 ,burn = 500, main = "h1")
plot(bsts52,burn = 500, main = "d1")
plot(bsts53,burn = 500, main = "int+h1")
plot(bsts54,burn = 500, main = "int+d1")
plot(bsts55,burn = 500, main = "h2")
plot(bsts56,burn = 500, main = "d2")

##  int+h1,  h1

################################################################################
##############################################################
################################################################################
##### comparing different models
ss <- NULL
ss  <- AddStaticIntercept(list(),y)
ss3 <- AddAr(ss,y,lags =3)
ss51 <- AddTrig(list(), y, period = 12, frequencies = 1, method = "harmonic")
ss53 <- AddTrig(ss, y, period = 12, frequencies = 1, method = "harmonic")
ss61 <- AddTrig(ss3, y, period = 12, frequencies = 1, method = "harmonic")
ss62 <- AddAr(list(),y,lags = 3)



bsts2 <- bsts(y ~ .,
              state.specification = ss,
              niter = 1500,
              data = df2,
              expected.model.size = 3)       
bsts23 <- bsts(y ~ .,
               state.specification = ss3,
               niter = 1500,
               data = df2,
               expected.model.size = 3)
bsts51 <- bsts(y ~ .,
               state.specification = ss51,
               niter = 1500,
               data = df2,
               expected.model.size = 3) 
bsts53 <- bsts(y ~ .,
               niter = 1500,
               state.specification = ss53,
               data = df2,
               expected.model.size = 3)
bsts61 <- bsts(y ~ .,
               niter = 1500,
               state.specification = ss61,
               data = df2,
               expected.model.size = 3)
bsts62 <- bsts(y ~ .,
               niter = 1500,
               state.specification = ss62,
               data = df2,
               expected.model.size = 3)

#comparing models
x11()
CompareBstsModels(list("intercept"     = bsts2,
                       "int+AR(2)+h1" = bsts61,
                       "AR(2)"     = bsts62,
                       "int+AR(2)"     = bsts23,
                       "h1"     = bsts51,
                       "int+h1"     = bsts53),
                  colors = c(1,2,3,4,5,6), burn = 500)
x11()
par(mfrow = c(3,2))
plot(bsts2 ,burn = 500, main = "intercept")
plot(bsts61,burn = 500, main = "int+AR(2)+h1")
plot(bsts62,burn = 500, main = "AR(2)")
plot(bsts23,burn = 500, main = "int+AR(2)")
plot(bsts51,burn = 500, main = "h1")
plot(bsts53,burn = 500, main = "int+h1")



################################################################################
#  using also the sin/cos covariates
df3 <- All_values[index1,c(4:16,19:24)]



ss2  <- AddStaticIntercept(list(),y)
ss3 <- AddAr(ss2,y,lags =2)
ss4 <- AddTrig(list(), y, period = 12, frequencies = 1, method = "harmonic")



bsts2 <- bsts(y ~ .,
              state.specification = ss2,
              niter = 2000,
              data = df2,
              expected.model.size = 3)

bsts2e <- bsts(y ~ .,
              state.specification = ss2,
              niter = 5000,
              data = df3,
              expected.model.size = 10)    

bsts3 <- bsts(y ~ .,
               state.specification = ss3,
               niter = 2000,
               data = df2,
               expected.model.size = 3)
bsts3e <- bsts(y ~ .,
               state.specification = ss3,
               niter = 2000,
               data = df3,
               expected.model.size = 10) 
bsts4 <- bsts(y ~ .,
               niter = 2000,
               state.specification = ss4,
               data = df2,
               expected.model.size = 3)
bsts4e <- bsts(y ~ .,
               niter = 2000,
               state.specification = ss4,
               data = df3,
               expected.model.size = 10)


#comparing models
x11()
CompareBstsModels(list("intercept"     = bsts2,
                       "interceptE" = bsts2e,
                       "int+AR(2)"     = bsts3,
                       "int+AR(2)E"     = bsts3e,
                       "h1"     = bsts4,
                       "h1E"     = bsts4e),
                  colors = c(1,2,3,4,5,6), burn = 500)
x11()
par(mfrow = c(3,2))
plot(bsts2 ,burn = 500, main = "intercept")
plot(bsts2e,burn = 500, main = "interceptE")
plot(bsts3,burn = 500, main = "int+AR(2)")
plot(bsts3e,burn = 500, main = "int+AR(2)E")
plot(bsts4,burn = 500, main = "h1")
plot(bsts4e,burn = 500, main = "h1E")


x11()
par(mfrow = c(1,3))
plot(bsts2e,"coef",burn = 500)
plot(bsts3,"coef",burn = 500)
plot(bsts4,"coef",burn = 500)


x11()
plot(bsts2e,"comp",burn = 500)
x11()
plot(bsts3e,"comp",burn = 500)
x11()
plot(bsts4,"comp",burn = 500)




################################################################################
#  using also the sin/cos covariates
df3 <- All_values[index1,c(4:16,19:24)]

#y <- y[1:45]
#df2 <- df2[1:45,]
#df3 <- df3[1:45,]

ss2  <- AddStaticIntercept(list(),y)
ss3 <- AddAr(ss2,y,lags =2)
ss4 <- AddTrig(ss2, y, period = 12, frequencies = 1, method = "harmonic")


ss5 <- AddLocalLevel(list(),y,
                     sigma.prior = SdPrior(sigma.guess =0.01, sample.size = 20) ,
                     initial.state.prior = NormalPrior(0, 2))
#ss6 <- AddAr(ss5,y,lags =2)
ss7 <- AddTrig(ss2, y, period = 12, frequencies = 1, method = "harmonic")



bsts2 <- bsts(y ~ .,
              state.specification = ss2,
              niter = 2000,
              data = df2,
              expected.model.size = 3)

bsts2e <- bsts(y ~ .,
               state.specification = ss2,
               niter = 2000,
               data = df3,
               expected.model.size = 10)    

bsts3 <- bsts(y ~ .,
              state.specification = ss3,
              niter = 2000,
              data = df3,
              expected.model.size = 3)
bsts3e <- bsts(y ~ .,
              niter = 2000,
              state.specification = ss3,
              data = df3,
              expected.model.size = 3)


bsts5 <- bsts(y ~ .,
               state.specification = ss5,
               niter = 2000,
               data = df2,
               expected.model.size = 10)    

bsts5e <- bsts(y ~ .,
              state.specification = ss5,
              niter = 2000,
              data = df3,
              expected.model.size = 3)
bsts4 <- bsts(y ~ .,
              niter = 2000,
              state.specification = ss4,
              data = df2,
              expected.model.size = 3)



#comparing models
x11()
CompareBstsModels(list(
                       "FixInt+Reg" = bsts2,
                       "FixInt+Reg_ex" = bsts2e,
                       #"FixInt and weather + AR(2)"     = bsts3,
                       "FixInt+Reg_ex+AR(2)"     = bsts3e,
                       "LocLv+Reg" = bsts5,
                       "LocLv+Reg_ex" = bsts5e,
                       "Harmonic+Reg"     = bsts4
                       #"h1b"     = bsts4,
                       #"AAAAA" =bsttt
                       ),
                  colors = c(1,2,3,4,5,6), burn = 500)
x11()
par(mfrow = c(3,2))

plot(bsts2,burn = 500, main = "FixInt+Reg")
plot(bsts2e,burn = 500, main = "FixInt+Reg_ex")
plot(bsts3e,burn = 500, main = "FixInt+Reg_ex+AR(2)" )
plot(bsts5,burn = 500, main = "LocLv+Reg" )
plot(bsts5e,burn = 500, main = "LocLv+Reg_ex")
plot(bsts4,burn = 500, main = "Harmonic+Reg")


x11()
par(mfrow = c(1,3))
plot(bsts2e,"coef",burn = 500)
plot(bsts3,"coef",burn = 500)
plot(bsts4,"coef",burn = 500)


x11()
plot(bsts2eb,"comp",burn = 500)
x11()
plot(bsts3,"comp",burn = 500)
x11()
plot(bsts4,"comp",burn = 500)





######  best three models:

# [intercept + covariates]  => bsts2
# int + AR(1) + covariates  => bsts23
# harmonic1 + covariate     => bsts51
# int+AR(1)+h1              => bsts61

# aa <- residuals(bsts51,
#           burn = 500,
#           mean.only = TRUE)
# b <- cumsum(aa[1:45])
# matplot(-b,type="l")

##################################

#
#plot(bsts1, "help")


impact <- CausalImpact(bsts.model = bsts4e,
                       post.period.response = post.period.response)

#CausalImpact(data2, pre.period, post.period)

x11()
plot(impact)

summary(impact)

summary(impact, "report")

plot(bsts2 ,burn = 500, main = "intercept")
plot(bsts35,burn = 500, main = "all")
plot(bsts31,burn = 500, main = "seasonality")
plot(bsts32,burn = 500, main = "harmonic")
plot(bsts33,burn = 500, main = "int+seasonality")
plot(bsts34,burn = 500, main = "int+harmonic")


# r <- residuals(bsts2)
# r <- r[,c(1:45)]
# par(mfrow = c(1,2))
# qqdist(r) ## A bit of departure in the upper tail
# AcfDist(r)



y0  <- log(All_values[index1,3])
post.period.response <- y0[post.period[1] : post.period[2]]
y<-y0
#y[post.period[1] : post.period[2]] <- NA

# Scaling covariates
df1 <- All_values[index1,-c(1,2,3,17,18)]
for (j in 1:dim(df2)[2]) {
  df1[,j] <- df1[,j] - min(df1[,j])
  df1[,j] <- df1[,j]/max(df1[,j])
}

#
y  <-y0[1:45]
df2<-df1[1:45,]

#
bsts4 <- bsts(y ~ .,
              state.specification = ss,
              niter = 1500,
              data = df2,
              expected.model.size = 4) 
#
pred1 <- predict(bsts3, horizon = 49, newdata = df1[46:94,])
aa <- bsts.prediction.errors(bsts1, burn = 500)

x11()
plot(pred1, plot.original = 50, ylim = range(pred1) )
lines(y0,col=1)
hh <- c((y0[1:45]-colMeans(aa$in.sample)),pred1$mean[1:2])
lines(hh,col="blue")



}

































#########################################


n <- 100
residual.sd <- .001
# Actual values of the AR coefficients
true.phi <- c(-.7, .3, .15)
ar <- arima.sim(model = list(ar = true.phi),
                n = n,
                sd = 3)
## Layer some noise on top of the AR process.
y1 <- ar + rnorm(n, 0, residual.sd)
ssaa <- AddAr(list(), lags = 2, sigma.prior = SdPrior(3.0, 1.0))
# Fit the model with knowledge with residual.sd essentially fixed at the
# true value.

y2 <- y1[1:50]
model2 <- bsts(y2, state.specification=ssaa, niter = 5000, prior = SdPrior(residual.sd, 50000))
pred1 <- predict(model2, horizon = 50)
aa <- bsts.prediction.errors(model2, burn = 500)


###################
model3 <- model2
x11()
CompareBstsModels(list("Model 2" = model2,
                       "model 3" = model3),
                  colors = c("black", "red"))
# equivalent to:
x11()
matplot(cumsum(abs(colMeans(aa$in.sample))),type="l",col=1)
# equivalent to:
points(1:50,cumsum(abs(colMeans(model2$one.step.prediction.errors))),col="red")


#######  Yt  and Yt^hat 
x11()
plot(pred1, plot.original = 50, ylim = range(pred1)/2 )
lines(y1,col=1)
hh <- c( (y2-colMeans(aa$in.sample))[1:50] ,pred1$mean[1:2])
lines(y2-colMeans(aa$in.sample),col="blue")
lines(hh,col="blue")

pred1 <- predict(model2, horizon = 50)
aa <- bsts.prediction.errors(model2, burn = 500)



post.period <- c(51, 100)
post.period.response <- y1[post.period[1] : post.period[2]]
y2 <- y1
y2[post.period[1] : post.period[2]] <- NA
model2 <- bsts(y2, state.specification=ssaa, niter = 5000, prior = SdPrior(residual.sd, 50000))


impact <- CausalImpact(bsts.model = model2,
                       post.period.response = post.period.response)
x11()
plot(impact)
summary(impact)


arima()


