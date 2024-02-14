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

NOx_sensors <- read.csv("./Data/NOx_sensors.csv")
NOx_sensors <- NOx_sensors[,-1]
All_values <- read.csv("./Data/All_values_Clean.csv")
All_values <- All_values[,-1]

# For Futher codice on model selection part cfr  Causal_Impact_extra.R

### NB open/close the {} for an easier reading (ex the one at line 63)

################################################################################
######### Model selection
################################################################################
# here we a selection of the models tested, in order to see the comparison methodology
##### Data preparation
{
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
df2 <- df2[,-c(3,4,9,11)] # removing some linearly correlated values
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



}



################################################################################
######### Choosed Model
################################################################################
# here we report the choosed model
{
  
  pre.period <- c(1, 45)
  post.period <- c(46, 94) #94
  
  # Selecting one sensor
  #effect_T[1,]
  index1 <- which(All_values$Id_sensor == 6204)
  
  # Formatting the target variable
  y0  <- All_values[index1,3]
  #y0 <- scale(y0)
  post.period.response <- y0[post.period[1] : post.period[2]]
  y<-y0
  
  
  y[post.period[1] : post.period[2]] <- NA
  
  df3 <- All_values[index1,c(4:16,19:24)]
  df3 <- df3[,-c(3,4,9,11)]
  #df3$month <- sd((1:94))
  ss2  <- AddStaticIntercept(list(),y,
                             initial.state.prior = NormalPrior(0, 2))
  
  
  x <- matrix(1,94,16)
  x[,2:15]<-as.matrix(df3[,1:14])
  x[,16]<-as.matrix(df3[,15])
  all(x[,2:16]==df3)
  prior1 <- SpikeSlabPrior(x, y=NULL,
                           mean.y = 0,
                           sdy = 1,                  # expect 3 nonzero predictors
                           prior.df = 1,           # weaker prior than the default
                           prior.information.weight = 0.01,
                           prior.inclusion.probabilities = rep(0.25,16),
                           diagonal.shrinkage = 0.05 ,
                           expected.r2 = 0.5
                           #optional.coefficient.estimate = rep(0, 16) # shrink to zero
  )
  
  bsts2e <- bsts(y ~ .,
                 state.specification = ss2,
                 niter = MCMC_iter,
                 data = df3,
                 model.options = BstsOptions(save.full.state = TRUE)
                 ,prior = prior1
                 #,expected.model.size = 4
  ) 
  
  x11()
  plot(bsts2e, "coef")

}



################################################################################
######### Plot for different Change points
################################################################################
# here we compute che impact for each change point, in order to choose the appropriate
# one to continue the analysis
{
  
  
  burn_in =1000
  MCMC_iter = 6000
  iter_to_keep = MCMC_iter-burn_in-1
  choosen_sensor = 6204
  # 6220 covid very significative
  # 6232 normative very significative
  
  x11()
  par(mfcol=c(3,3))
  
  ################################################################################
  #############  prediction 1:45
  ################################################################################
  ##### Data preparation
  pre.period <- c(1, 45)
  post.period <- c(46, 94) #94
  
  # Selecting one sensor
  #effect_T[1,]
  index1 <- which(All_values$Id_sensor == choosen_sensor)
  
  # Formatting the target variable
  y0  <- All_values[index1,3]
  #y0 <- scale(y0)
  post.period.response <- y0[post.period[1] : post.period[2]]
  y<-y0
  
  
  y[post.period[1] : post.period[2]] <- NA
  
  df3 <- All_values[index1,c(4:16,19:24)]
  df3 <- df3[,-c(3,4,9,11)]
  #df3$month <- sd((1:94))
  ss2  <- AddStaticIntercept(list(),y,
                             initial.state.prior = NormalPrior(0, 2))
  
  
  x <- matrix(1,94,16)
  x[,2:15]<-as.matrix(df3[,1:14])
  x[,16]<-as.matrix(df3[,15])
  all(x[,2:16]==df3)
  prior1 <- SpikeSlabPrior(x, y=NULL,
                           mean.y = 0,
                           sdy = 1,                  # expect 3 nonzero predictors
                           prior.df = 1,           # weaker prior than the default
                           prior.information.weight = 0.01,
                           prior.inclusion.probabilities = rep(0.25,16),
                           diagonal.shrinkage = 0.05 ,
                           expected.r2 = 0.5
                           #optional.coefficient.estimate = rep(0, 16) # shrink to zero
  )
  
  bsts2e <- bsts(y ~ .,
                 state.specification = ss2,
                 niter = MCMC_iter,
                 data = df3,
                 model.options = BstsOptions(save.full.state = TRUE)
                 ,prior = prior1
                 #,expected.model.size = 4
  ) 
  
  
  
  
  
  impact <- CausalImpact(bsts.model = bsts2e,
                         post.period.response = post.period.response)
  
  #impact <- CausalImpact(data1[index1,], pre.period, post.period, 
  #                       model.args = list(niter = MCMC_iter , standardize.data=F))
  # plot(impact$model$bsts.model, "comp")
  
  #x11()
  #plot(impact)
  
  summary(impact)
  y0  <- All_values[index1,3]
  post_y <-impact$model$posterior.samples
  # removing burn-in
  post_y <- post_y[(dim(post_y)[1]-iter_to_keep):(dim(post_y)[1]),]
  
  # Credibility intervals
  CI <- NULL
  for(i in 1:94){
    CI <- rbind(CI, c(quantile(post_y[,i],probs = c(0.025,0.975)),mean(post_y[,i])))
  }
  
  # plot
  plot(range(CI) ~ c(1,94), type = 'n', xlab="month", ylab="values", main ="1°change point")
  grid()
  # add fill
  newx=1:94
  polygon(c(rev(newx), newx), c(rev(CI[ ,1]), CI[ ,2]), col = 'lightblue', border = NA)
  #lines(newx, CI[ ,1], lty = 'dashed', col = 'blue')
  #lines(newx, CI[ ,2], lty = 'dashed', col = 'blue')
  lines(newx, CI[ ,3],  lty = 'dashed', col = 'blue',lwd = 2)
  lines(newx,y0,lwd = 2)
  abline(v=45, lwd=3, col="red")
  
  
  
  ########### pointwise error
  error <- post_y  #- 100*(1:94)
  for(i in 1:length(post_y[,1])){
    error[i,] <- y0- post_y[i,]
  }
  
  errCI <- NULL
  for(i in 1:94){
    errCI <- rbind(errCI,c(quantile(error[,i],probs = c(0.025,0.975)),mean(error[,i])))
  }
  # plot
  plot(range(errCI) ~ c(1,94), type = 'n', xlab="month", ylab="pointwise error")
  # add fill
  newx=1:94
  polygon(c(rev(newx), newx), c(rev(errCI[ ,1]), errCI[ ,2]), col = 'lightblue', border = NA)
  grid()
  #lines(newx, CI[ ,1], lty = 'dashed', col = 'blue')
  #lines(newx, CI[ ,2], lty = 'dashed', col = 'blue')
  abline(h=0, col="grey50", lwd = 2)
  lines(newx, errCI[ ,3],  lty = 'dashed', col = 'blue',lwd = 2)
  abline(v=45, col="red", lwd = 3)
  
  
  
  ###########cumulative error
  cumerr <- error[,46:94]
  for(i in 1:length(post_y[,1])){
    cumerr [i,] <- cumsum(error[i,46:94])
  }
  
  cumerrCI <- c(0,0,0)
  for(i in 1:length(cumerr[1,])){
    cumerrCI <- rbind(cumerrCI,c(quantile(cumerr[,i],probs = c(0.025,0.975)),mean(cumerr[,i])))
    
  }
  # plot
  plot(range(cumerrCI) ~ c(1,94), type = 'n', xlab="month", ylab="cumulative error")
  # add fill
  newx=45:94
  polygon(c(rev(newx), newx), c(rev(cumerrCI[ ,1]), cumerrCI[ ,2]), col = 'lightblue', border = NA)
  grid()
  #lines(newx, CI[ ,1], lty = 'dashed', col = 'blue')
  #lines(newx, CI[ ,2], lty = 'dashed', col = 'blue')
  abline(h=0, col="grey50", lwd = 2)
  lines(newx, cumerrCI[ ,3],  lty = 'dashed', col = 'blue',lwd = 2)
  abline(v=45, col="red", lwd = 3)
  
  
  
  
  ################################################################################
  #############  prediction 1:61
  ################################################################################
  ##### Data preparation
  pre.period <- c(1, 61)
  post.period <- c(62, 94) #94
  
  # Selecting one sensor
  #effect_T[1,]
  index1 <- which(All_values$Id_sensor == choosen_sensor)
  
  # Formatting the target variable
  y0  <- All_values[index1,3]
  post.period.response <- y0[post.period[1] : post.period[2]]
  y<-y0
  y[post.period[1] : post.period[2]] <- NA
  
  
  df3 <- All_values[index1,c(4:16,19:24)]
  df3 <- df3[,-c(3,4,9,11)]
  # 
  ss2  <- AddStaticIntercept(list(),y,
                             initial.state.prior = NormalPrior(0, 2))
  
  
  x <- matrix(1,94,16)
  x[,2:15]<-as.matrix(df3[,1:14])
  x[,16]<-as.matrix(df3[,15])
  all(x[,2:16]==df3)
  prior1 <- SpikeSlabPrior(x, y=NULL,
                           mean.y = 0,
                           sdy = 1,                  # expect 3 nonzero predictors
                           prior.df = 1,           # weaker prior than the default
                           prior.information.weight = 0.01,
                           prior.inclusion.probabilities = rep(0.25,16),
                           diagonal.shrinkage = 0.05 ,
                           expected.r2 = 0.5
                           
                           #optional.coefficient.estimate = rep(0, 16) # shrink to zero
  )
  
  bsts2e <- bsts(y ~ .,
                 state.specification = ss2,
                 niter = MCMC_iter,
                 data = df3,
                 model.options = BstsOptions(save.full.state = TRUE)
                 ,prior = prior1
                 #,expected.model.size = 4
  ) 
  
  
  
  
  
  
  impact <- CausalImpact(bsts.model = bsts2e,
                         post.period.response = post.period.response)
  
  #CausalImpact(data2, pre.period, post.period)
  
  # x11()
  # plot(impact)
  
  summary(impact)
  y0  <- All_values[index1,3]
  post_y <-impact$model$posterior.samples
  # removing burn-in
  post_y <- post_y[(dim(post_y)[1]-iter_to_keep):(dim(post_y)[1]),]
  
  # Credibility intervals
  CI <- NULL
  for(i in 1:94){
    CI <- rbind(CI, c(quantile(post_y[,i],probs = c(0.025,0.975)),mean(post_y[,i])))
  }
  #impact$model$posterior.samples[,1]
  # plot
  plot(range(CI) ~ c(1,94), type = 'n', xlab="month", ylab="values", main ="2°change point")
  grid()
  # add fill
  newx=1:94
  polygon(c(rev(newx), newx), c(rev(CI[ ,1]), CI[ ,2]), col = 'lightblue', border = NA)
  #lines(newx, CI[ ,1], lty = 'dashed', col = 'blue')
  #lines(newx, CI[ ,2], lty = 'dashed', col = 'blue')
  lines(newx, CI[ ,3],  lty = 'dashed', col = 'blue',lwd = 2)
  lines(newx,y0,lwd = 2)
  abline(v=61, lwd=3, col="red")
  
  
  
  ########### pointwise error
  error <- post_y  #- 100*(1:94)
  for(i in 1:length(post_y[,1])){
    error[i,] <- y0- post_y[i,]
  }
  
  errCI <- NULL
  for(i in 1:94){
    errCI <- rbind(errCI,c(quantile(error[,i],probs = c(0.025,0.975)),mean(error[,i])))
  }
  
  # plot
  plot(range(errCI) ~ c(1,94), type = 'n', xlab="month", ylab="pointwise error")
  # add fill
  newx=1:94
  polygon(c(rev(newx), newx), c(rev(errCI[ ,1]), errCI[ ,2]), col = 'lightblue', border = NA)
  grid()
  #lines(newx, CI[ ,1], lty = 'dashed', col = 'blue')
  #lines(newx, CI[ ,2], lty = 'dashed', col = 'blue')
  abline(h=0, col="grey50", lwd = 2)
  lines(newx, errCI[ ,3],  lty = 'dashed', col = 'blue',lwd = 2)
  abline(v=61, col="red", lwd = 3)
  
  
  
  ###########cumulative error
  cumerr <- error[,62:94]
  for(i in 1:length(post_y[,1])){
    cumerr [i,] <- cumsum(error[i,62:94])
  }
  
  cumerrCI <- c(0,0,0)
  for(i in 1:length(cumerr[1,])){
    cumerrCI <- rbind(cumerrCI,c(quantile(cumerr[,i],probs = c(0.025,0.975)),mean(cumerr[,i])))
    
  }
  # plot
  plot(range(cumerrCI) ~ c(1,94), type = 'n', xlab="month", ylab="cumulative error")
  # add fill
  newx=61:94
  polygon(c(rev(newx), newx), c(rev(cumerrCI[ ,1]), cumerrCI[ ,2]), col = 'lightblue', border = NA)
  grid()
  #lines(newx, CI[ ,1], lty = 'dashed', col = 'blue')
  #lines(newx, CI[ ,2], lty = 'dashed', col = 'blue')
  abline(h=0, col="grey50", lwd = 2)
  lines(newx, cumerrCI[ ,3],  lty = 'dashed', col = 'blue',lwd = 2)
  abline(v=61, col="red", lwd = 3)
  
  
  
  
  
  ################################################################################
  #############  prediction 1:45  vs 62:94
  ################################################################################
  ##### Data preparation
  pre.period <- c(1, 45)
  post.period <- c(46, 94) #94
  
  # Selecting one sensor
  #effect_T[1,]
  index1 <- which(All_values$Id_sensor == choosen_sensor)
  
  # Formatting the target variable
  y0  <- All_values[index1,3]
  post.period.response <- y0[post.period[1] : post.period[2]]
  y<-y0
  y[post.period[1] : post.period[2]] <- NA
  
  
  df3 <- All_values[index1,c(4:16,19:24)]
  df3 <- df3[,-c(3,4,9,11)]
  # 
  ss2  <- AddStaticIntercept(list(),y,
                             initial.state.prior = NormalPrior(0, 2))
  
  
  x <- matrix(1,94,16)
  x[,2:15]<-as.matrix(df3[,1:14])
  x[,16]<-as.matrix(df3[,15])
  all(x[,2:16]==df3)
  prior1 <- SpikeSlabPrior(x, y=NULL,
                           mean.y = 0,
                           sdy = 1,                  # expect 3 nonzero predictors
                           prior.df = 1,           # weaker prior than the default
                           prior.information.weight = 0.01,
                           prior.inclusion.probabilities = rep(0.25,16),
                           diagonal.shrinkage = 0.05 ,
                           expected.r2 = 0.5
                           
                           #optional.coefficient.estimate = rep(0, 16) # shrink to zero
  )
  
  bsts2e <- bsts(y ~ .,
                 state.specification = ss2,
                 niter = MCMC_iter,
                 data = df3,
                 model.options = BstsOptions(save.full.state = TRUE)
                 ,prior = prior1
                 #,expected.model.size = 4
  ) 
  
  
  
  
  
  impact <- CausalImpact(bsts.model = bsts2e,
                         post.period.response = post.period.response)
  
  #CausalImpact(data2, pre.period, post.period)
  
  # x11()
  # plot(impact)
  
  summary(impact)
  y0  <- All_values[index1,3]
  post_y <-impact$model$posterior.samples
  # removing burn-in
  post_y <- post_y[(dim(post_y)[1]-iter_to_keep):(dim(post_y)[1]),]
  
  # Credibility intervals
  CI <- NULL
  for(i in 1:94){
    CI <- rbind(CI, c(quantile(post_y[,i],probs = c(0.025,0.975)),mean(post_y[,i])))
  }
  # plot
  plot(range(CI) ~ c(1,94), type = 'n', xlab="month", ylab="values", main ="mixed")
  grid()
  # add fill
  newx=1:94
  polygon(c(rev(newx), newx), c(rev(CI[ ,1]), CI[ ,2]), col = 'lightblue', border = NA)
  #lines(newx, CI[ ,1], lty = 'dashed', col = 'blue')
  #lines(newx, CI[ ,2], lty = 'dashed', col = 'blue')
  lines(newx, CI[ ,3],  lty = 'dashed', col = 'blue',lwd = 2)
  lines(newx,y0,lwd = 2)
  abline(v=45, lwd=3, col="red")
  abline(v=61, lwd=3, col="red")
  
  
  ########### pointwise error
  error <- post_y  #- 100*(1:94)
  for(i in 1:length(post_y[,1])){
    error[i,] <- y0- post_y[i,]
  }
  
  errCI <- NULL
  for(i in 1:94){
    errCI <- rbind(errCI,c(quantile(error[,i],probs = c(0.025,0.975)),mean(error[,i])))
  }
  # plot
  plot(range(errCI) ~ c(1,94), type = 'n', xlab="month", ylab="pointwise error")
  # add fill
  newx=1:94
  polygon(c(rev(newx), newx), c(rev(errCI[ ,1]), errCI[ ,2]), col = 'lightblue', border = NA)
  grid()
  #lines(newx, CI[ ,1], lty = 'dashed', col = 'blue')
  #lines(newx, CI[ ,2], lty = 'dashed', col = 'blue')
  abline(h=0, col="grey50", lwd = 2)
  lines(newx, errCI[ ,3],  lty = 'dashed', col = 'blue',lwd = 2)
  abline(v=45, lwd=3, col="red")
  abline(v=61, col="red", lwd = 3)
  
  
  
  ###########cumulative error
  cumerr <- error[,62:94]
  for(i in 1:length(post_y[,1])){
    cumerr [i,] <- cumsum(error[i,62:94])
  }
  
  cumerrCI <- c(0,0,0)
  for(i in 1:length(cumerr[1,])){
    cumerrCI <- rbind(cumerrCI,c(quantile(cumerr[,i],probs = c(0.025,0.975)),mean(cumerr[,i])))
    
  }
  # plot
  plot(range(cumerrCI) ~ c(1,94), type = 'n', xlab="month", ylab="cumulative error")
  # add fill
  newx=61:94
  polygon(c(rev(newx), newx), c(rev(cumerrCI[ ,1]), cumerrCI[ ,2]), col = 'lightblue', border = NA)
  grid()
  #lines(newx, CI[ ,1], lty = 'dashed', col = 'blue')
  #lines(newx, CI[ ,2], lty = 'dashed', col = 'blue')
  abline(h=0, col="grey50", lwd = 2)
  lines(newx, cumerrCI[ ,3],  lty = 'dashed', col = 'blue',lwd = 2)
  abline(v=45, col="red", lwd = 3)
  abline(v=61, col="red", lwd = 3)
  
  
  
}



################################################################################
######### Geographical PLOTS
################################################################################
# here we compute che impact for each sensor using the change point 61 (january 2021),
# and the we create the plot to visualize them together
{
  
  
  burn_in =1000
  MCMC_iter = 6000
  iter_to_keep = MCMC_iter-burn_in-1
  choosen_sensor = 6204
  #change point
  pre.period <- c(1, 61)
  post.period <- c(62, 94) #94
  
  Cum_real <- 0 
  Cum_no_ef <-0
  SQ_err  <- 0
  ABs_ERR <- 0
  effect_T   <- NULL
  effect_I   <- NULL
  effect_B   <- NULL
  effect_boh <- NULL
  Covid <- 0
  
  sensor_id <-unique(All_values$Id_sensor) 
  for(hh in sensor_id ){
      ##### collecting data
      index1 <- which(All_values$Id_sensor == hh)
       #index1 <- which(All_values$Id_sensor == 6204)
      y0  <- All_values[index1,3]
      post.period.response <- y0[post.period[1] : post.period[2]]
      y<-y0
      y[post.period[1] : post.period[2]] <- NA
      df3 <- All_values[index1,c(4:16,19:24)]
      df3 <- df3[,-c(3,4,9,11)]
      
      # ####### old model
      # ss2  <- AddStaticIntercept(list(),y,
      #                          initial.state.prior = NormalPrior(0, 2))
      # bsts2e <- bsts(y ~ .,
      #                state.specification = ss2,
      #                niter = MCMC_iter,
      #                data = df3,
      #                expected.model.size = 4)
      model <- bsts(y, state.specification=ss2, niter = 500, prior = SdPrior(11, 100000))
      ####
      ss2  <- AddStaticIntercept(list(),y,
                                 initial.state.prior = NormalPrior(0, 2))
      
      
      x <- matrix(1,94,16)
      x[,2:15]<-as.matrix(df3[,1:14])
      x[,16]<-as.matrix(df3[,15])
      all(x[,2:16]==df3)
      prior1 <- SpikeSlabPrior(x, y=NULL,
                               mean.y = 0,
                               sdy = 1,                  # expect 3 nonzero predictors
                               prior.df = 1,           # weaker prior than the default
                               prior.information.weight = 0.01,
                               prior.inclusion.probabilities = rep(0.25,16),
                               diagonal.shrinkage = 0.05 ,
                               expected.r2 = 0.50
                               
                               #optional.coefficient.estimate = rep(0, 16) # shrink to zero
      )
      
      bsts2e <- bsts(y ~ .,
                     state.specification = ss2,
                     niter = MCMC_iter,
                     data = df3,
                     model.options = BstsOptions(save.full.state = TRUE)
                     ,prior = prior1
                     #,expected.model.size = 4
      ) 
      
      
      
      ####
      
      
      
      impact <- CausalImpact(bsts.model = bsts2e,
                             post.period.response = post.period.response)
       #x11()
       #plot(impact)
      # MSE/MAE
      ########### pointwise error
      
      yb  <- All_values[index1,3]
      post_y <-impact$model$posterior.samples
      # removing burn-in
      post_y <- post_y[(dim(post_y)[1]-iter_to_keep):(dim(post_y)[1]),]
      
      error <- post_y  #- 100*(1:94)
      for(i in 1:length(post_y[,1])){
        error[i,] <- yb- post_y[i,]
      }
      errCI <- NULL
      for(i in 1:94){
        errCI <- rbind(errCI,c(quantile(error[,i],probs = c(0.025,0.975)),mean(error[,i])))
      }
      
      if(errCI[51, 2 ] <0 )
        Covid=Covid+1
      
      SQ_err <- SQ_err+ sum((errCI[1:61,3])^2)
      ABs_ERR<- ABs_ERR+ sum(abs(errCI[1:61,3]))

      Cum_real <- Cum_real  + sum(yb[62:94])
      Cum_no_ef<- Cum_no_ef + sum(colMeans(post_y[,62:94]))
      
      ##### storing results
      if (All_values$type[index1[1]] == "T"){
        effect_T   <- rbind( effect_T,c(All_values$Id_sensor[index1[1]],
                                        impact$summary$AbsEffect.lower[2],
                                        impact$summary$AbsEffect[2],
                                        impact$summary$AbsEffect.upper[2]) ) 
      }
      if (All_values$type[index1[1]] == "I"){
        effect_I   <- rbind( effect_I,c(All_values$Id_sensor[index1[1]],
                                        impact$summary$AbsEffect.lower[2],
                                        impact$summary$AbsEffect[2],
                                        impact$summary$AbsEffect.upper[2]) ) 
      }
      if (All_values$type[index1[1]] == "B"){
        effect_B   <- rbind( effect_B,c(All_values$Id_sensor[index1[1]],
                                        impact$summary$AbsEffect.lower[2],
                                        impact$summary$AbsEffect[2],
                                        impact$summary$AbsEffect.upper[2]) ) 
      }
      # if (All_values$type[index1[1]] == "???"){
      #   effect_boh   <- rbind( effect_boh,c(All_values$Id_sensor[index1[1]],
      #                                       impact$summary$AbsEffect.lower[2],
      #                                       impact$summary$AbsEffect[2],
      #                                       impact$summary$AbsEffect.upper[2]) ) 
      # }
    print(paste(hh,"done"))
  }
  
  # cum effect
  Cum_Eff_red <- (Cum_no_ef-Cum_real)/Cum_no_ef # Cum_no_ef-Cum_real = 403.3771
  Cum_Eff_red  #0.04551248
  
  # cum error
  MSE  <- SQ_err/(61*54)
  MSE  # 0.0762
  MAE<- ABs_ERR/(61*54)
  MAE # 0.2492
  Covid
  # no effect : 14  / effect 40
  ###############################
  ###########  Range effect ########################################
  ###############################
  #range(effect_T[,-1])
  ###### PLOT
  a <-rinvgamma(1000,0.5,0.25)
  a <-a[which(a<100)] 
  hist(a)
  
  
  ylim1 <- range(c(range(effect_T[,-1]),range(effect_I[,-1]),range(effect_B[,-1])))   
  
  x11()
  par(mfrow = c(3,1))
  
  eee <- effect_T
  plot(c(1,dim(eee)[1]),ylim1, pch='',
       xlab='sensor', ylab=' Cumulative effect', main = "Traffic")
  for(i in 1:dim(eee)[1]) {
    lines (c(i,i), c(eee[i,2],eee[i,4]), col='grey55'); 
    points(i, eee[i,2], col="red", pch=16); 
    points(i,eee[i,3], pch=16, col='grey55'); 
    points(i, eee[i,4], col="green", pch=16); 
  }
  abline(h=0)
  
  
  eee <- effect_I
  plot(c(1,dim(eee)[1]),ylim1, pch='',
       xlab='sensor', ylab=' Cumulative effect', main = "Industrial")
  for(i in 1:dim(eee)[1]) {
    lines (c(i,i), c(eee[i,2],eee[i,4]), col='grey55'); 
    points(i, eee[i,2], col="red", pch=16); 
    points(i,eee[i,3], pch=16, col='grey55'); 
    points(i, eee[i,4], col="green", pch=16);
  }
  abline(h=0)
  
  
  eee <- effect_B
  
  plot(c(1,dim(eee)[1]),ylim1, pch='',
       xlab='sensor', ylab=' Cumulative effect', main = "Background")
  for(i in 1:dim(eee)[1]) {
    lines (c(i,i), c(eee[i,2],eee[i,4]), col='grey55'); 
    points(i, eee[i,2], col="red", pch=16); 
    points(i,eee[i,3], pch=16, col='grey55'); 
    points(i, eee[i,4], col="green", pch=16);
  }
  abline(h=0)
  
  # eee <- effect_boh
  # plot(c(1,dim(eee)[1]),ylim1, pch='',
  #      xlab='sensor', ylab=' Cumulative effect', main = "Boh")
  # for(i in 1:dim(eee)[1]) {
  # lines (c(i,i), c(eee[i,2],eee[i,3]), col='grey55'); 
  # points(i, eee[i,2], col="red", pch=16); 
  # points(i,eee[i,3], pch=16, col='grey55'); 
  # points(i, eee[i,4], col="green", pch=16);
  # }
  # abline(h=0)

  
  
  
  ###############################
  ###########  Lombardy map ########################################
  ###############################
  #geo_values <- matrix(0,dim(effect_B)[1],5)
  geo_values <- effect_T
  geo_values <- rbind(geo_values,effect_B)
  geo_values <- rbind(geo_values,effect_I)
  
  
  geo_values <- as.data.frame(geo_values)
  geo_values$lon <- 0
  geo_values$lat <- 0
  
  for (i in 1:dim(geo_values)[1]){
    geo_values$lon[i] <- NOx_sensors$lng[which(NOx_sensors$IdSensore ==geo_values[i,1])]
    geo_values$lat[i] <- NOx_sensors$lat[which(NOx_sensors$IdSensore ==geo_values[i,1])]
  }
  
  
  values <- geo_values[,3]
  #range(values)
  
  ccc <-  colorRamp(c("white", "black"),space ="Lab")(abs(values)/max(values))
  ccc[which(values>0),] <- colorRamp(c("white", "red"),space ="Lab")(values[which(values>0)]/max(values[which(values>0)]))
  ccc[which(values<0),] <- colorRamp(c("white", "green"),space ="Lab")((-values[which(values<0)])/(abs(min(values))))
  ccc <- rgb(ccc[,1], ccc[,2], ccc[,3], maxColorValue=255)
  
  
  zones <-  st_read("limits_IT_provinces.geojson")
  zones <- zones[which(zones$reg_name=="Lombardia"),]
  stations_sf <- st_as_sf(geo_values, coords = c("lon", "lat"), crs = 4326, agr = "constant")
  stations_sf <- st_transform(stations_sf, st_crs(zones))
  list1_with_zones <- st_join(stations_sf, zones)
  
  x11()
  ggplot() +
    geom_sf(data = zones, fill = NA, color = "black") +
    # (full circles)
    geom_sf(data = list1_with_zones, size = 5, pch = 16, color = geo_values$col, show.legend = FALSE) +
    # (void circle)
    geom_sf(data = list1_with_zones, size = 5, pch = 1) +
    theme_minimal() +
    ggtitle("Station impact")
  
  
  
  
}


#No2_sensor <- No2_sensor[-which(No2_sensor$Type==""),]


