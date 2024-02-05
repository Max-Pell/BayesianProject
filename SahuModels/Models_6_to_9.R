library(bmstdr)
library(ggplot2)

# Load data
data <- read.csv("./SahuModels/Data/Sahu_models_dataset.csv", header = TRUE, sep = ",") # nolint
coord <- data[, c("lat", "lon")]

# Create a new dataset with all the columns except lat, lon, type, area

f <- NOx ~ max_wind10 + avg_wind10 + max_wind100 +
  avg_wind100 + max_humidity + avg_humidity +
  max_precipitation + avg_precipitation + max_temperature +
  avg_temperature + min_temperature + k_precipitation +
  k_wind10

# Model 6

# Model 7
M7 <- Bsptime(package="sptDyn", model="GP", formula=f, data=data, coordtype="lonlat", coords=2:3, scale.transform = "LOG", mchoice=T) # nolint

# Model 8
M8 <- Bsptime(package="spBayes", formula=f, data=data,prior.sigma2=c(2, 25), prior.tau2 =c(2, 25), prior.sigma.eta =c(2, 0.001), coordtype="lonlat", coords=2:3, scale.transform = "LOG", mchoice=T) # nolint

# Model 9
M9 <- Bsptime(package ="spTimer", model = "GPP", g_size=5, formula=f, data = data, coordtype ="lonlat", coords = 2:3, scale.transform = "LOG") # nolint

summary(M7)
summary(M8)
summary(M9)
