library(bmstdr)
library(ggplot2)

# Load data
all.values <- read.csv("./SahuModels/Data/All_values_with_coord.csv", header = TRUE, sep = ",")
coord <- all.values[,c("lat", "lon")]

# Create a new dataset with all the columns except lat, lon, type, area
data <- all.values[ , !(names(all.values) %in% c("lat", "lon", "type", "area"))]

f2 <- y8hrmax ~ xmaxtemp+xwdsp+xrh


# Model 9
M9 <- Bsptime(package="spTimer", model="GPP", g_size=5, formula=f2
,data=data, coordtype="lonlat", coords=coord,
scale.transform = "SQRT")

