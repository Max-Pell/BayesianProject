library(bmstdr)
library(ggplot2)

# Load data
all.values <- read.csv("./SahuModels/Data/Sahu_models_dataset.csv", header = TRUE, sep = ",") # nolint
coord <- all.values[, c("lat", "lon")]

# Create a new dataset with all the columns except lat, lon, type, area
data <- all.values[, !(names(all.values) %in% c("lat", "lon"))]

f <- NOx ~ max_wind10 + avg_wind10 + max_wind100 + avg_wind100 + max_humidity + avg_humidity + max_precipitation + avg_precipitation + max_temperature + avg_temperature + min_temperature + k_precipitation + k_wind10

# Model 9
M9 <- Bsptime(package ="spTimer", model = "GPP", g_size=5, formula=f, data = data, coordtype ="lonlat", coords = coord, scale.transform = "SQRT") # nolint

