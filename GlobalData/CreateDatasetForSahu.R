# Load the data
all.values <- read.csv("./GlobalData/All_values.csv", header = TRUE, sep = ";")
location.sensor.coord <- read.csv("./GlobalData/location_sensor_coord.csv", header = TRUE, sep = ",")

# Create a dictionary with the name of the mounth in italian and the number of the mounth
italian.mounth <- c("gennaio" = "01", "febbraio" = "02", "marzo" = "03", "aprile" = "04",
 "maggio" = "05", "giugno" = "06", "luglio" = "07", "agosto" = "08",
  "settembre" = "09", "ottobre" = "10", "novembre" = "11", "dicembre" = "12")

# Rename the column "sensor_id" to "ID" in location.sensor.coord
colnames(location.sensor.coord)[which(colnames(location.sensor.coord) == "sensor_id")] <- "ID"

# Rename the column "Id_sensor" to "ID" in all.values
colnames(all.values)[which(colnames(all.values) == "Id_sensor")] <- "ID"

# Merge the data frames based on the "ID" column
all.values <- merge(location.sensor.coord, all.values, by = "ID")

#Keep only the raws with non missing values in the collumns from location.sensor.coord
all.values <- all.values[complete.cases(all.values[,c("ID", "lat", "lon")]),]

# Remove the "ID" column
all.values$ID <- NULL

# Rename the column location_id to s.index
colnames(all.values)[which(colnames(all.values) == "location_id")] <- "s.index"


# The data frame
# should be in long format having one row for each location and time combination.
# The data frame must be ordered by time within each site, and should optionally
# have a column, named s.index, providing the site indices. Thus the data, with
# n sites and T times within each site, should be organized in the order: (s1, t1),
# (s1, t2), ... (s1, T), ... (sn, t1), ... (sn, T). The data frame should also contain
# two columns giving the coordinates of the locations for spatio temporal model
# fitting.

# Extract month and year from the original Time column
month_year <- strsplit(all.values$Time, " ")

# separte the month and the year
mounth <- sapply(month_year, "[", 1)
year <- sapply(month_year, "[", 2)


# Replace the italian mounth with the number of the mounth
mounth <- italian.mounth[match(mounth, names(italian.mounth))]

# Create a new column with the month and years mm dd
all.values$Time <- paste(year, mounth, sep = " ")

# Order the data frame by s.index and time
all.values <- all.values[order(all.values$s.index, all.values$Time),]

# Swap year month to mount year
year_mounth <- strsplit(all.values$Time, " ")
mounth <- sapply(year_mounth, "[", 1)
year <- sapply(year_mounth, "[", 2)
all.values$Time <- paste(mounth, year, sep = " ")

# Remove the column type and area
all.values$type <- NULL
all.values$area <- NULL

# Remove the columns X.x and X.y
all.values$X.x <- NULL
all.values$X.y <- NULL


# Save the new dataset
write.csv(all.values, "./SahuModels/Data/Sahu_models_dataset.csv", row.names = FALSE)
