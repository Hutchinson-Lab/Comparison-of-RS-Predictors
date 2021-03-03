#################################################################################################
# Recreate LandTrendr summaries, given raster files of the raw bands, Tasssled Cap components, 
# and elevation data
#
# Laurel Hopkins
# 03/03/2020
#################################################################################################


library(sf)
library(rgdal)  # reads entire raster into memnory  
library(raster) # does not require rasters to be held in-memory which is beneficial for 
# analyzing and predicting to larger files

Working.directory <-"C:\\Users\\Laurel\\Documents\\Oregon State\\Research\\LandTrendr" 
setwd(Working.directory)

# Partially based on this post: https://geocompr.robinlovelace.net/intro.html
# potentially helpful: https://mgimond.github.io/Spatial/introGIS.html
#                      https://www.earthdatascience.org/courses/earth-analytics/lidar-raster-data-r/crop-raster-data-in-r/

#########################################################
# 1. Read in GEE exports and merge into single tif -- skip  
#    this if working with complete tif files
#    (Files on box do not need this step)
#########################################################
# read in files as rasters
files <- list.files(path="fittedImages", pattern="*.tif$", full.names=TRUE, recursive=FALSE)
ras1 <- stack(files[1])  # use stack() to read in all bands, raster() only reads in single band; brick() may be faster, but less flixible
ras2 <- stack(files[2])
ras3 <- stack(files[3])
ras4 <- stack(files[4])

# plot with spplot to view all layers
spplot(ras1)
plot(ras2[[1]])

# merge the rasters to form a single raster
x <- list(ras1, ras2, ras3, ras4)
x$filename <- 'OR_TasseledCap_2011.tif'
x$overwrite <- TRUE
OR_tif <- do.call(merge, x)
#names(OR_tif) <-c("B1", "B2", "B3", "B4", "B5", "B7")
names(OR_tif) <-c("TCB", "TCG", "TCW", "TCA")

# save as a geotiff file, may not be necessary, it seems like merge saves the tif
#rf <- writeRaster(OR_2020, filename="OR_2020_fittedLandTrendr.tif", format="GTiff", overwrite=TRUE)

# plot tif
# plot(OR_2020)
#plotRGB(OR_tif, r="B7", g="B4", b ="B3", stretch="lin")
plotRGB(OR_tif, r="TCB", g="TCG", b="TCW", stretch="lin")

#########################################################
# 2. Read in raster file OR__tif
#########################################################
# to read in raw bands
year <- "2011"
OR_tif <- stack(paste0("OR_", year, "_fittedImage.tif"))
names(OR_tif) <-c("B1", "B2", "B3", "B4", "B5", "B7")  # rename the layers, if necessary
plotRGB(OR_tif, r="B7", g="B4", b ="B3", stretch="lin")

# to read in elevation, slope, aspect
OR_elev <- stack("Elevation_Slope_Aspect.tif")
OR_elev  # print summary of raster
plot(OR_elev)  # plot elevation, slope, and aspect, plot(OR_tif$elevation) to only plot elevation
crs(OR_elev)


# to read in Tasseled Cap components
year <- "2011"
season <- "summer"
OR_tc <- stack(paste0("OR_TasseledCap_", season, "_", year, ".tif"))
names(OR_tc) <-c("TCB", "TCG", "TCW", "TCA")

OR_tc  # print summary of raster
plotRGB(OR_tc, r="TCB", g="TCG", b ="TCW", stretch="lin")
crs(OR_tc)



#########################################################
# 3. Create lat,long spatial points df
#########################################################
library(dplyr)
library(tidyr)

# create example dataframe
lat <- c(42.00128, 42.00175, 42.00220, 42.00246, 42.00321)
long <- c(-118.8341, -118.8413, -118.8524, -118.6301, -118.8584)
year <- c(2017, 2017, 2017, 2017, 2017)

locations <- data.frame(lat, long, year)

# create a SpatialPointsDataFrame w/ longlat projection
prj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # keep this for lat/long data

select <- dplyr::select #this tells R to use the select() function from the dplyr package
coords <- locations %>% select(long, lat)

locations_pt <- SpatialPointsDataFrame(coords = coords, data = locations, proj4string = prj)

# reproject longlat points to match projection of OR_tif
locations_pt <- spTransform(locations_pt, crs(OR_elev))
locations_pt@proj4string  # print projection

#plot(OR_elev$elevation)  # can plot points on map to make sure they overlap
#plot(locations_pt, add=TRUE)

# Check that projections match
identical(crs(locations_pt), crs(OR_elev)) # should print true


#########################################################
# 4. With raster &  locations as as spatial points df,
#    extract buffered summaries
#
#    Clipping: https://geocompr.robinlovelace.net/geometric-operations.html
#########################################################
# buffer radius in meters
buffer.radius <- 2400 

summary_df <- locations
summary_df$mean <- NA
summary_df$std_dev <- NA

for (i in 1:nrow(locations_pt)) {  # there may be a faster way to do this with lapply
  # create buffered region
  point = st_sfc(st_point(c(locations_pt@coords[i,"long"], locations_pt@coords[i,"lat"])), crs=st_crs(OR_elev))
  buffer = st_buffer(point, dist = buffer.radius) # create buffered region around point
  buffer_sp <- as_Spatial(buffer) # convert buffered region to SpatialPolygonsDataFrame
  
  # extract values from raster in the buffered region
  buffered_values <- extract(OR_elev$elevation, buffer_sp)
  
  # Use list apply to calculate the mean for the buffered region
  summary_df[i, "mean"] <- unlist(lapply(buffered_values, FUN=mean))
  summary_df[i, "std_dev"] <- unlist(lapply(buffered_values, FUN=sd))
  
}
