library(rgdal)
library(raster)
library(gstat)

# download the county file
tf <- tempfile() ; td <- tempdir()
download.file( "ftp://ftp2.census.gov/geo/tiger/TIGER2011/COUNTY/tl_2011_us_county.zip" , tf , mode = 'wb' )
z <- unzip( tf , exdir = td )


# isolate the file that ends with ".shp"
shapefile <- z[ grep( 'shp$' , z ) ]

# read the shapefile into working memory
county.map <- readOGR( shapefile, layer="tl_2011_us_county" )


ct.map <- subset( county.map , STATEFP %in% "09" )
# ct.map <- getData("GADM", country="United States", level=2) # alternate source for county map (note: counties not in same order, so data below would not line up)
# ct.map <- ct.map[ct.map$NAME_1=="Connecticut",]

# # extract centroid for each county
# ct.centroids <- data.frame(coordinates(ct.map), ct.map$GEOID)
# names(ct.centroids) <- c("lon","lat","GEOID")
# 
# # # # example data with population weights
# ctdata <-
#   data.frame(
#     ct.centroids ,
#     county.poverty.rate = c( 8 , 5.8 , 11.4 , 6.4 , 7.2 , 10.9 , 10.7 , 6.1 ) ,
#     county.pop = c( 916829 , 189927 , 118428 , 152691 , 274055 , 862477 , 894014 , 165676 )
#   )

## alternate approach

## add survey data to SpatialPolygonDataFrame
ct.map$county.poverty.rate <- c( 8 , 5.8 , 11.4 , 6.4 , 7.2 , 10.9 , 10.7 , 6.1 )
ct.map$pop <- c( 916829 , 189927 , 118428 , 152691 , 274055 , 862477 , 894014 , 165676 )

## extract centroid and data for each county
xy <- coordinates(ct.map)
ct.data <- data.frame(lon=xy[,1], lat=xy[,2], ct.map)

## FYI - replicate the map from http://www.indexmundi.com/facts/united-states/quick-facts/connecticut/percent-of-people-of-all-ages-in-poverty#map
colRamp <- colorRampPalette(c("lavender","steelblue4"))
plot(ct.map, col=colRamp(8)[cut(ct.data$county.poverty.rate, 8)], border="white")
text(ct.data$lon, ct.data$lat, ct.data$NAME, cex=0.5)

## interpolation
library(fields)
library(gstat)

# create a raster object
r <- raster(nrow=100, ncol=100, 
            xmn=bbox(ct.map)["x","min"], xmx=bbox(ct.map)["x","max"],
            ymn=bbox(ct.map)["y","min"], ymx=bbox(ct.map)["y","max"],
            crs=proj4string(ct.map))

# ## krigging - gstat
# coordinates(ct.data) <- c("lon","lat")
# proj4string(ct.data) <- proj4string(ct.map)
# v <- variogram(county.poverty.rate~1, ct.data)
# plot(v) # not enough data...??
# vmodel <- fit.variogram(v, vgm(1, "Exp", 10, 1))
# model <- gstat(NULL, "county.poverty.rate", county.poverty.rate~1, x, model=vmodel)
# r <- interpolate(r, model)

# ## krigging - fields
# tps <- Tps(cbind(ct.data$lon, ct.data$lat), ct.data$county.poverty.rate, 
#            weights=ct.data$pop) # not enough data...
# r <- interpolate(r, tps)
# r <- mask(r, ct.map)
