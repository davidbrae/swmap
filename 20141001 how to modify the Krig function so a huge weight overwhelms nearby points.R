# load the connecticut data
library(rgdal)
library(raster)
library(gstat)
library(fields)

# global mapping settings
par(mar=c(0,0,0,0), bty="n")
colRamp <- colorRampPalette(c("lavender","steelblue4"))


# download the county file
tf <- tempfile() ; td <- tempdir()
download.file( "ftp://ftp2.census.gov/geo/tiger/TIGER2011/COUNTY/tl_2011_us_county.zip" , tf , mode = 'wb' )
z <- unzip( tf , exdir = td )


# isolate the file that ends with ".shp"
shapefile <- z[ grep( 'shp$' , z ) ]

# read the shapefile into working memory
county.map <- readOGR( shapefile, layer="tl_2011_us_county" )
ct.map <- subset( county.map , STATEFP %in% "09" )

## add some connecticut data
ct.map$county.poverty.rate <- c( 8 , 5.8 , 11.4 , 6.4 , 7.2 , 10.9 , 10.7 , 6.1 )
ct.map$pop <- c( 916829 , 189927 , 118428 , 152691 , 274055 , 862477 , 894014 , 165676 )

## extract centroid and data for each county
xy <- coordinates(ct.map)
ct.data <- data.frame(lon=xy[,1], lat=xy[,2], ct.map)


# create a raster object
ras <- raster(nrow=100, ncol=100, 
            xmn=bbox(ct.map)["x","min"], xmx=bbox(ct.map)["x","max"],
            ymn=bbox(ct.map)["y","min"], ymx=bbox(ct.map)["y","max"],
            crs=proj4string(ct.map))




# # # here is what connecticut looks like with hard borders
colRamp <- colorRampPalette(c("lavender","steelblue4"))
plot(ct.map, col=colRamp(8)[cut(ct.data$county.poverty.rate, 8)])
text(ct.data$lon, ct.data$lat, ct.data$NAME, cex=0.5)



# # # hartford, new haven, and fairfield are the three most populous counties


# make the populated counties the same as other counties
size <- 1

# # # 1-to-1 weighted map with Krig
Krig.output.wt <- Krig( cbind(ct.data$lon,ct.data$lat) , ct.data$county.poverty.rate ,weights=c( size , 1 , 1 , 1 , 1 , size , size , 1 ),Covariance="Matern")  
r <- interpolate(ras, Krig.output.wt)
r <- mask(r, ct.map)
plot(r, col=colRamp(100) ,axes=FALSE,legend=FALSE)




# make populated counties 5x as impactful as other points
size <- 5

# # # 5-to-1 weighted map with Krig
Krig.output.wt <- Krig( cbind(ct.data$lon,ct.data$lat) , ct.data$county.poverty.rate ,weights=c( size , 1 , 1 , 1 , 1 , size , size , 1 ),Covariance="Matern")  
r <- interpolate(ras, Krig.output.wt)
r <- mask(r, ct.map)
plot(r, col=colRamp(100) ,axes=FALSE,legend=FALSE)



# make populated counties 100x as impactful as other points
size <- 100

# # # 1-to-1 weighted map with Krig
Krig.output.wt <- Krig( cbind(ct.data$lon,ct.data$lat) , ct.data$county.poverty.rate ,weights=c( size , 1 , 1 , 1 , 1 , size , size , 1 ),Covariance="Matern")  
r <- interpolate(ras, Krig.output.wt)
r <- mask(r, ct.map)
plot(r, col=colRamp(100) ,axes=FALSE,legend=FALSE)

