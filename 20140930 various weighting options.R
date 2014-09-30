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

## add survey data to SpatialPolygonDataFrame
ct.map$county.poverty.rate <- c( 8 , 5.8 , 11.4 , 6.4 , 7.2 , 10.9 , 10.7 , 6.1 )
ct.map$pop <- c( 916829 , 189927 , 118428 , 152691 , 274055 , 862477 , 894014 , 165676 )

## extract centroid and data for each county
xy <- coordinates(ct.map)
ct.data <- data.frame(lon=xy[,1], lat=xy[,2], ct.map)




# # # hard-bordered county map
colRamp <- colorRampPalette(c("lavender","steelblue4"))
plot(ct.map, col=colRamp(8)[cut(ct.data$county.poverty.rate, 8)])
text(ct.data$lon, ct.data$lat, ct.data$NAME, cex=0.5)



# create a raster object
ras <- raster(nrow=100, ncol=100, 
            xmn=bbox(ct.map)["x","min"], xmx=bbox(ct.map)["x","max"],
            ymn=bbox(ct.map)["y","min"], ymx=bbox(ct.map)["y","max"],
            crs=proj4string(ct.map))



# gstat unweighted (unknown how to make this weighted?)
coordinates(ct.data) <- c("lon","lat")
proj4string(ct.data) <- proj4string(ct.map)
v <- variogram(county.poverty.rate~1, ct.data)
vmodel <- fit.variogram(v, vgm(1, "Exp", 10, 1))
model <- gstat(NULL, "county.poverty.rate", county.poverty.rate~1, ct.data, model=vmodel)
r <- interpolate(ras, model)
r <- mask(r, ct.map)
plot(r, col=colRamp(8) )



			
# # # unweighted map with Tps
tps <- Tps(cbind(ct.data$lon, ct.data$lat), ct.data$county.poverty.rate, weights=ct.data$pop) 
r <- interpolate(ras, tps)
r <- mask(r, ct.map)
plot(r, col=colRamp(8) )



# # # weighted map with Tps
tps <- Tps(cbind(ct.data$lon, ct.data$lat), ct.data$county.poverty.rate)
r <- interpolate(ras, tps)
r <- mask(r, ct.map)
plot(r, col=colRamp(8) )



# # # unweighted map with Krig
Krig.output <- Krig( cbind(ct.data$lon,ct.data$lat) , ct.data$county.poverty.rate )  
r <- interpolate(ras, Krig.output)
r <- mask(r, ct.map)
plot(r, col=colRamp(8) ,axes=F,legend=F)



# make populated counties this much bigger than others
size <- 2

# # # 2-to-1 weighted map with Krig
Krig.output.wt <- Krig( cbind(ct.data$lon,ct.data$lat) , ct.data$county.poverty.rate ,weights=c( size , 1 , 1 , 1 , 1 , size , size , 1 ) )  
r <- interpolate(ras, Krig.output.wt)
r <- mask(r, ct.map)
plot(r, col=colRamp(8) ,axes=FALSE,legend=FALSE)


# make populated counties this much bigger than others
size <- 5

# # # 5-to-1 weighted map with Krig
Krig.output.wt <- Krig( cbind(ct.data$lon,ct.data$lat) , ct.data$county.poverty.rate ,weights=c( size , 1 , 1 , 1 , 1 , size , size , 1 ) )  
r <- interpolate(ras, Krig.output.wt)
r <- mask(r, ct.map)
plot(r, col=colRamp(8) ,axes=FALSE,legend=FALSE)


# make populated counties this much bigger than others
size <- 50

# # # 50-to-1 weighted map with Krig
Krig.output.wt <- Krig( cbind(ct.data$lon,ct.data$lat) , ct.data$county.poverty.rate ,weights=c( size , 1 , 1 , 1 , 1 , size , size , 1 ) )  
r <- interpolate(ras, Krig.output.wt)
r <- mask(r, ct.map)
plot(r, col=colRamp(8) ,axes=FALSE,legend=FALSE)








# # # weighted map with Krig (using Matern and default smoothness of 0.5)
Krig.output.wt <- Krig( cbind(ct.data$lon,ct.data$lat) , ct.data$county.poverty.rate ,weights=ct.data$pop,Covariance="Matern")  
r <- interpolate(ras, Krig.output.wt)
r <- mask(r, ct.map)
plot(r, col=colRamp(8) ,axes=FALSE,legend=FALSE)


# # # weighted map with Krig (using Matern and heavy smoothness)
Krig.output.wt <- Krig( cbind(ct.data$lon,ct.data$lat) , ct.data$county.poverty.rate ,weights=ct.data$pop,Covariance="Matern",smoothness=0.9)  
r <- interpolate(ras, Krig.output.wt)
r <- mask(r, ct.map)
plot(r, col=colRamp(8) ,axes=FALSE,legend=FALSE)



# # # weighted map with Krig (using lots of colors instead of smoothing)
Krig.output.wt <- Krig( cbind(ct.data$lon,ct.data$lat) , ct.data$county.poverty.rate ,weights=ct.data$pop)  
r <- interpolate(ras, Krig.output.wt)
r <- mask(r, ct.map)
plot(r, col=colRamp(100) ,axes=FALSE,legend=FALSE)



# # # weighted map with Krig (using lots of colors AND heavy smoothing)
Krig.output.wt <- Krig( cbind(ct.data$lon,ct.data$lat) , ct.data$county.poverty.rate ,weights=ct.data$pop,Covariance="Matern",smoothness=0.9)  
r <- interpolate(ras, Krig.output.wt)
r <- mask(r, ct.map)
plot(r, col=colRamp(50) ,axes=FALSE,legend=FALSE)
