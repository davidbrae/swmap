library(rgdal)
library(raster)
library(gstat)

tf <- tempfile() ; td <- tempdir()

download.file( "ftp://ftp2.census.gov/geo/tiger/TIGER2011/COUNTY/tl_2011_us_county.zip" , tf , mode = 'wb' )

z <- unzip( tf , exdir = td )


# isolate the file that ends with ".shp"
shapefile <- z[ grep( 'shp$' , z ) ]

# read the shapefile into working memory
county.map <- readOGR( shapefile, layer="tl_2011_us_county" )

ct.map <- subset( county.map , STATEFP %in% "09" )


# extract centroid for each county
ct.centroids <- data.frame(coordinates(ct.map), ct.map$GEOID)
names(ct.centroids) <- c("lon","lat","GEOID")

x <-
	data.frame(
		ct.centroids ,
		vowels.in.name =  unlist( lapply( gregexpr( "a|e|i|o|u|y",ct.map$NAME) , length ) ) ,
		SE = rep( 1:2 , 4 ) ,
		N = 999
	)
	
x$v <- ( x$SE )^2 / x$N
x$w <- 1 / x$v


# read in a base map
m <- getData("GADM", country="United States", level=1)
m <- m[ ( m$NAME_1 %in% "Connecticut" ) ,]

# create a raster object
r <- raster(nrow=500, ncol=500, 
            xmn=bbox(m)["x","min"], xmx=bbox(m)["x","max"],
            ymn=bbox(m)["y","min"], ymx=bbox(m)["y","max"],
            crs=proj4string(m))



# run inverse distance weighted model - modified code from ?interpolate...needs more research
model <- 
	gstat(
		id = "vowels.in.name" , 
		formula = vowels.in.name ~ 1 , 
		# weights = "w" , 
		locations = ~ lon + lat , 
		data = x ,
		nmax = 7 , 
		set = list( idp = 0.5 ) 
	)
	
r <- interpolate(r, model, xyNames=c("lon","lat"))
r <- mask(r, m) # discard interpolated values outside the states

# plot map
par(mar=c(0,0,0,0), bty="n")
cols <- c(rgb(0.9,0.8,0.8), rgb(0.9,0.4,0.3),
          rgb(0.8,0.8,0.9), rgb(0.4,0.6,0.9),
          rgb(0.8,0.9,0.8), rgb(0.4,0.9,0.6))
col.ramp <- colorRampPalette(cols) # custom colour ramp
plot(r, axes=FALSE, legend=FALSE, col=col.ramp(100))
plot(m, add=TRUE) # overlay base map
legend("right", pch=22, pt.bg=cols[c(2,4,6)], legend=c(0,1,2), bty="n")


