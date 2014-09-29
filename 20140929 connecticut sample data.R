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


# extract centroid for each county
ct.centroids <- data.frame(coordinates(ct.map), ct.map$GEOID)
names(ct.centroids) <- c("lon","lat","GEOID")


# # # example data with population weights
ctdata <-
	data.frame(
		ct.centroids ,
		county.poverty.rate = c( 8 , 5.8 , 11.4 , 6.4 , 7.2 , 10.9 , 10.7 , 6.1 ) ,
		county.pop = c( 916829 , 189927 , 118428 , 152691 , 274055 , 862477 , 894014 , 165676 )
	)

