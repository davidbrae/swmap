# # # # # # # # # # # # # # # # #
# # set the working directory # #
# # # # # # # # # # # # # # # # #

# setwd( "C:/My Directory/SWMAP/" )


# # # # # # # # # # # # # # # #
# # example survey data set # #
# # # # # # # # # # # # # # # #

# new york city housing and vacancy survey


# # # # # # # # # # # # # # # # # #
# # smallest level of geography # #
# # # # # # # # # # # # # # # # # #

# city boro and subboro areas
# (subboro areas are neighborhoods that map to collections of census tracts)


# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # asdfree.com blog post for this survey microdata # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# http://www.asdfree.com/search/label/new%20york%20city%20housing%20and%20vacancy%20survey%20%28nychvs%29


# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # r code repository for setup and analysis examples # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# https://github.com/ajdamico/usgsd/tree/master/New%20York%20City%20Housing%20and%20Vacancy%20Survey


# # # # # # # # # # # # #
# # value of interest # #
# # # # # # # # # # # # #

# ratio of persons per room


# # # # # # # # # # # # # # # # # # # # #
# # step 1: load the survey microdata # #

library(downloader)

# download the 2011 new york city housing and vacancy survey microdata onto the local disk
years.to.download <- 2011
source_url( "https://raw.github.com/ajdamico/usgsd/master/New%20York%20City%20Housing%20and%20Vacancy%20Survey/2002%20-%202011%20-%20download%20all%20microdata.R" , prompt = FALSE , echo = TRUE )

# # end of step 1 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 2: conduct your analysis of interest at the smallest geography allowed # #

library(survey)

# following the analysis examples in the r code repository --
# https://github.com/ajdamico/usgsd/blob/master/New%20York%20City%20Housing%20and%20Vacancy%20Survey/2011%20analysis%20examples.R
# -- calculate the persons per room rate at the smallest available geographic area


# note the large cautionary text that the standard errors for this survey are garbage #


# set R to produce conservative standard errors instead of crashing
# http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )
# this setting matches the MISSUNIT option in SUDAAN

# load the 2011 data files
load( 'nychvs11.rda' )

# create a fake survey design object with just the `hhweight`
# (household weight) variable
occ.d <- svydesign( ~1 , data = occ , weights = ~hhweight )
# this svydesign() call is incorrect because nychvs
# does not release its sampling clusters

# this script is useful as an example of how to map city-wide survey data
# but this actual new york city microdata has incorrect standard errors


# the persons per room variable has two decimals
occ.d <- update( occ.d , pproom = as.numeric( crowd100 ) / 100 )

# calculate the 2011 persons per room rate
svymean( ~ pproom , occ.d )

# # examine which geographies are available # #

# the new york city housing and vacancy survey identifies records
# from subboro (neighborhood) areas within all five boros
svytable( ~ borough + subboro , occ.d )

# simply use both of those geographies in the by= argument
# of the `svyby` command, and re-calculate the poverty rates
smallest.area.statistics <-
	svyby( 
		~ pproom , 
		~ borough + subboro , 
		occ.d , 
		svymean 
	)
# this is the same command as the city-wide calculation above,
# except these results have been broken into smaller areas.	

# these are the statistics to be mapped
print( smallest.area.statistics )
# the standard errors are a measure of precision,
# their inverse will serve as the mapping weights

# again!  don't forget that
# the new york city housing and vacancy survey's
# standard errors are not computable.

# make this object easier to type
sas <- smallest.area.statistics

# # end of step 2 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 3: download and import necessary geographic crosswalks # #

library(downloader)

# load the download.cache and related functions
# to prevent re-downloading of files once they've been downloaded.
source_url(
	"https://raw.github.com/ajdamico/usgsd/master/Download%20Cache/download%20cache.R" ,
	prompt = FALSE ,
	echo = FALSE
)


# create a temporary file containing the census bureau's
# 2010 census summary file #1 for new york state
# then download the file.
sf1ny.tf <- tempfile()


download.cache( 
	"ftp://ftp2.census.gov/census_2010/04-Summary_File_1/New_York/ny2010.sf1.zip" ,
	sf1ny.tf ,
	mode = 'wb'
)
# note: to re-download a file from scratch, add the parameter usecache = FALSE


# unzip the summary file #1 files
sf1ny.uz <- unzip( sf1ny.tf , exdir = tempdir() )


# file layout from http://www.census.gov/prod/cen2010/doc/sf1.pdf#page=18
sf1ny <- read.fwf( sf1ny.uz[ grep( "nygeo2010" , sf1ny.uz ) ] , c( -8 , 3 , -16 , 2 , 3 , -22 , 6 , 1 , 4 , -253 , 9 , -9 , 11 , 12 ) )

# add columns names matching the census bureau, so it's easy to read
names( sf1ny ) <- c( "sumlev" , "state" , "county" , "tract" , "blkgrp" , "block" , "pop100" , "intptlat" , "intptlon" )

# summary level 101 has census tracts and census blocks
sf1ny.101 <- subset( sf1ny , sumlev == "101" )

# one record per census block in new york state.  see?  same number.
nrow( sf1ny.101 )
# https://www.census.gov/geo/maps-data/data/tallies/census_block_tally.html

# and guess what?  the total new york population matches as well.
sum( sf1ny.101$pop100 )
# http://quickfacts.census.gov/qfd/states/36000.html


# separately, read in the crosswalk between new york census tracts and nychvs subboro areas #
# http://www.census.gov/housing/nychvs/data/2011/11subcom1.pdf
# http://www.census.gov/housing/nychvs/data/2011/11subcom2.pdf
nycsb.tf <- tempfile()

download( "https://raw.githubusercontent.com/ajdamico/usgsd/master/New%20York%20City%20Housing%20and%20Vacancy%20Survey/boro%20and%20subboro%20to%20census%20tract%20crosswalk.csv" , nycsb.tf )

nychvs.subboro <- read.csv( nycsb.tf )

# also define nyc counties
nyc.counties <- c( '005' , '047' , '061' , '081' , '085' ) 

# ahh, we also need a county fips code to new york city borough match.
boro.to.county.fips <-
	data.frame( 
		boroname = c( 'Bronx' , 'Brooklyn' , 'Manhattan' , 'Queens' , 'Staten Island' ) , 
		county = as.numeric( nyc.counties ) 
	)

# merge on the borough county fips codes	
bo <- merge( nychvs.subboro , boro.to.county.fips )

# confirm no record loss from the previous merge
stopifnot( nrow( bo ) == nrow( nychvs.subboro ) )

# rename the `bo` data.frame's census tract column to match `sf1ny.101`
names( bo )[ names( bo ) == 'ctract' ] <- 'tract'

# rename the `bo` data.frame's boro column to match `sas`
names( bo )[ names( bo ) == 'boro' ] <- 'borough'

# merge this with the new york state summary file #1..
sf1.bo <- merge( sf1ny.101 , bo )

# ..and guess what?  now we have a perfect match with new york city's 2010 population.
sum( sf1.bo$pop100 )
# http://quickfacts.census.gov/qfd/states/36/3651000.html

# so now we have a data.frame object with
# one record per census block,
# and also with the two geography-levels
# that match the new york city housing and vacancy survey
head( sf1.bo )

# and guess what?
# we've now got the census 2010 weighted populations (field pop100)
# and also each census block's centroid latitude & longitude (fields intptlat + intptlon)

# # end of step 3 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 4: merge the results of your survey analysis with the small-area geography # #

# confirm that we've created all possible geographies correctly.

# the number of records in our small area statistics..
sas.row <- nrow( sas )

# ..should equal the number of unique-match-merged records..
mrow <- nrow( merge( unique( sf1.bo[ , c( 'borough' , 'subboro' ) ] ) , sas ) )

# ..and it does/they do.
stopifnot( sas.row == mrow )

# now the census block-level new york city census data *could* merge if you wanted it to.


# but you don't.  yet.


# the standard error (the `se` field) is a measure of precision.
print( sas )
# the smaller the standard error, the more confident you should be
# that the estimate at a particular geography is correct.


# so invert it.  you heard me.  invert it.
sas$invse <- 1 / sas$se
# a smaller standard error indicates more precision.

# for our purposes, precision can be considered weight! #

# now we've got the weight that we should give each of our estimates #

# distribute that weight across all census blocks #

# aggregate the 2010 census block populations to the geographies that you have.
popsum <- aggregate( sf1.bo$pop100 , by = ( sf1.bo[ , c( 'borough' , 'subboro' ) ] ) , sum )

# make the column name meaningful
names( popsum )[ names( popsum ) == 'x' ] <- 'popsum'

# merge the popsum onto the sasfile
sas <- merge( sas , popsum )

# now.  merge
	# the persons per room rate (the variable of interest)
	# the inverted standard error (the total weight of the broad geography)
	# the population sum (the total population of all census blocks that are part of that geography)

x <- merge( sf1.bo , sas )

# confirm no record loss
stopifnot( nrow( x ) == nrow( sf1.bo ) )


# (this is the fun part)
# calculate the weight of each census block
x$weight <- x$invse * ( x$pop100 / x$popsum )

# note that weight of all census blocks put together
# sums to the `invse` on the original analysis file
stopifnot( all.equal( sum( x$weight ) , sum( sas$invse ) ) )

# remove records with zero population
x <- subset( x , weight > 0 )

# scale all weights so that they average to one
x$weight <- x$weight / mean( x$weight )

# you're done preparing your data.
# keep only the columns you need.
x <- x[ , c( 'pproom' , 'weight' , 'intptlat' , 'intptlon' ) ]

# # end of step 4 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # #
# # step 5: decide on your map parameters # #

library(ggplot2)
library(scales)
library(mapproj)

# before you ever touch surface smoothing or kriging,
# make some decisions about how you generally want
# your map to look:  the projection and coloring

# the options below simply use hadley wickham's ggplot2
# with the census block-level poverty rates and centroids


# initiate the simple map
nyc.map <- 
	qplot( 
		intptlon , 
		intptlat , 
		data = x , 
		colour = pproom ,
		xlab = NULL ,
		ylab = NULL
	)

# choose your coloring and severity from the midpoint
nyc.map <- 
	nyc.map + 

	scale_colour_gradient2( 
	
		# low person per room rates are good
		low = muted( "blue" ) , 
		# so invert the default colors
		high = muted( "red" ) , 
		
		# shows the most severe difference in coloring
		midpoint = mean( unique( x$pproom ) )
		
		# shows the population-weighted difference in coloring
		# midpoint = weighted.mean( x$pproom , x$weight )
	)

	
# remove all map crap.

nyc.map <- 
	nyc.map + 

	scale_x_continuous( breaks = NULL ) +

    scale_y_continuous( breaks = NULL ) +

    theme(
		legend.position = "none" ,
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		panel.background = element_blank(),
		panel.border = element_blank(),
		axis.ticks = element_blank()
	)


# print the map without any projection
nyc.map

# print the map with an albers projection.
nyc.map + coord_map( project = "albers" , lat0 = min( x$intptlat ) , lat1 = max( x$intptlat ) )
# see ?mapproject for a zillion alternatives

# # end of step 5 # #
# # # # # # # # # # #



# # # # # # # # # # # # # # # # # #
# # step 6: tie knots and krige # #

library(fields)

# how many knots should you make?

# cannot be more than `nrow( x )`
# but one hundred is okay.
# if you've got a powerful computer,
# you can increase this
number.of.knots <- min( 100 , nrow( x ) )
# number.of.knots <- min( 250 , nrow( x ) )


stop( "stop using knots!  use population-weighted centroids instead" )
xknots <- cover.design( cbind( x$intptlon , x$intptlat ) , number.of.knots )$design


krig.fit <-
	Krig(
		cbind( x$intptlon , x$intptlat ) ,
		x$pproom ,
		weights = x$weight ,
		knots = xknots # ,
		# Covariance = "Matern"
	)

# that is: what is the (weighted) relationship between
# your variable of interest (persons per room) and
# the x/y points on a grid?

# check this out!
surface( krig.fit )
# you're almost there!

# and here's an alternate approach using the `gam` function
library(mgcv)

gam.fit <- 
	gam( 
		pproom ~ s(intptlon , intptlat ) , 
		weights = weight , 
		data = x
	)
	
# # end of step 6 # #
# # # # # # # # # # #


# # # # # # # # # # # #
# # step 7: outline # #

library(maptools)

shpny.tf <- tempfile()

download.cache(
	"http://www2.census.gov/geo/tiger/TIGER2010/COUNTY/2010/tl_2010_36_county10.zip" ,
	shpny.tf ,
	mode = 'wb'
)
# note: to re-download a file from scratch, add the parameter usecache = FALSE

shpny.uz <- unzip( shpny.tf , exdir = tempdir() )

ny.shp <- readShapePoly( shpny.uz[ grep( 'shp$' , shpny.uz ) ] )

# limit the shapefile to only the five boroughs
nyc.shp <- subset( ny.shp , as.numeric( as.character( COUNTYFP10 ) ) %in% c( 5 , 47 , 61 , 81 , 85 ) )



# projection <- paste0( "+proj=albers +lat_0=" , min( x$intptlat ) , " +lat_1=" , max( x$intptlat ) )

# proj4string( nyc.shp ) <- projection
# nyc.shp <- spTransform( nyc.shp , CRS( projection ) )

# # end of step 7 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # #
# # step 8: make a grid and predict # #


x.range <- summary( x$intptlon )[ c( 1 , 6 ) ]
y.range <- summary( x$intptlat )[ c( 1 , 6 ) ]

# add five percent on each side
x.diff <- abs( x.range[ 2 ] - x.range[ 1 ] ) * 0.05
y.diff <- abs( y.range[ 2 ] - y.range[ 1 ] ) * 0.05

x.range[ 1 ] <- x.range[ 1 ] - x.diff
x.range[ 2 ] <- x.range[ 2 ] + x.diff
y.range[ 1 ] <- y.range[ 1 ] - y.diff
y.range[ 2 ] <- y.range[ 2 ] + y.diff



# this is a small map, so using a very fine grid does not take much time
grid.length <- 1000


# create three identical grid objects
grd <- gam.grd <- krig.grd <- 
	expand.grid(
		intptlon = seq( from = bbox( nyc.shp )[1,1] , to = bbox( nyc.shp )[1,2] , length = grid.length ) , 
		intptlat = seq( from = bbox( nyc.shp )[2,1] , to = bbox( nyc.shp )[2,2] , length = grid.length )
	)

outer.grd <- 
	data.frame(
		intptlon = c( x.range[1] - x.diff , x.range[2] + x.diff ) , 
		intptlat = c( y.range[1] - y.diff , y.range[2] + y.diff )
	)


# along your rectangular grid,
# what are the predicted values of
# the poverty rate?
krig.grd$kout <- predict( krig.fit , krig.grd )

# alternate grid using gam.fit
gam.grd$gamout <- predict( gam.fit , gam.grd )

# # end of step 8 # #
# # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 9: create a polygon to cover everything outside the boundary # #

library(rgeos)

# convert grd to SpatialPoints object
coordinates( outer.grd ) <- c( "intptlon" , "intptlat" )

# draw a rectangle around the grd
nyc.shp.diff <- gEnvelope( outer.grd )
nyc.shp.out <- gEnvelope( nyc.shp )


# Create a bounding box 10% bigger than the bounding box of connecticut
# x_excess = (nyc.shp@bbox['x','max'] - nyc.shp@bbox['x','min'])*0.1
# y_excess = (nyc.shp@bbox['y','max'] - nyc.shp@bbox['y','min'])*0.1
# x_min = nyc.shp@bbox['x','min'] - x_excess
# x_max = nyc.shp@bbox['x','max'] + x_excess
# y_min = nyc.shp@bbox['y','min'] - y_excess
# y_max = nyc.shp@bbox['y','max'] + y_excess
# bbox = matrix(c(x_min,x_max,x_max,x_min,x_min,
                # y_min,y_min,y_max,y_max,y_min),
              # nrow = 5, ncol =2)
# bbox = Polygon(bbox, hole=FALSE)
# bbox = Polygons(list(bbox), "bbox")
# nyc.shp.out = SpatialPolygons(Srl=list(bbox), pO=1:1, proj4string=nyc.shp@proj4string)




# proj4string( nyc.shp.diff ) <- projection
# nyc.shp.diff <- spTransform( nyc.shp.diff , CRS( projection ) )

# get the difference between your boundary and the rectangle
# nyc.shp.diff <- gDifference( bbox , nyc.shp )
nyc.shp.diff <- gDifference( nyc.shp.out , nyc.shp )

# # end of step 9 # #
# # # # # # # # # # #



stop( "capping your outliers is critically important.  the scale is much more visible if they are maxxed and minned" )



library(ggplot2)
library(scales)
library(mapproj)


outside <- fortify( nyc.shp.diff )
# outside <- nyc.shp.diff

# weighted.
plot <- ggplot(data = krig.grd, aes(x = intptlon, y = intptlat))  #start with the base-plot 
layer1 <- geom_tile(data = krig.grd, aes(fill = kout ))  #then create a tile layer and fill with predicted values
layer2 <- geom_polygon(data=outside, aes(x=long,y=lat,group=group), fill='white')
co <- coord_map( project = "albers" , lat0 = min( x$intptlat ) , lat1 = max( x$intptlat ) )
# print this to a pdf instead, so it formats properly
# plot + layer1 + layer2 + co + scale_fill_gradient( low = muted( 'blue' ) , high = muted( 'red' ) )
# plot + layer1 + layer2 + scale_fill_gradient( low = muted( 'blue' ) , high = muted( 'red' ) )
layer3 <- geom_path( data = nyc.shp , aes( x=long , y=lat , group = group ) )

co <- coord_map( project = "newyorker" , r = 0 )

myplot <-
	plot + layer1 + layer2 + layer3 + scale_fill_gradient( low = 'white' , high = muted( 'red' ) )


# plot + layer1 + layer2 + co + scale_fill_gradient( low = muted( 'blue' ) , high = muted( 'red' ) ) + coord_equal()

tf <- tempfile()
for ( this.county in nyc.counties ){

	this.file <- 
		paste0(
			"http://www2.census.gov/geo/tiger/TIGER2013/AREAWATER/tl_2013_36" ,
			this.county ,
			"_areawater.zip"
		)
		
	download.file( this.file , tf )
	
	z <- unzip( tf , exdir = tempdir() )
	
	nyc.shp <- readShapePoly( z[ grep( 'shp$' , z ) ] )

	water <- fortify( nyc.shp )
	water <- geom_polygon(data = water , aes(x=long,y=lat,group=group), fill='white')
	
	myplot <- myplot + water
	
}







# weighted.
plot <- ggplot(data = gam.grd, aes(x = intptlon, y = intptlat))  #start with the base-plot 
layer1 <- geom_tile(data = gam.grd, aes(fill = kout ))  #then create a tile layer and fill with predicted values
# sol <- fortify( ct.shp )
# layer2 <- geom_path(data = sol, aes(long, lat), colour = "grey40", size = 1)
co <- coord_map( project = "albers" , lat0 = min( x$intptlat ) , lat1 = max( x$intptlat ) )
# print this to a pdf instead, so it formats properly
# plot + layer1 + layer2 + co + scale_fill_gradient( low = muted( 'blue' ) , high = muted( 'red' ) )
plot + layer1 + co + scale_fill_gradient( low = muted( 'blue' ) , high = muted( 'red' ) )



# raster plots

coordinates(krig.grd) <- coordinates(gam.grd) <- c("intptlon", "intptlat")
gridded(krig.grd) <- gridded(gam.grd) <- TRUE
krig.r <- raster(krig.grd)
gam.r <- raster(gam.grd)

colRamp <- colorRampPalette(c(muted("blue"),muted("red")))
plot(krig.r, axes=FALSE, col=colRamp(100), main="Krig")
plot(ct.shp.diff, add=TRUE, col="white", border="white", lwd=5)
degAxis(1)
degAxis(2)
box()

plot(gam.r, axes=FALSE, col=colRamp(100), main="GAM")
plot(ct.shp.diff, add=TRUE, col="white", border="white", lwd=5)
degAxis(1)
degAxis(2)
box()


stop( "do you want to use the nyc specific projection for this?" )

