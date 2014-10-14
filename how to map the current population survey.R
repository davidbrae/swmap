# # # # # # # # # # # # # # # # #
# # set the working directory # #
# # # # # # # # # # # # # # # # #

# setwd( "C:/My Directory/SWMAP/" )


# # # # # # # # # # # # # # # #
# # example survey data set # #
# # # # # # # # # # # # # # # #

# current population survey, march supplement


# # # # # # # # # # # # # # # # # #
# # smallest level of geography # #
# # # # # # # # # # # # # # # # # #

# state, core-based statistical areas
# (for connecticut, new england city and town areas)


# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # asdfree.com blog post for this survey microdata # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# http://www.asdfree.com/search/label/current%20population%20survey%20%28cps%29


# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # r code repository for setup and analysis examples # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# https://github.com/ajdamico/usgsd/tree/master/Current%20Population%20Survey


# # # # # # # # # # # # #
# # value of interest # #
# # # # # # # # # # # # #

# poverty rate


# # # # # # # # # # # # # # # # # # # # #
# # step 1: load the survey microdata # #

library(downloader)

# download the 2013 current population survey microdata onto the local disk
cps.years.to.download <- 2013
source_url( "https://raw.github.com/ajdamico/usgsd/master/Current%20Population%20Survey/download%20all%20microdata.R" , prompt = FALSE , echo = TRUE )

# # end of step 1 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 2: conduct your analysis of interest at the smallest geography allowed # #

library(survey)
library(RSQLite)

# following the analysis examples in the r code repository --
# # https://github.com/ajdamico/usgsd/blob/master/Current%20Population%20Survey/2012%20asec%20-%20analysis%20examples.R
# -- calculate the poverty rate at the smallest available geographic area
# within the state of connecticut

# turn on replicate-weighted mean squared errors
options( survey.replicates.mse = TRUE )
# this matches the official census bureau published methods

# construct a replicate-weighted, database-backed survey design object
cps.design <-
	svrepdesign(
		weights = ~marsupwt,
		repweights = "pwwgt[1-9]",
		type = "Fay",
		rho = ( 1 - 1 / sqrt( 4 ) ) ,
		data = "asec13" ,
		dbtype = "SQLite" ,
		dbname = "cps.asec.db"
	)

# restrict the survey object to connecticut records only
connecticut.design <- subset( cps.design , gestfips == 9 )

# the original national survey object is no longer necessary
rm( cps.design ) ; gc()
# remove it and clear up RAM


# calculate the 2012 connecticut state-wide poverty rate
svymean( ~ as.numeric( povll %in% 1:3 ) , subset( connecticut.design , pov_univ == 1 ) )
# using only records in the poverty universe

# note: this estimate and standard error precisely matches
# the census bureau's table 19 (historical poverty by state)
# https://www.census.gov/hhes/www/poverty/data/historical/hstpov19.xls


# # examine which geographies are available # #

# the current population survey identifies records
# from six different core-based statistical areas (cbsa)
svytable( ~ gtcbsa , connecticut.design )

# the current population survey identifies records
# from both metro and non-metro respondents
svytable( ~ gtmetsta , connecticut.design )

# the smallest geography reasonably extracted
# from this survey microdata set will be
# cbsa + metro status combined
svytable( ~ gtcbsa + gtmetsta , connecticut.design )

# simply use both of those geographies in the by= argument
# of the `svyby` command, and re-calculate the poverty rates
smallest.area.statistics <-
	svyby( 
		~ as.numeric( povll %in% 1:3 ) , 
		~ gtcbsa + gtmetsta , 
		subset( connecticut.design , pov_univ == 1 ) , 
		svymean 
	)
# this is the same command as the statewide calculation above,
# except these results have been broken into smaller areas.	

# these are the statistics to be mapped
print( smallest.area.statistics )
# the standard errors are a measure of precision,
# their inverse will serve as the mapping weights

# make this object easier to type..
sas <- smallest.area.statistics

# ..and also easier to read
names( sas )[ names( sas ) == 'as.numeric(povll %in% 1:3)' ] <- 'povrate'

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
# 2010 census summary file #1 for connecticut
# then download the file.
sf1ct.tf <- tempfile()


download.cache( 
	"ftp://ftp2.census.gov/census_2010/04-Summary_File_1/Connecticut/ct2010.sf1.zip" ,
	sf1ct.tf ,
	mode = 'wb'
)


# create a temporary directory
td <- tempdir()


# unzip the summary file #1 files
sf1ct.uz <- unzip( sf1ct.tf , exdir = td )


# file layout from http://www.census.gov/prod/cen2010/doc/sf1.pdf#page=18
sf1ct <- read.fwf( sf1ct.uz[ grep( "ctgeo2010" , sf1ct.uz ) ] , c( -8 , 3 , -16 , 2 , 3 , -22 , 6 , 1 , 4 , -62 , 5 , -186 , 9 , -9 , 11 , 12 , -117 , 1 ) )

# add columns names matching the census bureau, so it's easy to read
names( sf1ct ) <- c( "sumlev" , "state" , "county" , "tract" , "blkgrp" , "block" , "necta" , "pop100" , "intptlat" , "intptlon" , "nmemi" )

# summary level 101 has NECTA and census blocks
sf1ct.101 <- subset( sf1ct , sumlev == "101" )

# one record per census block in connecticut.  see?  same number.
nrow( sf1ct.101 )
# https://www.census.gov/geo/maps-data/data/tallies/census_block_tally.html

# and guess what?  the total connecticut population matches as well.
sum( sf1ct.101$pop100 )
# http://quickfacts.census.gov/qfd/states/09000.html


# so now we have a data.frame object with
# one record per census block,
# and also with the two geography-levels
# that match the current population survey
head( sf1ct.101 )
# in connecticut,
# necta is the cbsa and
# nmemi indicates metropolitan status

# and guess what?
# we've now got the census 2010 weighted populations (field pop100)
# and also each census block's centroid latitude & longitude (fields intptlat + intptlon)

# # end of step 3 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 4: merge the results of your survey analysis with the small-area geography # #

# add the current population survey results'
# geographic identifiers to the connecticut census block data.frame
sf1ct.101 <-
	transform(
		sf1ct.101 ,
		# if the new england city and town area is in the cps result, keep it.  otherwise zero-it.
		gtcbsa = ifelse( necta %in% unique( sas$gtcbsa ) , necta , 0 ) ,
		# if the census block is metro, `gtmetsta` is a one.  otherwise it's a two.
		gtmetsta = ifelse( nmemi == 1 , 1 , 2 )
	)

# confirm that we've created all possible geographies correctly.

# the number of records in our small area statistics..
sas.row <- nrow( sas )

# ..should equal the number of unique-match-merged records..
mrow <- nrow( merge( unique( sf1ct.101[ , c( 'gtcbsa' , 'gtmetsta' ) ] ) , sas ) )

# ..and it does/they do.
stopifnot( sas.row == mrow )

# now the census block-level connecticut census data *could* merge if you wanted it to.


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
popsum <- aggregate( sf1ct.101$pop100 , by = ( sf1ct.101[ , c( 'gtcbsa' , 'gtmetsta' ) ] ) , sum )

# make the column name meaningful
names( popsum )[ names( popsum ) == 'x' ] <- 'popsum'

# merge the popsum onto the sasfile
sas <- merge( sas , popsum )

# now.  merge
	# the poverty rate (the variable of interest)
	# the inverted standard error (the total weight of the broad geography)
	# the population sum (the total population of all census blocks that are part of that geography)

x <- merge( sf1ct.101 , sas )

# confirm no record loss
stopifnot( nrow( x ) == nrow( sf1ct.101 ) )


# (this is the fun part)
# calculate the weight of each census block
x$weight <- x$invse * ( x$pop100 / x$popsum )

# note that weight of all census blocks put together
# sums to the `invse` on the original analysis file
stopifnot( sum( x$weight ) == sum( sas$invse ) )

# remove records with zero population
x <- subset( x , weight > 0 )

# scale all weights so that they average to one
x$weight <- x$weight / mean( x$weight )

# you're done preparing your data.
# keep only the columns you need.
x <- x[ , c( 'povrate' , 'weight' , 'intptlat' , 'intptlon' ) ]

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
ct.map <- 
	qplot( 
		intptlon , 
		intptlat , 
		data = x , 
		colour = povrate ,
		xlab = NULL ,
		ylab = NULL
	)

# choose your coloring and severity from the midpoint
ct.map <- 
	ct.map + 

	scale_colour_gradient2( 
	
		# low poverty rates are good
		low = muted( "blue" ) , 
		# so invert the default colors
		high = muted( "red" ) , 
		
		# shows the most severe difference in coloring
		midpoint = mean( unique( x$povrate ) )
		
		# shows the population-weighted difference in coloring
		# midpoint = weighted.mean( x$povrate , x$weight )
	)

	
# remove all map crap.

ct.map <- 
	ct.map + 

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
ct.map

# print the map with an albers projection.
ct.map + coord_map( project = "albers" , lat0 = min( x$intptlat ) , lat1 = max( x$intptlat ) )
# see ?mapproject for a zillion alternatives


# notice how the dotted delineations match the census bureau's 2006 necta definitions
# http://www2.census.gov/geo/maps/metroarea/us_wall/Dec2006/necta_1206_large.gif

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


xknots <- cover.design( cbind( x$intptlon , x$intptlat ) , number.of.knots )$design


krig.fit <-
	Krig(
		cbind( x$intptlon , x$intptlat ) ,
		x$povrate ,
		weights = x$weight ,
		knots = xknots # ,
		# Covariance = "Matern"
	)

# that is: what is the (weighted) relationship between
# your variable of interest (poverty rate) and
# the x/y points on a grid?

# check this out!
surface( krig.fit )
# you're almost there!

# and here's an alternate approach using the `gam` function
library(mgcv)

gam.fit <- 
	gam( 
		povrate ~ s(intptlon , intptlat ) , 
		weights = weight , 
		data = x
	)
	
# # end of step 6 # #
# # # # # # # # # # #


# # # # # # # # # # # #
# # step 7: outline # #

library(maptools)

shpct.tf <- tempfile()

download.cache( 
	"ftp://ftp2.census.gov/geo/pvs/tiger2010st/09_Connecticut/09/tl_2010_09_state10.zip" ,
	shpct.tf ,
	mode = 'wb'
)

shpct.uz <- unzip( shpct.tf , exdir = td )

ct.shp <- readShapePoly( shpct.uz[ grep( 'shp$' , shpct.uz ) ] )

# projection <- paste0( "+proj=albers +lat_0=" , min( x$intptlat ) , " +lat_1=" , max( x$intptlat ) )

# proj4string( ct.shp ) <- projection
# ct.shp <- spTransform( ct.shp , CRS( projection ) )

# # end of step 7 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # #
# # step 7: make a grid and predict # #

# x.range <- summary( x$intptlon )[ c( 1 , 6 ) ]
# y.range <- summary( x$intptlat )[ c( 1 , 6 ) ]

# add five percent on each side
# x.diff <- abs( x.range[ 2 ] - x.range[ 1 ] ) * 0.05
# y.diff <- abs( y.range[ 2 ] - y.range[ 1 ] ) * 0.05

# x.range[ 1 ] <- x.range[ 1 ] - x.diff
# x.range[ 2 ] <- x.range[ 2 ] + x.diff
# y.range[ 1 ] <- y.range[ 1 ] - y.diff
# y.range[ 2 ] <- y.range[ 2 ] + y.diff


# do you want your map to print decently in a few minutes?
grid.length <- 100
# or beautifully in a few hours?
# grid.length <- 250


# create three identical grid objects
grd <- gam.grd <- krig.grd <- 
	expand.grid(
		intptlon = seq( from = bbox( ct.shp )[1,1] , to = bbox( ct.shp )[1,2] , length = grid.length ) , 
		intptlat = seq( from = bbox( ct.shp )[2,1] , to = bbox( ct.shp )[2,2] , length = grid.length )
	)

# outer.grd <- 
	# data.frame(
		# intptlon = c( x.range[1] - x.diff , x.range[2] + x.diff ) , 
		# intptlat = c( y.range[1] - y.diff , y.range[2] + y.diff )
	# )


# along your rectangular grid,
# what are the predicted values of
# the poverty rate?
krig.grd$kout <- predict( krig.fit , krig.grd )

# alternate grid using gam.fit
gam.grd$gamout <- predict( gam.fit , gam.grd )

# # end of step 7 # #
# # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 9: create a polygon to cover everything outside your boundary # #

library(rgeos)

# convert grd to SpatialPoints object
# coordinates( outer.grd ) <- c( "intptlon" , "intptlat" )

# draw a rectangle around the grd
# ct.shp.diff <- gEnvelope( outer.grd )
# ct.shp.out <- gEnvelope( ct.shp )


# Create a bounding box 10% bigger than the bounding box of connecticut
x_excess = (ct.shp@bbox['x','max'] - ct.shp@bbox['x','min'])*0.1
y_excess = (ct.shp@bbox['y','max'] - ct.shp@bbox['y','min'])*0.1
x_min = ct.shp@bbox['x','min'] - x_excess
x_max = ct.shp@bbox['x','max'] + x_excess
y_min = ct.shp@bbox['y','min'] - y_excess
y_max = ct.shp@bbox['y','max'] + y_excess
bbox = matrix(c(x_min,x_max,x_max,x_min,x_min,
                y_min,y_min,y_max,y_max,y_min),
              nrow = 5, ncol =2)
bbox = Polygon(bbox, hole=FALSE)
bbox = Polygons(list(bbox), "bbox")
ct.shp.out = SpatialPolygons(Srl=list(bbox), pO=1:1, proj4string=ct.shp@proj4string)




# proj4string( ct.shp.diff ) <- projection
# ct.shp.diff <- spTransform( ct.shp.diff , CRS( projection ) )

# get the difference between your boundary and the rectangle
# ct.shp.diff <- gDifference( bbox , ct.shp )
ct.shp.diff <- gDifference( ct.shp.out , ct.shp )

# # end of step 9 # #
# # # # # # # # # # #




library(ggplot2)
library(scales)
library(mapproj)


outside <- fortify( ct.shp.diff )
# outside <- ct.shp.diff

# weighted.
plot <- ggplot(data = krig.grd, aes(x = intptlon, y = intptlat))  #start with the base-plot 
layer1 <- geom_tile(data = krig.grd, aes(fill = kout ))  #then create a tile layer and fill with predicted values
layer2 <- geom_polygon(data=outside, aes(x=long,y=lat,group=group), fill='white')
co <- coord_map( project = "albers" , lat0 = min( x$intptlat ) , lat1 = max( x$intptlat ) )
# print this to a pdf instead, so it formats properly
# plot + layer1 + layer2 + co + scale_fill_gradient( low = muted( 'blue' ) , high = muted( 'red' ) )
# plot + layer1 + layer2 + scale_fill_gradient( low = muted( 'blue' ) , high = muted( 'red' ) )

plot + layer1 + layer2 + co + scale_fill_gradient( low = muted( 'blue' ) , high = muted( 'red' ) ) + coord_equal()




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

