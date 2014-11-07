# # # # # # # # # # # # # # # # #
# # set the working directory # #
# # # # # # # # # # # # # # # # #

# setwd( "C:/My Directory/SWMAP/" )


# # # # # # # # # # # # # # # #
# # example survey data set # #
# # # # # # # # # # # # # # # #

# european social survey


# # # # # # # # # # # # # # # # # # # # #
# # different from other maps because # #
# # # # # # # # # # # # # # # # # # # # #

# displays an ordinal categorical variable
# approximates (unweighted) centroid calculations


# # # # # # # # # # # # # # # # # #
# # smallest level of geography # #
# # # # # # # # # # # # # # # # # #

# varying regions across multiple countries


# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # asdfree.com blog post for this survey microdata # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# http://www.asdfree.com/search/label/european%20social%20survey%20%28ess%29


# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # r code repository for setup and analysis examples # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# https://github.com/ajdamico/usgsd/tree/master/European%20Social%20Survey


# # # # # # # # # # # # #
# # value of interest # #
# # # # # # # # # # # # #

# average hours of television watched every weekday


# # # # # # #
# # flaws # #
# # # # # # #

# map implies huge fluctuation in television viewing time
# when really, almost everybody watches about two hours of tv.


# # # # # # # # # # # # # # # # # # # # #
# # step 1: load the survey microdata # #

library(downloader)

# download the 2012 european social survey microdata onto the local disk
# note that this requires (free) registration before the download will work
# http://www.europeansocialsurvey.org/user/new
your.email <- "email@address.com"
source_url( "https://raw.github.com/ajdamico/usgsd/master/European%20Social%20Survey/download%20all%20microdata.R" , prompt = FALSE , echo = TRUE )

# # end of step 1 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 2: conduct your analysis of interest at the smallest geography allowed # #

library(survey)
library(stringr)

# following the analysis examples in the r code repository --
# # https://github.com/ajdamico/usgsd/blob/master/European%20Social%20Survey/replication.R
# -- calculate the average number of hours of television watched
# at the smallest available geographic area, across all available countrieswithin the state of connecticut

# load the complete integrated file as a data.frame `x`
load( "./2012/integrated.rda" )

# skip israel because it's too far away from the others to map
integrated <- subset( x , cntry != 'IL' )
# and also because it does not currently include a `SDDF`

# skip iceland because it's also on the border and because
integrated <- subset( integrated , cntry != 'IS' )
# its results in the final map are boring.  it majorly decreases
# the map's resolution while not adding much of a story on its own

# initiate an empty object
sddf <- NULL

# loop through all countries in the `integrated` file
for ( j in unique( integrated$cntry ) ){

	# find the filepath of the `SDDF` file within the current country's 2012 folder
	sddf.tn <- grep( 'SDDF' , list.files( paste0( "./2012/" , j ) ) , value = TRUE )
	
	# load that `.rda` file into working memory
	load( paste0( './2012/' , j , '/' , sddf.tn ) )
	
	# stack what's already in the `sddf` data.frame (from previous loops)
	# on top of the latest `sddf` file, until you have
	# the clustering & strata variables from every country.
	sddf <- 
		rbind( 
			sddf , 
			x[ , c( 'cntry' , 'idno' , 'psu' , 'stratify' , 'prob' ) ] 
		)
	
}

# merge the complete european social survey integrated file
# with this complex sample information
y <- merge( integrated , sddf )

# confirm that zero records have been lost at sea.
stopifnot( nrow( y ) == nrow( integrated ) )

# construct a complex sample survey design object
integrated.design <- 
	svydesign(
		ids = ~psu ,
		strata = ~stratify ,
		probs = ~prob ,
		data = y ,
		nest = TRUE
	)

# set R to produce conservative standard errors instead of crashing
# http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )
# this setting matches the MISSUNIT option in SUDAAN


# and now, in the blink of an eye, calculate the median category of
# hours of television watched per weekday.  `tvtot` is an ordinal variable:
# average television viewing in half-hour intervals.  computing its median
# across almost every region available in the european social survey
# does throw out some information, but it's probably a reasonable
# proxy for cross-continental high versus low television viewership areas
smallest.area.statistics <- 
	svyby( 
		~ tvtot , 
		~ region , 
		integrated.design , 
		svyquantile ,
		0.5 , 
		ties = "rounded" ,
		interval.type = "betaWald" ,
		na.rm = TRUE , 
		ci = TRUE
	)

# these are the statistics to be mapped
print( smallest.area.statistics )
# the standard errors are a measure of precision,
# their inverse will serve as the mapping weights

# but wait, problem problem awooooogah
summary( smallest.area.statistics )
# some of the standard errors are missing.
# that will lead to missing weights, a no-no.

# replace all missing standard errors with the non-zero minimum
smallest.area.statistics[ is.na( smallest.area.statistics$se ) , 'se' ] <-
	min( subset( smallest.area.statistics , !is.na( se ) )$se )

# excellent, now everybody has a weight
summary( smallest.area.statistics )

# make this object easier to type
sas <- smallest.area.statistics

# trim the regions, and use the variable name
# that eurostat uses on its shapefiles
sas$NUTS_ID <- str_trim( sas$region )

# # end of step 2 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # #
# # step 3: choose outlines # #

library(raster)
library(rgdal)
library(maptools)
library(downloader)

source_url(
	"https://raw.github.com/ajdamico/usgsd/master/Download%20Cache/download%20cache.R" ,
	prompt = FALSE ,
	echo = FALSE
)
# note: to re-download a file from scratch, add the parameter usecache = FALSE


# # # map of the world # # #

# initiate a temporary file
tf <- tempfile()

# use eurostat's map of the world
world.fn <- "http://epp.eurostat.ec.europa.eu/cache/GISCO/geodatafiles/CNTR_2014_03M_SH.zip"

# store it to the local disk
download.cache( world.fn , tf )

# unzip it
world.uz <- unzip( tf , exdir = tempdir() )

# identify the shapefile
world.sfn <- grep( 'CNTR_RG(.*)shp$' , world.uz , value = TRUE )

# read it in
world.shp <- readOGR( world.sfn  , layer = gsub( "\\.shp" , "" , basename( world.sfn ) ) )

# here's the outline of every country in the world
plot(world.shp)


# # # map of europe # # #

# use eurostat's map of europe
eu.fn <- "http://epp.eurostat.ec.europa.eu/cache/GISCO/geodatafiles/NUTS_2010_03M_SH.zip"

# store it to the local disk
download.cache( eu.fn , tf )

# unzip it
eu.uz <- unzip( tf , exdir = tempdir() )

# identify the shapefile
eu.sfn <- grep( 'NUTS_RG(.*)shp$' , eu.uz , value = TRUE )

# read it in
eu.shp <- readOGR( eu.sfn  , layer = gsub( "\\.shp" , "" , basename( eu.sfn ) ) )

# here's the outline of every country in europe
plot( eu.shp )
# including all administrative regions

# here's the same plot, but only using national borders.
plot( subset( eu.shp , STAT_LEVL_ == 0 ) )


# # # map of eu countries available in the european social survey # # #

matches.shp <- subset( eu.shp , NUTS_ID %in% unique( sas$NUTS_ID ) )

plot( matches.shp , col = 'red' )

# the canary islands (spanish land off of the coast of morocco)
# have about two million inhabitants (6x the population of iceland)
# but they're isolated from all other landmasses
# (iceland has two regions, the canary islands has just one)
# re-draw the map without them..
plot( subset( matches.shp , NUTS_ID != 'ES70' ) , col = 'red' )
# ..alright, it looks a bit better, so i'm going to throw them out.

# store this shapefile
matnci.shp <- subset( matches.shp , NUTS_ID != 'ES70' )
# matches with no canary islands.

# plot the international borders on top of 
plot( world.shp , add = TRUE )

# so look at that.  there are some glaring omissions
# european union members like austria, greece, latvia did not participate
# and then there are some regions (like corsica and that italian province of molise)
# that are also missing.  we'll deal with those troublemakers later.

# but guess what?
nonmatches <- unique( sas[ !( sas$NUTS_ID %in% matnci.shp@data$NUTS_ID ) , 'NUTS_ID' ] )
# the european social survey includes certain non-european states
# so let's fetch their provincial boundaries as well
print( nonmatches )


# # # add albania, ukraine, russian federation, kosovo # # #

# pulling from the administrative region data at http://www.gadm.org/

# specify the filenames of albania, ukraine, kosovo, and russian shapefiles
ab.fn <- 'http://biogeo.ucdavis.edu/data/diva/adm/ALB_adm.zip'
uk.fn <- 'http://biogeo.ucdavis.edu/data/diva/adm/UKR_adm.zip'
ko.fn <- 'http://biogeo.ucdavis.edu/data/diva/adm/KO-_adm.zip'
ru.fn <- 'http://biogeo.ucdavis.edu/data/diva/adm/RUS_adm.zip'


# albania #

download.cache( ab.fn , tf )
alb.files <- unzip( tf , exdir = tempdir() )

# there are four administrative files to choose from.
alb.files

# you want the regions, which are stored in `adm1`
alb.shp <- grep( 'adm1(.*)shp$' , alb.files , value = TRUE )

# read it in
alb <- readOGR( alb.shp  , layer = gsub( "\\.shp" , "" , basename( alb.shp ) ) )

# compare the region names (in the ess codebook)
# with the region names in the shapefile
alb@data

# tack on the region identifiers
alb@data$NUTS_ID <- paste0( 'AL' , c( '01' , '09' , '02' , '03' , '04' , '05' , '06' , '07' , '08' , '10' , '11' , '12' ) )

# re-map everything, suddenly we've got a reddened albania
# (across the adriatic sea from the bootheel of italy)
plot( matnci.shp , col = 'red' )
plot( world.shp , add = TRUE )
plot( alb , add = TRUE , col = 'red' )


# kosovo #

download.cache( ko.fn , tf )
kos.files <- unzip( tf , exdir = tempdir() )

# there are three administrative files to choose from.
kos.files

# you want the regions, which are stored in `adm1`
kos.shp <- grep( 'adm1(.*)shp$' , kos.files , value = TRUE )

# read it in
kos <- readOGR( kos.shp  , layer = gsub( "\\.shp" , "" , basename( kos.shp ) ) )

# compare the region names (in the ess codebook)
# with the region names in the shapefile
kos@data

# tack on the region identifiers
kos@data$NUTS_ID <- paste0( 'XK' , c( 4 , 5 , 2 , 6 , 1 , 3 , 7 ) )

# still have your map loaded?
# make kosovo (northeast of albania) red.
plot( kos , add = TRUE , col = 'red' )


# ukraine #

download.cache( uk.fn , tf )
ukr.files <- unzip( tf , exdir = tempdir() )

# there are three administrative files to choose from.
ukr.files

# you want the regions, which are stored in `adm1`
ukr.shp <- grep( 'adm1(.*)shp$' , ukr.files , value = TRUE )

# read it in
ukr <- readOGR( ukr.shp  , layer = gsub( "\\.shp" , "" , basename( ukr.shp ) ) )

# compare the region names (in the ess codebook)
# with the region names in the shapefile
ukr@data

# in the ukranian shapefile, this is the order of UA01 - UA26
ukr.ord <- c( 4 , 24 , 25 , 5 , 6 , 27 , 23 , 26 , 7 , 11 , 13 , 14 , 15 , 16 , 17 , 18 , 19 , 21 , 22 , 8 , 9 , 10 , 1 , 3 , 2 , 12 )

# tack the regions on to the ukranian shapefile
ukr@data[ ukr.ord , 'NUTS_ID' ] <- paste0( 'UA' , str_pad( 1:26 , 2 , pad = '0' ) )

# throw out regions not included in the ess
# on the southern tip of crimea
ukr <- subset( ukr , !is.na( NUTS_ID ) )

# look at what you've got
plot( ukr , add = TRUE , col = 'red' )


# russia #

# http://www.europeansocialsurvey.org/docs/round6/fieldwork/russian_federation/ESS_region_variable_in_the_russian_federation.pdf

download.cache( ru.fn , tf )
rus.files <- unzip( tf , exdir = tempdir() )

# there are three administrative files to choose from.
rus.files

# you want the regions, which are stored in `adm1`
rus.shp <- grep( 'adm1(.*)shp$' , rus.files , value = TRUE )

# read it in
rus <- readOGR( rus.shp  , layer = gsub( "\\.shp" , "" , basename( rus.shp ) ) )

# pull in the russian regional crosswalk that i made by hand just for you
download( "https://raw.githubusercontent.com/davidbrae/swmap/master/2012%20ESS%20crosswalk%20of%20Russian%20Regions.csv" , tf )
# http://www.europeansocialsurvey.org/docs/round6/fieldwork/russian_federation/ESS_region_variable_in_the_russian_federation.pdf

rus.xwalk <- read.csv( tf )

# tack on the various NUTS_IDs for each of the available regions
rus@data$NUTS_ID <- rus.xwalk[ match( rus@data$ID_1 , rus.xwalk$ID_1 ) , 'region' ]


# # want to explore this a bit?  sure you do # #

# lop off the easternmost siberian province so this map doesn't wrap around
rusnc <- subset( rus , ID_1 != 2524 )

# russia without the far far eastern tip
plot(rusnc)

# plot all eight regions so you can see what this landmass looks like
for ( i in 1:8 ) plot( subset( rus , str_trim( NUTS_ID ) == paste0( 'RU1' , i ) ) , add = TRUE , col = rainbow(8)[i] )

# but that's not terribly interesting,
# and the siberian sample sizes are tiny.

# the main point of this survey is continental europe,
# so the principle here will be: center on the continent
# and if parts of russia get colored on the edge of the map, cool.

# # exploration end # #

# re-map everything, suddenly we've got lot more available geographies
plot( matnci.shp , col = 'red' )
plot( world.shp , add = TRUE )
plot( alb , add = TRUE , col = 'red' )
plot( kos , add = TRUE , col = 'red' )
plot( ukr , add = TRUE , col = 'red' )
plot( rus , add = TRUE , col = 'red' )
# but it's still not as tight of a map as it could be


# # here's the goal # #
# russia is going to be off the map, no question
# but all of the other countries should be visible entirely.
# using the current bounding box of this shape,
plot( 1 ,
	type = 'n' , axes = FALSE , xlab = "" , ylab = "" ,
	xlim = bbox( matnci.shp )[ 1 , ] , 
	ylim = bbox( matnci.shp )[ 2 , ]
)

# ukraine gets cut off.
plot( ukr , add = TRUE , col = 'red' )
# yes, russia gets cut off too, but we probably have to live with that
plot( rus , add = TRUE , col = 'red' )
# let's re-calculate the bounding box with ukraine
# so that it's not cut off.

# we can skip this for kosovo and albania
# because they are both *within* the bounding box
# created by including cyprus.

# start with the main europe map,
# transform its projection to standard longlat
ess.shp <- spTransform( matnci.shp , CRS( "+proj=longlat" ) )
# repeat this conversion for ukraine
ukr.ll <- spTransform( ukr , CRS( "+proj=longlat" ) )

# keep only the NUTS identifier in both shapefiles
ess.shp@data <- ess.shp@data[ "NUTS_ID" ]
ukr.ll@data <- ukr.ll@data[ "NUTS_ID" ]

# merge both shapes
ess.shp <- rbind( ess.shp , ukr.ll )

# now you could plot it on its own..
plot( ess.shp )
# ..but the purpose of this was to determine
( bb <- bbox( as( 1.05 * extent( ess.shp ), "SpatialPolygons" ) ) )
# the latitude and longitude limits that make sense.

# initiate the plot using that new ukranian-extended bounding box
plot( 1 ,
	type = 'n' , axes = FALSE , xlab = "" , ylab = "" ,
	xlim = bb[ 1 , ] , 
	ylim = bb[ 2 , ]
)

# add in every shape, one by one.

# no color for the world map
plot( world.shp , add = TRUE )

# then add red to all regions with ess microdata.
plot( matnci.shp , col = 'red' , add = TRUE )
plot( alb , add = TRUE , col = 'red' )
plot( kos , add = TRUE , col = 'red' )
plot( ukr , add = TRUE , col = 'red' )
plot( rus , add = TRUE , col = 'red' )


# # end of step 3 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 4: merge the results of your survey analysis with the small-area geography # #

library(rgeos)
library(sqldf)

# # note: this uses the unweighted centroid,
# # rather than the population-weighted centroid.
# # population-weighted centroids aren't too difficult
# # to calculate (see the other mapping scripts for examples)
# # but require that you merge on the smallest possible area
# # and then distribute the small area statistics to even smaller geographies.
# # the population estimates *within* each of these regions
# # would mostly be the NUTS3 estimate from
# # each country's most recent decennial census.
# # but it's a lot of work with relatively little payoff for a
# # continent-wide map, so this map works around it.
# # it's not perfect, though.  you do experience issues like
# # the centroid of norway's northernmost province is in sweden.

# calculate the centroids of each administrative boundary
matnci.cen <- gCentroid( matnci.shp , byid = TRUE )

# you can have a little detour to see what these look like
plot( matnci.cen )
plot( matnci.shp , add = TRUE )
# those look pretty central, huh?

# additionally calculate the centroids
# of albania, kosovo, and ukraine
alb.cen <- gCentroid( alb , byid = TRUE )
kos.cen <- gCentroid( kos , byid = TRUE )
ukr.cen <- gCentroid( ukr , byid = TRUE )

# now russia is different from the others.
rus.cen <- gCentroid( rus , byid = TRUE )
# for each of the previous centroid calculations,
# you had one centroid per small area statistic
# but with russia, you have about ten.
# we'll have to deal with this later.


# # # construct the object containing values at every point # # #

# initiate a function that combines centroids with `tvtot` and `se`
# but also calculates the number of instances of each NUTS_ID
cte <- 
	function( cen , shp ){
		out <- data.frame( cen )
		out$NUTS_ID <- shp@data$NUTS_ID
		out$tvtot <- sas[ match( shp@data$NUTS_ID , sas$NUTS_ID ) , 'tvtot' ]
		out$se <- sas[ match( shp@data$NUTS_ID , sas$NUTS_ID ) , 'se' ]
		nc <- sqldf( "select NUTS_ID , count(*) as nc from out group by NUTS_ID" )
		out <- merge( out , nc )
		
		out
	}

matnci.vals <- cte( matnci.cen , matnci.shp )
alb.vals <- cte( alb.cen , alb )
kos.vals <- cte( kos.cen , kos )
ukr.vals <- cte( ukr.cen , ukr )
rus.vals <- cte( rus.cen , rus )

# note that *only* russia has multiple centroids per NUTS_ID
head( rus.vals )
# all of the other data.frame objects have a `nc` field of one
unique( c( matnci.vals$nc , alb.vals$nc , kos.vals$nc , ukr.vals$nc ) )	
# see?  seeeee?  i told you.

# stack each of these values data.frame objects together
x <- rbind( matnci.vals , alb.vals , kos.vals , ukr.vals , rus.vals )

# alright.  remember that the standard error (the `se` field) is a measure of precision.
print( x )
# the smaller the standard error, the more confident you should be
# that the estimate at a particular geography is correct.


# so invert it.  you heard me.  invert it.
x$invse <- 1 / x$se
# a smaller standard error indicates more precision.

# for our purposes, precision can be considered weight! #

# we also blankly distributed our values and standard errors
# across the multi-province regions in russia without
# accounting for multiple regions per province.  let's do that now.
x$weight <- x$invse / x$nc
# so we have quite a few more russian centroids,
# but each of them has far lower weight than other points on our map


# as mentioned before, this isn't as ideal.  for example:
# st. petersburg gets exactly one eleventh of the total
# "north-western" weight because there are eleven
# administrative regions in the north west.
# that's a rough cut.  this effect might be even more biased
# for kaliningrad (the russian enclave west of lithuania)
# which is also in the north-western region


# if you'd like to calculate population-weighted centroids
# for all of these countries, it's just a lot of manual labor


# note that this distributed weight sums to
# the `invse` on the original analysis file
stopifnot( all.equal( sum( x$weight ) , sum( 1 / subset( sas , NUTS_ID != 'ES70' )$se ) ) )
# don't forget to pull out the weight of the canary islands as well!
# once you do that, the sums of the inverted, divided standard errors match up

# scale all weights so that they average to one
x$weight <- x$weight / mean( x$weight )

# you're done preparing your data.
# keep only the columns you need.
x <- x[ , c( 'x' , 'y' , 'tvtot' , 'weight' ) ]


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
# with the region-level television viewing rates and centroids


# initiate the simple map
eu.map <- 
	qplot( 
		x , 
		y , 
		data = x , 
		colour = tvtot ,
		xlab = NULL ,
		ylab = NULL
	)

# look at that.. russia is still throwing things off
eu.map

# set the bounding box limits that
# you and i agreed to earlier in the script
# oh, also, remove all map crap
eu.map <- 
	eu.map + 

	scale_x_continuous( limits = bb[ 1 , ] , breaks = NULL ) +

    scale_y_continuous( limits = bb[ 2 , ] , breaks = NULL ) +

    theme(
		legend.position = "none" ,
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		panel.background = element_blank(),
		panel.border = element_blank(),
		axis.ticks = element_blank()
	)

# the asian part of russia has now been cut off
eu.map

# print the map with an albers projection.
eu.map + coord_map( project = "albers" , lat0 = min( x$y ) , lat1 = max( x$y ) )
# see ?mapproject for a zillion alternatives
	
# if you like that projection, store it in the map object.
eu.map <- 
	eu.map + coord_map( project = "albers" , lat0 = min( x$y ) , lat1 = max( x$y ) )


# check out some purty colors.
eu.map + scale_colour_gradient( low = 'green' , high = 'red' )

eu.map + scale_colour_gradient( low = 'white' , high = 'blue' )

eu.map + scale_colour_gradient( low = muted( 'blue' ) , high = muted( 'red' ) )

# clear up RAM
rm( eu.map ) ; gc()

# # end of step 5 # #
# # # # # # # # # # #


# # # # # # # # # # #
# # step 6: krige # #

# note that because there are less than five hundred points
# that we're kriging across, smaller computers can handle
# this particular kriged regression without knot tying
nrow( x )


# interpolation option one #
library(fields)

krig.fit <-
	Krig(
		cbind( x$x , x$y ) ,
		x$tvtot ,
		weights = x$weight
	)

# that is: what is the (weighted) relationship between
# your variable of interest (hours of television) and
# the x/y points on a grid?

# check this out!
surface( krig.fit )
# lookin' good.  but this includes siberia


# # end of step 6 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # #
# # step 7: make a grid and predict # #

library(raster)

x.range <- bb[ 1 , ]
y.range <- bb[ 2 , ]

# add five percent on each side
x.diff <- abs( x.range[ 2 ] - x.range[ 1 ] ) * 0.05
y.diff <- abs( y.range[ 2 ] - y.range[ 1 ] ) * 0.05

x.range[ 1 ] <- x.range[ 1 ] - x.diff
x.range[ 2 ] <- x.range[ 2 ] + x.diff
y.range[ 1 ] <- y.range[ 1 ] - y.diff
y.range[ 2 ] <- y.range[ 2 ] + y.diff

# choose the number of ticks (in each direction) on your grid
grid.length <- 400
# grid.length <- 600
# # note: smaller grids will render much much faster
# # (so they're better if you're just playing around)
# # but larger grids will prevent your final plot from
# # being too pixelated, even when zooming in.
# # anything beyond a 400 x 400 grid might
# # overload the RAM capacity of small computers.


# create some grid data.frame objects, one for each interpolation type
grd <- krig.grd <-
	expand.grid(
		x = seq( from = x.range[1] , to = x.range[2] , length = grid.length ) , 
		y = seq( from = y.range[1] , to = y.range[2] , length = grid.length )
	)


# along your rectangular grid,
# what are the predicted values of
# television viewership hours?

# loop through this prediction to conserve RAM
for ( i in split( seq( nrow( grd ) ) , ceiling( seq( nrow( grd ) ) / 20000 ) ) ){
	krig.grd[ i , 'kout' ] <- predict( krig.fit , krig.grd[ i , c( 'x' , 'y' ) ] )
	gc()
}

# note: alternative prediction methods fare poorly on such a large surface,
# at least for me.  but i'd love to be proven wrong.  thanx


# # end of step 7 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # #
# # step 8: ggplot and choose options # #

library(ggplot2)

# # # psa # # # 
# capping your outliers might drastically change your map.
# if you find the 25th percentile and 75th percentile with
summary( krig.grd$kout )
# and then replace all `kout` values below the 25th or above the 75th
# with those capped percentile endpoints, i promise promise promise
# your maps will appear quite different.  you could cap at the 25th and 75th with..
# grd.sum <- summary( krig.grd$kout )
# krig.grd[ krig.grd$kout > grd.sum[ 5 ] , 'kout' ] <- grd.sum[ 5 ]
# krig.grd[ krig.grd$kout < grd.sum[ 2 ] , 'kout' ] <- grd.sum[ 2 ]
# # # end # # # 

# you don't want to cap at the 25th and 75th?
# well consider one other idea: at least cap at the 5th and 95th
# this will also increase the visible gradient ultimately plotted.

# if a numeric vector has values below the 5th percentile or above the 95th percentile, cap 'em
minnmax.at.0595 <- 
	function( z ){ 
		q0595 <- quantile( z , c( 0.05 , 0.95 ) )
		z[ z < q0595[ 1 ] ] <- q0595[ 1 ]
		z[ z > q0595[ 2 ] ] <- q0595[ 2 ]
		z
	}

# min and max all numeric values.  this makes the gradient much more visible.
krig.grd$kout <- minnmax.at.0595( krig.grd$kout )
# it also (unfairly perhaps) amplifies the differences between points


# initiate the krige-based plot
krg.plot <- 
	ggplot( data = krig.grd , aes( x = x , y = y ) ) +
	geom_point( data = krig.grd , aes( color = kout ) )
	

# view the grid
krg.plot


# initiate the entire plot
the.plot <-

	krg.plot +
	
	# blank out the legend and axis labels
	theme(
		legend.position = "none" ,
		axis.title.x = element_blank() ,
		axis.title.y = element_blank()		
	) + 
	
	# blank out other plot elements
	scale_x_continuous( limits = bb[ 1 , ] , breaks = NULL , oob = squish ) +
    scale_y_continuous( limits = bb[ 2 , ] , breaks = NULL , oob = squish ) +
	theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		panel.background = element_blank(),
		panel.border = element_blank(),
		axis.ticks = element_blank()
	)

# print the plot to the screen
the.plot

# # end of step 8 # #
# # # # # # # # # # #


# # # # # # # # # # #
# # step 9: blank # #

library(SpatialTools)
library(plyr)

# you have the krigged grid
# you have the map of world country borders
# you have a few maps of regions that you actually have data for

# everything outside country borders is water.
# everything inside country borders but outside of regions with data should be "missing"


# we had previously constructed the bounding box
# with `matnci.shp` plus ukraine.  but we now need
# albania, kosovo, and russia added to that shape.

# standardize the projections of all shapes
# but for russia, we only need RU11 - RU15
rusub <- subset( rusnc , NUTS_ID %in% c( 'RU11' , 'RU12' , 'RU13' , 'RU14' , 'RU15' ) )
rus.ll <- spTransform( rusub , CRS( "+proj=longlat" ) )
alb.ll <- spTransform( alb , CRS( "+proj=longlat" ) )
kos.ll <- spTransform( kos , CRS( "+proj=longlat" ) )

# keep only the NUTS identifier
rus.ll@data <- rus.ll@data[ "NUTS_ID" ]
alb.ll@data <- alb.ll@data[ "NUTS_ID" ]
kos.ll@data <- kos.ll@data[ "NUTS_ID" ]

# avoid conflicting identifiers
rus.ll <- spChFIDs( rus.ll , paste0( 'rus_' , sapply( slot( rus.ll , "polygons" ) , slot , "ID" ) ) )
alb.ll <- spChFIDs( alb.ll , paste0( 'alb_' , sapply( slot( alb.ll , "polygons" ) , slot , "ID" ) ) )
kos.ll <- spChFIDs( kos.ll , paste0( 'kos_' , sapply( slot( kos.ll , "polygons" ) , slot , "ID" ) ) )

# stack 'em alongside what's already in `ess.shp`
data.shp <- rbind( ess.shp , rus.ll )
data.shp <- rbind( data.shp , alb.ll )
data.shp <- rbind( data.shp , kos.ll )

# here are the landmasses where
# ess provides some data
plot( data.shp )

# note that some of these regional geographies self-intersect.
# quickly fix this with gBuffer if you don't care too much about perfect coastlines
data.shp <- gBuffer( data.shp , width = 0 )

# draw a huge rectangle to make sure you encircle russia's european side
ess.shp.blank <- as( 2 * extent( ess.shp ), "SpatialPolygons" )

# compute the difference between regions with data and
# the huge rectangle beyond our bounding box borders
ess.shp.diff <- gDifference( ess.shp.blank , data.shp )

# here's where you have data
plot( ess.shp , col = 'red' )
plot( data.shp , col = 'red' , add = TRUE )
# here's where you have no data
plot( ess.shp.diff , add = TRUE , col = 'gray' )


# beyond that, you need a water layer.

# find the centroid of germany
germany.centroid <- data.frame( gCentroid( subset( world.shp , CNTR_ID == 'DE' ) ) )

# find every centroid of every country worldwide
all.centroids <- data.frame( gCentroid( world.shp , byid = TRUE ) )

# find each distance to the centroid of germany
all.distances <- as.numeric( dist2( as.matrix( germany.centroid ) , as.matrix( all.centroids ) ) )

# subset the world shape to only *countries near germany*
cng <- world.shp[ ( all.distances < 60 ) , ]

# save and transform this result
cng@data <- cng@data[ "CNTR_ID" ]
cng <- spTransform( cng , CRS( "+proj=longlat" ) )

# manually tack on russia, since its centroid isn't close to germany
# only use the western regions
rus <- rusub

# align the russian shapefile with the `cng` one
rus@data$CNTR_ID <- 'RU'
rus@data <- rus@data[ "CNTR_ID" ]
rus <- spTransform( rus , CRS( "+proj=longlat" ) )

# prevent unique ids from conflicting
rus <- spChFIDs( rus , paste0( 'rus_' , sapply( slot( rus , "polygons" ) , slot , "ID" ) ) )

# bind these two shapefiles together
cngpr <- rbind( cng , rus )

# combine the country lines
cngpr <- gBuffer( cngpr , width = 0 )

# construct a shape of all water near the european continent
water.shp <- gDifference( ess.shp.blank , cngpr )


# here's where you have data
plot( ess.shp , col = 'red' , 	xlim = bb[ 1 , ] , ylim = bb[ 2 , ] )
plot( data.shp , col = 'red' , add = TRUE )
# here's where you have no data
plot( ess.shp.diff , add = TRUE , col = 'gray' )
# here's where there's water
plot( water.shp , add = TRUE , col = 'blue' )

# prepare the difference layer for ggplot2
water <- fortify( water.shp )

# fix the islands
water2 <- ddply( water , .(piece) , function(x) rbind( x , water[ 1 , ] ) )

# blank out coastal areas
water.layer <- 
	geom_polygon( 
		data = water2 , 
		aes( x = long , y = lat , group = id ) , 
		fill = 'white' 
	)

# closer, eh?
the.plot + water.layer

# prepare the missing data layer for ggplot2
nodata <- fortify( ess.shp.diff )

# fix the islands
nodata2 <- ddply( nodata , .(piece) , function(x) rbind( x , nodata[ 1 , ] ) )

nodata.layer <- 
	geom_polygon( 
		data = nodata2 , 
		aes( x = long , y = lat , group = id ) , 
		fill = 'grey50' 
	)

# eeeeven closer.
the.plot + nodata.layer + water.layer

# prepare the world shapefile for ggplot2
borders <- fortify( world.shp )

# create a international border line around each nation
border.layer <- 
	geom_path( 
		data = borders , 
		aes( x = long , y = lat , group = group ) 
	)

# lookin' good.
the.plot + nodata.layer + water.layer + border.layer

# save our progress so far
the.plot <- the.plot + nodata.layer + water.layer + border.layer

# last shape.  add a white rectangle around the outside border
the.plot + geom_rect( xmin = bb[ 1 , 1 ] , xmax = bb[ 1 , 2 ] , ymin = bb[ 2 , 1 ] , ymax = bb[ 2 , 2 ] , color = 'white' , fill = NA , size = 4 )
# see that?  the weird edge of the grid has been blanked out.

# hold on to that layer too.
the.plot <- the.plot + geom_rect( xmin = bb[ 1 , 1 ] , xmax = bb[ 1 , 2 ] , ymin = bb[ 2 , 1 ] , ymax = bb[ 2 , 2 ] , color = 'white' , fill = NA , size = 4 )


# # end of step 9 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # #
# # step 10: color, project, and save # #

library(ggplot2)
library(scales)
library(raster)
library(plyr)
library(rgeos)
library(RColorBrewer)
library(mapproj)
library(colorRamps)

# check out some purty colors.

# from http://colorbrewer2.org/

# two color schemes
Oranges.3.p <- colorRampPalette( brewer.pal( 3 , "Oranges" ) )
Oranges.9.p <- colorRampPalette( brewer.pal( 9 , "Oranges" ) )

the.plot + scale_color_gradientn( colours = Oranges.3.p( 100 ) )
the.plot + scale_color_gradientn( colours = Oranges.9.p( 100 ) )
the.plot + scale_color_gradient( low = "#56B1F7" , high = "#132B43" )

# alternatively, here's a color scheme from blue to red.
plot( 1:70 , rep( 1 , 70 ) , col = rainbow( 100 )[ 70:1 ] , pch = 16 , cex=3 )

# or a color scheme from dark blue to dark red
plot( 1:100 , rep( 1 , 100 ) , col = matlab.like2( 100 ) , pch = 16 , cex=3 )

# want to see both of these gradients plotted?
the.plot + scale_color_gradientn( colours = rainbow( 100 )[ 70:1 ] )
the.plot + scale_color_gradientn( colours = matlab.like2( 100 ) )

# ooh i like that one mom, can we keep it can we keep it?
final.plot <- the.plot + scale_color_gradientn( colours = matlab.like2( 100 ) )

# here's the final plot
final.plot

# save the file to your current working directory
ggsave( 
	"2012 average hours of television - unprojected.png" ,
	plot = final.plot
)
# but that's unprojected.  you might prefer a projected map.

# # # pick your projection # # #

# this is as good of a time as any to do it.
matnci.gg <- fortify( matnci.shp )

qp <- qplot( long , lat , data = matnci.gg , geom = 'path' , group = group )

# unprojected
qp

# here are lots of choices.  choose wisely.
qp + coord_map( project = "albers" , lat0 = bb[ 2 , 1 ] , lat1 = bb[ 2 , 2 ] )
qp + coord_fixed()
qp + coord_cartesian()
qp + coord_map( "gilbert" )
qp + coord_map( "lagrange" )
qp + coord_map( "stereographic" )


# choose a projection.  i prefer stereographic for europe, but any work just fine.
co <- coord_map( "stereographic" )
# printing the projected plot takes much more time than printing the unprojected one


# project this pup
projected.plot <- final.plot + co

# would you like to save this game?

# # # fair warning # # #
# the projected map takes hours to render.
# choose carefully from the shapes above,
# then leave this save command running overnight.

# save the projected plot, which takes longer doesn't it.
ggsave( 
	"2012 average hours of television - projected.png" ,
	plot = projected.plot
)

# # end of step ten # #
# # # # # # # # # # # #
