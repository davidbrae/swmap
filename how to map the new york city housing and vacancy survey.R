# # # # # # # # # # # # # # # # #
# # set the working directory # #
# # # # # # # # # # # # # # # # #

# setwd( "C:/My Directory/SWMAP/" )


# # # # # # # # # # # # # # # #
# # example survey data set # #
# # # # # # # # # # # # # # # #

# new york city housing and vacancy survey


# # # # # # # # # # # # # # # # # # # # #
# # different from other maps because # #
# # # # # # # # # # # # # # # # # # # # #

# incorporates shapefiles with more than one starting projection
# binds multiple shapefiles (not layers) to blank out water areas


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


# # # # # # #
# # flaws # #
# # # # # # #

# the standard errors calculated from this data set are incorrect, so
# the weights used here are even less scientific than in the other maps.


# # # # # # # # # # # # # # # # # # # # #
# # step 1: load the survey microdata # #

library(downloader)

# download the 2011 new york city housing and vacancy survey microdata onto the local disk
years.to.download <- 2011
source_url( "https://raw.github.com/ajdamico/usgsd/master/New%20York%20City%20Housing%20and%20Vacancy%20Survey/download%20all%20microdata.R" , prompt = FALSE , echo = TRUE )

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

# load the download_cached and related functions
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


download_cached( 
	"ftp://ftp2.census.gov/census_2010/04-Summary_File_1/New_York/ny2010.sf1.zip" ,
	sf1ny.tf ,
	mode = 'wb'
)
# note: to re-download a file from scratch, add the parameter usecache = FALSE


# unzip the summary file #1 files
sf1ny.uz <- unzip( sf1ny.tf , exdir = tempdir() )


# file layout from http://www.census.gov/prod/cen2010/doc/sf1.pdf#page=18
sf1ny <- 
	read.fwf( 
		sf1ny.uz[ grep( "nygeo2010" , sf1ny.uz ) ] , 
		c( -8 , 3 , -16 , 2 , 3 , -4 , 5 , -4 , 5 , -4 , 6 , 1 , 4 , -106 , 5 , -142 , 9 , -9 , 11 , 12 ) 
	)

# add columns names matching the census bureau, so it's easy to read
names( sf1ny ) <- 
	c( "sumlev" , "state" , "county" , "cousub" , "place" , "tract" , "blkgrp" , "block" , "zcta5" , "pop100" , "intptlat" , "intptlon" )

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

library(maptools)
library(ggplot2)
library(rgdal)
library(RColorBrewer)
library(scales)

# before you ever touch surface smoothing or kriging,
# make some decisions about how you generally want
# your map to look:  the projection and coloring

# the options below simply use hadley wickham's ggplot2
# with the census block-level poverty rates and centroids


# initiate a simple map
nyc.map <- 
	ggplot( data = x , aes( x = intptlon , y = intptlat ) ) +
	geom_point( data = x , aes( colour = pproom ) )

# remove all map crap.
nyc.map <- 
	nyc.map + 
	
	xlab( "" ) + ylab( "" ) +

	# force the x and y axis limits at the shape of the city and don't do anything special for off-map values
	scale_x_continuous( limits = c( min( x$intptlon ) , max( x$intptlon ) ) , breaks = NULL , oob = squish ) +
	# since we're going to add lots of surrounding-area detail!
    scale_y_continuous( limits = c( min( x$intptlat ) , max( x$intptlat ) ) , breaks = NULL , oob = squish ) +

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


# check out some purty colors.

# from http://colorbrewer2.org/

# three sequential color schemes
YlOrBr.3.p <- colorRampPalette( brewer.pal( 3 , "YlOrBr" ) )
YlOrBr.9.p <- colorRampPalette( brewer.pal( 9 , "YlOrBr" ) )
Purples.9.p <- colorRampPalette( brewer.pal( 9 , "Purples" ) )

# one diverging color schemes
RdGy.11.p <- colorRampPalette( rev( brewer.pal( 11 , "RdGy" ) ) )

# here's what the map looks like in black n white.
nyc.map + scale_colour_gradient( low = 'white' , high = 'black' )

# or one of the colorbrewer schemes
nyc.map + scale_colour_gradientn( colours = YlOrBr.3.p( 100 ) )
nyc.map + scale_colour_gradientn( colours = YlOrBr.9.p( 100 ) )
nyc.map + scale_colour_gradientn( colours = RdGy.11.p( 100 ) )
nyc.map + scale_colour_gradientn( colours = Purples.9.p( 100 ) )

# download and read-in the new york city parks shapefile
tf <- tempfile()

download_cached( "http://www.nyc.gov/html/dpr/nycbigapps/DPR_Parks_001.zip" , tf )

z <- unzip( tf , exdir = tempdir() )

sfname <- z[ grep( 'shp$' , z ) ]

# new york city's shapefiles are not in longlat projection format, so
# use `readOGR` instead of `readShapePoly` here to capture the map projection
parks.shp <- readOGR( sfname  , layer = gsub( "\\.shp" , "" , basename( sfname ) ) )

# convert the shapefile to longlat (which matches the us census bureau's summary file #1)
parks.shp <- spTransform( parks.shp , CRS( "+proj=longlat" ) )
# now `parks.shp` will overlay properly with us census bureau files.

# prepare the parks shapefile for ggplot2
parks <- fortify( parks.shp )

# the `parks.shp` has a weird circle in the upper-left.
parks <- subset( parks , long > -75 )
# hack it off by removing all points to the west of -75 longitude

# create a dark green layer of parks in new york city
park.layer <- 
	geom_polygon( 
		data = parks , 
		aes( x = long , y = lat , group = group ) , 
		fill = '#abdda4' 
	)

# that's what the map looks like so far..
nyc.map + park.layer

# ..save it if you like.
nyc.map <- nyc.map + park.layer

# oh sorry, if you'd like black & white again, add this segment.
nyc.map + scale_colour_gradient( low = 'white' , high = 'black' )

# download and read-in the new york city clipped-to-shoreline borough map
download_cached( "http://www.nyc.gov/html/dcp/download/bytes/nybb_14c.zip" , tf )
# from http://www.nyc.gov/html/dcp/html/bytes/districts_download_metadata.shtml

z <- unzip( tf , exdir = tempdir() )

sfname <- z[ grep( 'shp$' , z ) ]

# once again, new york city's shapefiles are not in longlat projection format, so
# use `readOGR` instead of `readShapePoly` here to capture the map projection
boro.shp <- readOGR( sfname  , layer = gsub( "\\.shp" , "" , basename( sfname ) ) )

# convert the shapefile to longlat (which matches the us census bureau's summary file #1)
boro.shp <- spTransform( boro.shp , CRS( "+proj=longlat" ) )
# now `boro.shp` will overlay properly with us census bureau files.

# prepare the boro shapefile for ggplot2
boro <- fortify( boro.shp )

# create a borough-border around the city
boro.layer <- 
	geom_path( 
		data = boro , 
		aes( x = long , y = lat , group = group ) 
	)

# and would you look at that?
nyc.map + boro.layer
# some of the city parks are in the ocean.  we'll have to snip off those later.

# let's save all of those attributes, including the coloring
nyc.map <- nyc.map + boro.layer + scale_colour_gradient( low = 'white' , high = 'black' )

# and finally, try some projections!

# which of these do you prefer?
nyc.map + coord_map( "newyorker" , r = 0 )
nyc.map + coord_map( "newyorker" , r = 10 )
nyc.map + coord_map( "newyorker" , r = 20 )
nyc.map + coord_map( "newyorker" , r = 30 )
nyc.map + coord_map( "newyorker" , r = 40 )

# meh, i like the map better without any.
nyc.map

# # end of step 5 # #
# # # # # # # # # # #


# # # # # # # # # # # #
# # step 6: outline # #

library(raster)
library(rgdal)
library(RCurl)

# new york city has lots of water areas within the city limits
# instead of the regular census bureau tiger shapefiles,
# the city government provides the cleanest clipped shapefile.

# draw a rectangle 5% bigger than the city limits
boro.shp.out <- as( 1.1 * extent( boro.shp ), "SpatialPolygons" )

# get the directory listing of the folder with all water layers from census
water.ftp <- 'ftp://ftp2.census.gov/geo/tiger/TIGER2013/AREAWATER/'

water.files <- 
	paste0(
		water.ftp , 
		strsplit( 
		getURL( 
			water.ftp , 
			ftp.use.epsv = FALSE , 
			ftplistonly = TRUE
		) , 
		'\\s+' )[[ 1 ]]
	)

# limit these files to new york, new jersey, and connecticut
# you can get more counties than you need, the program will just run a bit slower.
nynjct.water <- 
	water.files[ 
		# bergen, hudson, essex, passaic, morris, somerset, union, middlesex, monmouth in new jersey, or
		substr( water.files , 61 , 65 ) %in% c( '34003' , '34017' , '34013' , '34031' , '34027' , '34035' , '34039' , '34023' , '34025' ) |
		
		# rockland, westchester, nassau, suffolk, orange, putnam in new york state
		substr( water.files , 61 , 65 ) %in% c( '36087' , '36119' , '36059' , '36103' , '36071' , '36079' ) |
		
		# fairfield in connecticut
		substr( water.files , 61 , 65 ) %in% c( '09001' )
	]

# download and extract to a temporary directory
invisible( sapply( nynjct.water , function( x ) {
	path <- file.path( tempdir() , basename( x ) )
	download_cached( x , destfile = path , mode = 'wb' )
	unzip( path , exdir = file.path( tempdir() , 'watershps' ) )
} ) )

# read in all shps, and prepend shapefile name to identifiers
shps <- lapply( sub( '\\.zip' , '' , basename( nynjct.water ) ) , function( x ) {
	shp <- readOGR( file.path( tempdir() , 'watershps' ) , x )
	shp <- spChFIDs( shp , paste0( x , '_' , sapply( slot( shp , "polygons" ) , slot , "ID" ) ) )
	shp
})
# this step above removes potential duplicate identifiers

# rbind to a single object
water.shp <- do.call( rbind , as.list( shps ) )

# want to see all of the little waters of the region that can now be blanked out?
plot( water.shp )
# these water layers do not come with the city's shapefile, so construct them independently.

# prepare the surrounding region shapefile for ggplot2
water <- fortify( water.shp )


lightblue.water.layer <-
	geom_polygon( 
		data = water , 
		aes( x = long , y = lat , group = group ) , 
		fill = 'lightblue' 
	)

# here's the surrounding area water in light blue so you can see it..
nyc.map + lightblue.water.layer

# ..but we actually want to blank this on the final map,
# so make it white on the layer that gets saved
water.layer <- 
	geom_polygon( 
		data = water , 
		aes( x = long , y = lat , group = group ) , 
		fill = 'white' 
	)

	
# outline the surrounding landmasses as well
ccbf.tf <- tempfile()

# new york city borders the ocean,
# so use the census bureau's cartographic boundary files
# instead of the regular tiger shapefiles

download_cached( 
	"http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_county_500k.zip" ,
	ccbf.tf ,
	mode = 'wb'
)

ccbf.uz <- unzip( ccbf.tf , exdir = tempdir() )

ccbf.shp <- readShapePoly( ccbf.uz[ grep( 'shp$' , ccbf.uz ) ] )

nynjct.shp <- 
	subset( 
		ccbf.shp , 
		GEOID %in% 
			c( '34003' , '34017' , '34013' , '34031' , '34027' , '34035' , '34039' , '34023' , '34025' ,
				'36087' , '36119' , '36059' , '36103' , '36071' , '36079' ,
				'09001'
			)
	)
	
# prepare the surrounding region shapefile for ggplot2
nynjct <- fortify( nynjct.shp )

# gray out surrounding landmasses
nynjct.layer <- 
	geom_polygon( 
		data = nynjct , 
		aes( x = long , y = lat , group = group ) , 
		fill = 'lightgray' 
	)

# and would you look at that?
nyc.map + nynjct.layer
# no?  we will re-order the layers later.
# but now you've got a blank-gray of the surrounding areas.

# put it all together, what have you got?
nyc.map + nynjct.layer + lightblue.water.layer
# hey okay.  that looks like crap but we can work with it.

	
# # end of step 6 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # #
# # step 7: tie knots and krige # #

library(sqldf)

# how many knots should you make? #

# knots are the computationally-intensive part of this process,
# choose as many as your computer and your patience can handle.

# you should aim for between 100 - 999 knots,
# but numbers closer to 1,000 will overload smaller computers

# with census microdata, you've often already got easy access to a relevant geographic grouping
# however for new york city, most of the groupings are either too big to be useful as knots,
# or too small that they'll overload smaller computers.


# the city of new york contains
nrow( unique( sf1.bo[ , c( 'boroname' , 'neighborhood' ) ] ) )
# subboro areas, which is too small of a number of knots.

# the city also has
nrow( unique( sf1.bo[ , c( 'boroname' , 'tract' ) ] ) )
# census tracts, which will overload most computers

# county subdivision and place codes do not exist within the city limits.
nrow( unique( sf1.bo[ , c( 'boroname' , 'cousub' , 'place' ) ] ) )

# so be creative.  take another look at the summary file #1 documentation
# http://www.census.gov/prod/cen2010/doc/sf1.pdf#page=18
# oh hayyyy how about zip code tabulation areas?  that could work.
nrow( unique( sf1.bo[ , c( 'boroname' , 'zcta5' ) ] ) )
# nope, too many for a computer with 3gb of ram.

# if you have a powerful computer, you might try tying knots at the census tract-level
# otherwise, the neighborhods defined by the new york city housing and vacancy survey work.

# `sqldf` does not like periods in the data.frame name
# # sf1_bo <- sf1.bo

# within each borough x zcta5,
# calculate the population-weighted mean of the coordinates
# and (for smoothing) the weighted share at each borough-zcta5 centroid
# # nyc.knots <- 
	# # sqldf( 
		# # "select 
			# # boroname , zcta5 ,
			# # sum( pop100 ) as pop100 , 
			# # sum( pop100 * intptlon ) / sum( pop100 ) as intptlon ,
			# # sum( pop100 * intptlat ) / sum( pop100 ) as intptlat
		# # from sf1_bo
		# # group by
			# # boroname , zcta5"
	# # )
# note: this screws up coordinates that cross the international date line
# or the equator.  in the united states, only alaska's aleutian islands do this
# and those geographies will be thrown out later.  so it doesn't matter.

# clear up RAM
rm( sf1.bo ) ; gc()


# interpolation option one #
library(fields)

# instead, we can let the `fields` package attempt to guess knots for you,
xknots <- cover.design( cbind( x$intptlon , x$intptlat ) , 200 )$design
# # # # note: tying 200 knots instead of 100 knots takes about an hour longer
# # # # but the final map looks a bit better.  if you're just passing through, use 100.

# you can look at the estimated knots
plot( xknots )

krig.fit <-
	Krig(
		cbind( x$intptlon , x$intptlat ) ,
		x$pproom ,
		weights = x$weight ,
		knots = xknots
		# if you computed the knots yourself, you'll need this knots= line instead:
		# knots = cbind( nyc.knots$intptlon , nyc.knots$intptlat )
	)

# that is: what is the (weighted) relationship between
# your variable of interest (persons per room) and
# the x/y points on a grid?

# check this out!
surface( krig.fit )
# you're almost there!


# interpolation option two #
library(mgcv)

gam.fit <- 
	gam( 
		pproom ~ s(intptlon , intptlat ) , 
		weights = weight , 
		data = x
	)
	

# for the third alternative, keep reading.
	
	
# # end of step 7 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # #
# # step 8: make a grid and predict # #

library(raster)

x.range <- bbox( boro.shp.out )[ 1 , ]
y.range <- bbox( boro.shp.out )[ 2 , ]

# add five percent on each side
x.diff <- abs( x.range[ 2 ] - x.range[ 1 ] ) * 0.05
y.diff <- abs( y.range[ 2 ] - y.range[ 1 ] ) * 0.05

x.range[ 1 ] <- x.range[ 1 ] - x.diff
x.range[ 2 ] <- x.range[ 2 ] + x.diff
y.range[ 1 ] <- y.range[ 1 ] - y.diff
y.range[ 2 ] <- y.range[ 2 ] + y.diff

# choose the number of ticks (in each direction) on your grid
grid.length <- 500
# # note: smaller grids will render faster
# # (so they're better if you're just playing around)
# # but larger grids will prevent your final plot from
# # being too pixelated, even when zooming in


# create some grid data.frame objects, one for each interpolation type
grd <- gam.grd <- krig.grd <-
	expand.grid(
		intptlon = seq( from = x.range[1] , to = x.range[2] , length = grid.length ) , 
		intptlat = seq( from = y.range[1] , to = y.range[2] , length = grid.length )
	)


# along your rectangular grid,
# what are the predicted values of
# the number of persons per room?
krig.grd$kout <- predict( krig.fit , krig.grd )

# alternate grid using gam.fit
gam.grd$gamout <- predict( gam.fit , gam.grd )

# interpolation option three #
library(spatstat)

smoout <- 
	Smooth(
		ppp( 
			x$intptlon , 
			x$intptlat , 
			x.range ,
			y.range ,
			marks = x$pproom
		) ,
		# here's a good starting point for sigma, but screw around with this value.
		sigma = 0.05 ,
		weights = x$weight
	)

smoo.grd <-	
	expand.grid(
		intptlon = seq( from = smoout$xrange[1] , to = smoout$xrange[2] , length = smoout$dim[1] ) , 
        intptlat = seq( from = smoout$yrange[1] , to = smoout$yrange[2] , length = smoout$dim[2] )
	)

smoo.grd$smoout <- as.numeric( t( smoout$v ) )

# # end of step 8 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # #
# # step 9: ggplot and choose options # #

library(ggplot2)
library(mapproj)
library(scales)


# # # psa # # # 
# capping your outliers might drastically change your map.
# if you find the 25th percentile and 75th percentile with
# summary( krig.grd$kout )
# and then replace all `kout` values below the 25th or above the 75th
# with those capped percentile endpoints, i promise promise promise
# your maps will appear quite different.  you could cap at the 25th and 75th with..
# grd.sum <- summary( krig.grd$kout )
# krig.grd[ krig.grd$kout > grd.sum[ 5 ] , 'kout' ] <- grd.sum[ 5 ]
# krig.grd[ krig.grd$kout < grd.sum[ 2 ] , 'kout' ] <- grd.sum[ 2 ]
# # # end # # # 


# you don't want to cap at the 25th and 75th?
# well consider one other idea: at least cap at the 5th and 95th of the nation
# this will also increase the visible gradient ultimately plotted.

# for example, the lowest krigged value is negative.
summary( krig.grd$kout )
# that's obviously not right.

# if a numeric vector has values below the 5th percentile or above the 75th percentile, cap 'em
minnmax.at.0595 <- 
	function( z ){ 
		q0595 <- quantile( z , c( 0.05 , 0.95 ) )
		z[ z < q0595[ 1 ] ] <- q0595[ 1 ]
		z[ z > q0595[ 2 ] ] <- q0595[ 2 ]
		z
	}

# min and max all numeric values.
# krig.grd$kout <- minnmax.at.0595( krig.grd$kout )
# gam.grd$gamout <- minnmax.at.0595( gam.grd$gamout )
# smoo.grd$smoout <- minnmax.at.0595( smoo.grd$smoout )
# sometimes this makes the gradient much more visible.
# but for this statistic, it doesn't do much.

# initiate the krige-based plot
krg.plot <- 
	ggplot( data = krig.grd , aes( x = intptlon , y = intptlat ) ) +
	geom_tile( data = krig.grd , aes( fill = kout ) )
	
# initiate the gam-based plot
gam.plot <- 
	ggplot( data = gam.grd , aes( x = intptlon , y = intptlat ) ) +
	geom_tile( data = gam.grd , aes( fill = gamout ) )

# initiate the smooth-based plot
smooth.plot <- 
	ggplot( data = smoo.grd , aes( x = intptlon , y = intptlat ) ) +
	geom_tile( data = smoo.grd , aes( fill = smoout ) )

# view all three grids!
krg.plot
gam.plot
smooth.plot


# initiate the entire plot
the.plot <-

	# choose only one of the three interpolation grids
	krg.plot +
	# gam.plot +
	# smooth.plot +
	
	# blank out the legend and axis labels
	theme(
		legend.position = "none" ,
		axis.title.x = element_blank() ,
		axis.title.y = element_blank()		
	) + 
	
	xlab( "" ) + ylab( "" ) +

	# force the x and y axis limits at the shape of the city and don't do anything special for off-map values
	scale_x_continuous( limits = c( min( grd$intptlon ) , max( grd$intptlon ) ) , breaks = NULL , oob = squish ) +
	# since we're going to add lots of surrounding-area detail!
    scale_y_continuous( limits = c( min( grd$intptlat ) , max( grd$intptlat ) ) , breaks = NULL , oob = squish ) +

	theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		panel.background = element_blank(),
		panel.border = element_blank(),
		axis.ticks = element_blank()
	)

# print the plot to the screen
the.plot
# this is the bottom layer.

# next the park layer, don't forget about the park layer!
the.plot + park.layer

# are you alright with saving that?  save it.
the.plot <- the.plot + park.layer

# # end of step 9 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # #
# # step 10: blank, color, save # #

library(ggplot2)
library(scales)
library(raster)
library(plyr)
library(rgeos)

# draw a rectangle 15% bigger than the original city
boro.shp.blank <- as( 1.3 * extent( boro.shp ), "SpatialPolygons" )

# compute the difference between new york city and the rectangle 15% beyond the borders
boro.shp.diff <- gDifference( boro.shp.blank , boro.shp )

# prepare the difference layer for ggplot2
outside <- fortify( boro.shp.diff )

# fix any weird island polygons
outside2 <- ddply( outside , .(piece) , function(x) rbind( x , outside[ 1 , ] ) )

# blank out waterways and coastal areas
blank.layer <- 
	geom_polygon( 
		data = outside2 , 
		aes( x = long , y = lat , group = id ) , 
		fill = 'white' 
	)

# closer, eh?
the.plot + blank.layer
# that blanks out absolutely everything outside of the city limits

# store this plot
the.plot <- the.plot + blank.layer	

# but actually, we should add back in the landmasses.  like this.
the.plot + nynjct.layer + water.layer

# store this plot
the.plot <- the.plot + nynjct.layer + water.layer

# plus the borough outlines.  do you want to outline the boroughs?
the.plot + boro.layer

# store this layer on top of everything.
the.plot <- the.plot + boro.layer

# print with the same purty colors
the.plot + scale_fill_gradient( low = 'white' , high = 'black' )
the.plot + scale_fill_gradientn( colours = RdGy.11.p( 100 ) )
the.plot + scale_fill_gradientn( colours = Purples.9.p( 100 ) )
the.plot + scale_fill_gradientn( colours = YlOrBr.3.p( 100 ) )
the.plot + scale_fill_gradientn( colours = YlOrBr.9.p( 100 ) )

# ooh i like that one mom, can we keep it can we keep it?
final.plot <- the.plot + scale_fill_gradientn( colours = YlOrBr.9.p( 100 ) )

# here's the final plot
final.plot

# would you like to save this game?

# use cairo-png as your bitmap type
options( bitmapType = "cairo" )

# save the file to your current working directory
ggsave( 
	"2011 new york city number of persons per room.png" ,
	plot = final.plot ,
	scale = 2 ,
	type = "cairo-png" 
)
# happy?

# save a silly globular-projected file to your current working directory,
ggsave( 
	"2011 new york city number of persons per room - globular.png" ,
	plot = final.plot + coord_map( "globular" , orientation = c( 40.55 , -74.13 , -30 )  ) ,
	scale = 2 ,
	type = "cairo-png" 
)
# in case you're into that kinda stuff.

# # end of step ten # #
# # # # # # # # # # # #
