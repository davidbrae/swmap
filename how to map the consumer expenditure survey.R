# # # # # # # # # # # # # # # # #
# # set the working directory # #
# # # # # # # # # # # # # # # # #

# setwd( "C:/My Directory/SWMAP/" )


# # # # # # # # # # # # # # # #
# # example survey data set # #
# # # # # # # # # # # # # # # #

# consumer expenditure survey


# # # # # # # # # # # # # # # # # #
# # smallest level of geography # #
# # # # # # # # # # # # # # # # # #

# state, some metropolitan statistical areas


# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # asdfree.com blog post for this survey microdata # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# http://www.asdfree.com/search/label/consumer%20expenditure%20survey%20%28ce%29


# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # r code repository for setup and analysis examples # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# https://github.com/ajdamico/usgsd/tree/master/Consumer%20Expenditure%20Survey


# # # # # # # # # # # # #
# # value of interest # #
# # # # # # # # # # # # #

# transportation expenditure as a share of total expenditure


# # # # # # # # # # # # # # # # # # # # #
# # step 1: load the survey microdata # #

library(downloader)

# download the 2013 consumer expenditure survey microdata onto the local disk
years.to.download <- 2013
source_url( "https://raw.github.com/ajdamico/usgsd/master/Consumer%20Expenditure%20Survey/download%20all%20microdata.R" , prompt = FALSE , echo = TRUE )

# # end of step 1 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 2: download and import necessary geographic crosswalks # #

library(downloader)
library(sqldf)

# load the download.cache and related functions
# to prevent re-downloading of files once they've been downloaded.
source_url(
	"https://raw.github.com/ajdamico/usgsd/master/Download%20Cache/download%20cache.R" ,
	prompt = FALSE ,
	echo = FALSE
)


# create a temporary file containing the census bureau's
# 2010 census summary file #1, then download the file.
sf1.tf <- tempfile()

# initiate an empty data.frame object
# to store all summary file blocks
sf <- NULL

# download the census bureau's 2010 summary file one #

# this takes a long time.
# depending on your internet speed,
# you might need to let this loop run overnight.

# but this code will cache the downloads onto your local disk
# when you download these files once,
# they'll never need to be downloaded again.

# loop through every state and dc
for ( state.number in 1:51 ){

	# after downloading all fifty states, get dc as well
	if( state.number == 51 ){
		sn <- "District_of_Columbia"
		sa <- "dc"
	} else {
		sn <- gsub( " " , "_" , state.name[ state.number ] )
		sa <- tolower( state.abb[ state.number ] )
	}
	
	# create a single-element character string containing the ftp path
	ftp.loc <- 
		paste0( 
			"ftp://ftp2.census.gov/census_2010/04-Summary_File_1/" ,
			sn ,
			"/" ,
			sa ,
			"2010.sf1.zip"
		)

	# download the current state's summary file
	download.cache( ftp.loc , sf1.tf , mode = 'wb' )
	# note: to re-download a file from scratch, add the parameter usecache = FALSE

	# unzip the summary file #1 files
	sf1.uz <- unzip( sf1.tf , exdir = tempdir() )

	# file layout from http://www.census.gov/prod/cen2010/doc/sf1.pdf#page=18
	sf1 <- read.fwf( sf1.uz[ grep( "geo2010" , sf1.uz ) ] , c( -8 , 3 , -14 , 1 , -1 , 2 , 3 , -22 , 6 , 1 , 4 , -47 , 5 , 2 , -5 , 3 , -191 , 9 , -9 , 11 , 12 ) )

	# add columns names matching the census bureau, so it's easy to read
	names( sf1 ) <- c( "sumlev" , "region" , "state" , "county" , "tract" , "blkgrp" , "block" , "cbsa" , "cbsasc" , "csa" , "pop100" , "intptlat" , "intptlon" )

	# summary level 101 has metro areas, urban/rural status, and census blocks
	sf101 <- subset( sf1 , sumlev == "101" )

	# within each census tract x cbsa/csa combo,
	# calculate the population-weighted mean of the coordinates
	sfs <- 
		sqldf( 
			"select 
				region , state , county , tract , cbsa , cbsasc , csa , 
				count(*) as census_blocks ,
				sum( pop100 ) as pop100 , 
				sum( pop100 * intptlon ) / sum( pop100 ) as intptlon ,
				sum( pop100 * intptlat ) / sum( pop100 ) as intptlat
			from sf101
			group by
				region , state , county , tract , cbsa , cbsasc , csa" )
	# note: this screws up coordinates that cross the international date line
	# or the equator.  in the united states, only alaska's aleutian islands do this
	# and those geographies will be thrown out later.  so it doesn't matter.
	
	# the above consolidation step isn't necessary if you have a huge computer
	# and a lot of time.. but it makes all of the kriging and rendering computations
	# work much faster, and mapping at the census tract- versus census block-level
	# really doesn't make much of a damn difference.
	
	# stack these blocks in with all the other states
	sf <- rbind( sf , sfs )
	
	# remove the single-state data.frame objects and clear up RAM
	rm( sf101 , sf1 , sfs ) ; gc()
	
	# remove the unzipped files from your local disk
	file.remove( sf1.uz , sf1.tf )

}

# one record per census block in every state.  see?  same number.
tapply( sf$census_blocks , sf$state , sum )
# https://www.census.gov/geo/maps-data/data/tallies/census_block_tally.html

# and guess what?  the population by state matches as well.
tapply( sf$pop100 , sf$state , sum )
# http://en.wikipedia.org/wiki/2010_United_States_Census#State_rankings

# remove columns you actually don't need
sf <- sf[ , !( names( sf ) %in% c( 'sumlev' , 'block' , 'county' , 'region' , 'tract' , 'blkgrp' , 'cbsasc' ) ) ]

# remove records with zero population
sf <- subset( sf , pop100 > 0 )

# clear up RAM
gc()


# so now we have a data.frame object with
# one record per census block,
# and also with each of the geography-levels
# that match the consumer expenditure survey
head( sf )

# and guess what?
# we've now got the census 2010 weighted populations (field pop100)
# and also each census block's centroid latitude & longitude (fields intptlat + intptlon)

# add the consumer expenditure survey results'
# geographic identifiers to the census block data.frame

# # align psu variables # #

# note: the primary sampling units available in the ces microdata
# http://www.bls.gov/cex/2013/csxintvwdata.pdf#page=9
# do not perfectly map to combined statistical areas or
# core-based statistical areas, so
# (1) match the geographies that might be matchable
# (2) combine los angeles in both data.frame objects
# (3) make all other records 9999
sf <-
	transform(
		sf ,
		
		psu =
			# new york, ny
			ifelse( state %in% 36 & csa %in% 408 , 1109 ,
			# new york, ct
			ifelse( state %in% 9 & csa %in% 408 , 1110 ,
			# new york, nj
			ifelse( state %in% 34 & csa %in% 408 , 1111 ,
			# philadelphia
			ifelse( csa %in% 428 , 1102 ,
			# boston
			ifelse( csa %in% 148 , 1103 ,
			# chicago
			ifelse( csa %in% 176 , 1207 ,
			# detroit
			ifelse( csa %in% 220 , 1208 ,
			# cleveland
			ifelse( csa %in% 184 , 1210 ,
			# minneapolis
			ifelse( csa %in% 378 , 1211 ,
			# washington dc
			ifelse( cbsa %in% 47900 , 1312 ,
			# baltimore
			ifelse( cbsa %in% 12580 , 1313 ,
			# dallas
			ifelse( csa %in% 206 , 1316 ,
			# houston
			ifelse( csa %in% 288 , 1318 ,
			# atlanta
			ifelse( csa %in% 122 , 1319 ,
			# miami
			ifelse( csa %in% 370 , 1320 ,
			# los angeles - orange
			ifelse( csa %in% 348 , 1000 ,
			# los angeles suburbs
			ifelse( csa %in% 348 , 1000 ,
			# san francisco
			ifelse( csa %in% 488 , 1422 ,
			# seattle
			ifelse( csa %in% 500 , 1423 ,
			# san diego
			ifelse( cbsa %in% 41740 , 1424 ,
			# phoenix
			ifelse( cbsa %in% 38060 , 1429 , 
		
				9999 ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
		
	)

# # align metro status variables # #
sf <- transform( sf , smsastat = ifelse( cbsa %in% 99999 , 2 , 1 ) )

# which geographies are available amongst all census blocks
sf.available.geographies <- unique( sf[ , c( 'state' , 'psu' , 'smsastat' ) ] )
# note: this really should be the universe, but it's not because
# the consumer expenditure survey's psus don't appear to perfectly map to census areas

# # end of step 2 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 3: prepare your analysis of interest at the smallest geography allowed # #

library(survey)
library(plyr)
library(stringr)

# following the analysis examples in the r code repository --
# # https://github.com/ajdamico/usgsd/blob/master/Consumer%20Expenditure%20Survey/2011%20fmly%20intrvw%20-%20analysis%20examples.R
# -- calculate the transportation share of total expenditure at the smallest available geographic area

# load all five quarters
load( "./2013/intrvw/fmli131x.rda" )
load( "./2013/intrvw/fmli132.rda" )
load( "./2013/intrvw/fmli133.rda" )
load( "./2013/intrvw/fmli134.rda" )
load( "./2013/intrvw/fmli141.rda" )

# stack all five quarters
fmly <- rbind.fill( fmli131x , fmli132 , fmli133 , fmli134 , fmli141 )

# before anything else, the los angeles suburban split isn't possible on the census side
fmly <-
	transform(
		fmly ,
		psu =
			# align with los angeles area in the `sf` object
			ifelse( psu %in% 1419:1420 , 1000 ,
			ifelse( psu == '' , 9999 , 
				as.numeric( psu ) ) )
	)

# coerce states to match `sf` as well
fmly$state <- as.numeric( fmly$state )

# set blanks to 99s
fmly[ is.na( fmly$state ) , 'state' ] <- 99

# extract available geographies
fmly.available.geographies <- unique( fmly[ , c( 'state' , 'psu' , 'smsastat' ) ] )

# merge this with the sf file's available geographies
ag <- merge( sf.available.geographies , fmly.available.geographies )

# create a flag with geographies to keep
ag$keep <- 1

# merge the available geographies back on
fmly <- merge( fmly , ag , all = TRUE )

# anyone with a missing flag needs their geography blanked out
fmly[ is.na( fmly$keep ) , 'state' ] <- 99
fmly[ is.na( fmly$keep ) , 'psu' ] <- 9999

# remove the flag
fmly$keep <- NULL

# create a character vector containing 45 variable names (wtrep01, wtrep02, ... wtrep44 and finlwt21)
wtrep <- c( paste0( "wtrep" , str_pad( 1:44 , 2 , pad = "0" ) ) , "finlwt21" )

# immediately loop through each weight column (stored in the wtrep vector)
# and overwrite all missing values (NA) with zeroes
for ( i in wtrep ) fmly[ is.na( fmly[ , i ] ) , i ] <- 0

# create a new variable in the fmly data table called 'totalexp'
# that contains the sum of the total expenditure from the current and previous quarters
fmly$totalexp <- rowSums( fmly[ , c( "totexppq" , "totexpcq" ) ] , na.rm = TRUE )

# immediately convert missing values (NA) to zeroes
fmly[ is.na( fmly$totalexp ) , "totalexp" ] <- 0

# same for transportation
fmly$transexp <- rowSums( fmly[ , c( "transpq" , "transcq" ) ] , na.rm = TRUE )
fmly[ is.na( fmly$transexp ) , "transexp" ] <- 0


# turn on replicate-weighted mean squared errors
options( survey.replicates.mse = TRUE )
# this matches the official census bureau published methods

# construct a replicate-weighted survey design object
fmly.design <-
	svrepdesign(
		repweights = "wtrep[0-9]+" ,
		weights = ~finlwt21 ,
		data = fmly
	)

	
# the family tables are no longer necessary
rm( fmly , fmli131x , fmli132 , fmli133 , fmli134 , fmli141 ) ; gc()
# remove them and clear up RAM


# calculate the 2013 nationwide ratio of transportation spending as a share of total spending
svyratio( ~ transexp , ~ totalexp , fmly.design )

# note: this is almost the same number as the bls-published 2011 share:
# http://www.bls.gov/cex/2011/share/cusize.pdf

# the smallest geography reasonably extracted
# from this survey microdata set will be
# state + psu + metro status all combined
full.table <- data.frame( svytable( ~ state + psu + smsastat , fmly.design ) )
# this crosstabulation includes too many zeroes,
# so store the result in a data.frame object
# and only print the non-zero records
subset( full.table , Freq > 0 )

# simply use both of those geographies in the by= argument
# of the `svyby` command, and re-calculate the
# transportation expenditure shares
smallest.area.statistics <-
	svyby( 
		~ transexp , 
		denominator = ~ totalexp ,
		by = ~ state + psu + smsastat ,
		fmly.design , 
		svyratio
	)
# this is the same command as the nationwide calculation above,
# except these results have been broken into smaller areas.	

# these are the statistics to be mapped
print( smallest.area.statistics )
# the standard errors are a measure of precision,
# their inverse will serve as the mapping weights

# make this object easier to type..
sas <- smallest.area.statistics

# ..and also easier to read
names( sas )[ names( sas ) == 'transexp/totalexp' ] <- 'share'
names( sas )[ names( sas ) == 'se.transexp/totalexp' ] <- 'se'

# remove objects you no longer need..
rm( fmly.design ) ; gc()
# ..and clear up RAM

# # end of step 3 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 4: merge the results of your survey analysis with the small-area geography # #

# merge the available geographies on to the census block file too
sf <- merge( sf , ag , all = TRUE )

# note that alaska and hawaii need to be manually removed,
# but some of the geographies in these areas will be collapsed
# into the 99/9999 categories.  so pre-collapse, identify them.
sf$akhi <- sf$state %in% c( 2 , 15 )

# anyone with a missing flag needs their geography blanked out
sf[ is.na( sf$keep ) , 'state' ] <- 99
sf[ is.na( sf$keep ) , 'psu' ] <- 9999

# remove the flag
sf$keep <- NULL

# continue being as sparse as possible.  remove columns you no longer need.
sf <- sf[ , ( names( sf ) %in% c( 'state' , 'psu' , 'smsastat' , 'pop100' , 'intptlat' , 'intptlon' , 'akhi' ) ) ]

# integers are overflowing
sf$pop100 <- as.numeric( sf$pop100 )

# clear up RAM
gc()

# confirm that we've created all possible geographies correctly.

# the number of records in our small area statistics..
sas.row <- nrow( sas )

# ..should equal the number of unique-match-merged records..
mrow <- nrow( merge( unique( sf[ , c( 'state' , 'psu' , 'smsastat' ) ] ) , sas ) )

# ..and it does/they do.
stopifnot( sas.row == mrow )

# now the census block-level nationwide census data *could* merge if you wanted it to.


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
popsum <- aggregate( sf$pop100 , by = ( sf[ , c( 'state' , 'psu' , 'smsastat' ) ] ) , sum )

# make the column name meaningful
names( popsum )[ names( popsum ) == 'x' ] <- 'popsum'

# merge the popsum onto the sasfile
sas <- merge( sas , popsum )

# now.  merge
	# the transportation share of total expenditure (the variable of interest)
	# the inverted standard error (the total weight of the broad geography)
	# the population sum (the total population of all census blocks that are part of that geography)

x <- merge( sf , sas )

# confirm no record loss
stopifnot( nrow( x ) == nrow( sf ) )

# clear up RAM
rm( sf ) ; gc()

# (this is the fun part)
# calculate the weight of each census block
x$weight <- x$invse * ( x$pop100 / x$popsum )

# note that weight of all census blocks put together
# sums to the `invse` on the original analysis file
stopifnot( all.equal( sum( x$weight ) , sum( sas$invse ) ) )

# scale all weights so that they average to one
x$weight <- x$weight / mean( x$weight )


# now that all weights have been computed,
# remove alaska and hawaii
x <- subset( x , !( akhi ) )
# note that those states need to be included up until this point
# otherwise their populations won't scoop up their
# respective shares of any multi-state statistics


# you're done preparing your data.
# keep only the columns you need.
x <- x[ , c( 'share' , 'weight' , 'intptlat' , 'intptlon' ) ]

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
# with the census block-level transportation shares and centroids


# initiate the simple map
us.map <- 
	qplot( 
		intptlon , 
		intptlat , 
		data = x , 
		colour = share ,
		xlab = NULL ,
		ylab = NULL
	)

# choose your coloring and severity from the midpoint
us.map <- 
	us.map + 

	scale_colour_gradient2( 
	
		# low transportation spending is good
		low = muted( "blue" ) , 
		# so invert the default colors
		high = muted( "red" ) , 
		
		# shows the most severe difference in coloring
		midpoint = mean( unique( x$share ) )
		
		# shows the population-weighted difference in coloring
		# midpoint = weighted.mean( x$share , x$weight )
	)

	
# remove all map crap.

us.map <- 
	us.map + 

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
us.map

# print the map with an albers projection.
us.map + coord_map( project = "albers" , lat0 = min( x$intptlat ) , lat1 = max( x$intptlat ) )
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
		x$share ,
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
		share ~ s(intptlon , intptlat ) , 
		weights = weight , 
		data = x
	)
	
# # end of step 6 # #
# # # # # # # # # # #


# # # # # # # # # # # #
# # step 7: outline # #

library(maptools)

shpus.tf <- tempfile()

download.cache( 
	"http://www2.census.gov/geo/tiger/TIGER2010/STATE/2010/tl_2010_us_state10.zip" ,
	shpus.tf ,
	mode = 'wb'
)

shpus.uz <- unzip( shpus.tf , exdir = tempdir() )

us.shp <- readShapePoly( shpus.uz[ grep( 'shp$' , shpus.uz ) ] )

# remove alaska, hawaii, and puerto rico
us.shp <- subset( us.shp , !( STUSPS10 %in% c( 'AK' , 'HI' , 'PR' ) ) )

# projection <- paste0( "+proj=albers +lat_0=" , min( x$intptlat ) , " +lat_1=" , max( x$intptlat ) )

# proj4string( us.shp ) <- projection
# us.shp <- spTransform( us.shp , CRS( projection ) )

# # end of step 7 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # #
# # step 8: make a grid and predict # #

# do you want your map to print decently in a few minutes?
# grid.length <- 100
# or beautifully in a few hours?
grid.length <- 500


# create three identical grid objects
grd <- gam.grd <- krig.grd <- 
	expand.grid(
		intptlon = seq( from = bbox( us.shp )[1,1] , to = bbox( us.shp )[1,2] , length = grid.length ) , 
		intptlat = seq( from = bbox( us.shp )[2,1] , to = bbox( us.shp )[2,2] , length = grid.length )
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

# # end of step 8 # #
# # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 9: create a polygon to cover everything outside the boundary # #

library(rgeos)

# convert grd to SpatialPoints object
# coordinates( outer.grd ) <- c( "intptlon" , "intptlat" )

# draw a rectangle around the grd
# us.shp.diff <- gEnvelope( outer.grd )
us.shp.out <- gEnvelope( us.shp )


# Create a bounding box 10% bigger than the bounding box of connecticut
# x_excess = (us.shp@bbox['x','max'] - us.shp@bbox['x','min'])*0.1
# y_excess = (us.shp@bbox['y','max'] - us.shp@bbox['y','min'])*0.1
# x_min = us.shp@bbox['x','min'] - x_excess
# x_max = us.shp@bbox['x','max'] + x_excess
# y_min = us.shp@bbox['y','min'] - y_excess
# y_max = us.shp@bbox['y','max'] + y_excess
# bbox = matrix(c(x_min,x_max,x_max,x_min,x_min,
                # y_min,y_min,y_max,y_max,y_min),
              # nrow = 5, ncol =2)
# bbox = Polygon(bbox, hole=FALSE)
# bbox = Polygons(list(bbox), "bbox")
# us.shp.out = SpatialPolygons(Srl=list(bbox), pO=1:1, proj4string=us.shp@proj4string)




# proj4string( us.shp.diff ) <- projection
# us.shp.diff <- spTransform( us.shp.diff , CRS( projection ) )

# get the difference between your boundary and the rectangle
# us.shp.diff <- gDifference( bbox , us.shp )
us.shp.diff <- gDifference( us.shp.out , us.shp )

# # end of step 9 # #
# # # # # # # # # # #


stop( "capping your outliers is critically important.  the scale is much more visible if they are maxxed and minned" )




library(ggplot2)
library(scales)
library(mapproj)


outside <- fortify( us.shp.diff )
# outside <- us.shp.diff

# weighted.
plot <- ggplot(data = krig.grd, aes(x = intptlon, y = intptlat))  #start with the base-plot 
layer1 <- geom_tile(data = krig.grd, aes(fill = kout ))  #then create a tile layer and fill with predicted values
layer2 <- geom_polygon(data=outside, aes(x=long,y=lat,group=group), fill='white')
co <- coord_map( project = "albers" , lat0 = min( x$intptlat ) , lat1 = max( x$intptlat ) )
# print this to a pdf instead, so it formats properly
# plot + layer1 + layer2 + co + scale_fill_gradient( low = muted( 'blue' ) , high = muted( 'red' ) )
# plot + layer1 + layer2 + scale_fill_gradient( low = muted( 'blue' ) , high = muted( 'red' ) )

# plot + layer1 + layer2 + co + scale_fill_gradient( low = muted( 'blue' ) , high = muted( 'red' ) ) + coord_equal()

plot + layer1 + layer2 + scale_fill_gradient( low = muted( 'white' ) , high = muted( 'red' ) ) + co



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


stop( 'test this out on your personal 8gb laptop?' )