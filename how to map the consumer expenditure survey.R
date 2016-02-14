# # # # # # # # # # # # # # # # #
# # set the working directory # #
# # # # # # # # # # # # # # # # #

# setwd( "C:/My Directory/SWMAP/" )


# # # # # # # # # # # # # # # #
# # example survey data set # #
# # # # # # # # # # # # # # # #

# consumer expenditure survey


# # # # # # # # # # # # # # # # # # # # #
# # different from other maps because # #
# # # # # # # # # # # # # # # # # # # # #

# this is a good starting point, probably
# the simplest map you can make with survey data


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

# https://github.com/ajdamico/asdfree/tree/master/Consumer%20Expenditure%20Survey


# # # # # # # # # # # # #
# # value of interest # #
# # # # # # # # # # # # #

# transportation expenditure as a share of total expenditure


# # # # # # #
# # flaws # #
# # # # # # #

# major downloads required.  let this run overnight.


# # # # # # # # # # # # # # # # # # # # #
# # step 1: load the survey microdata # #

# remove the # in order to run this install.packages line only once
# install.packages( c( "MonetDB.R" , "MonetDBLite" , "sqldf" , "survey" , "SAScii" , "descr" , "mitools" , "plyr" , "downloader" , "digest" , "readxl" , "stringr" , "reshape2" , "XML" , "R.utils" ) , repos=c("http://dev.monetdb.org/Assets/R/", "http://cran.rstudio.com/"))

library(downloader)

# download the consumer expenditure survey microdata onto the local disk
source_url( "https://raw.githubusercontent.com/ajdamico/asdfree/master/Consumer%20Expenditure%20Survey/download%20all%20microdata.R" , prompt = FALSE , echo = TRUE )

# # end of step 1 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 2: download and import necessary geographic crosswalks # #

library(downloader)
library(sqldf)

# load the download_cached and related functions
# to prevent re-downloading of files once they've been downloaded.
source_url(
	"https://raw.github.com/ajdamico/asdfree/master/Download%20Cache/download%20cache.R" ,
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
	download_cached( ftp.loc , sf1.tf , mode = 'wb' )
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
# one record per census tract,
# and also with each of the geography-levels
# that match the consumer expenditure survey
head( sf )

# and guess what?
# we've now got the census 2010 weighted populations (field pop100)
# and also each census tract's centroid latitude & longitude (fields intptlat + intptlon)

# add the consumer expenditure survey results'
# geographic identifiers to the census tract data.frame

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
# # https://github.com/ajdamico/asdfree/blob/master/Consumer%20Expenditure%20Survey/2011%20fmly%20intrvw%20-%20analysis%20examples.R
# -- calculate the transportation share of total expenditure at the smallest available geographic area

# load all five quarters of 2013 microdata
load( "./2013/intrvw/fmli131x.rda" )
load( "./2013/intrvw/fmli132.rda" )
load( "./2013/intrvw/fmli133.rda" )
load( "./2013/intrvw/fmli134.rda" )
load( "./2013/intrvw/fmli141.rda" )

# load all five quarters of 2012 microdata
load( "./2012/intrvw/fmli121x.rda" )
load( "./2012/intrvw/fmli122.rda" )
load( "./2012/intrvw/fmli123.rda" )
load( "./2012/intrvw/fmli124.rda" )
load( "./2012/intrvw/fmli131.rda" )

# stack all ten quarters
fmly <- rbind.fill( fmli121x , fmli122 , fmli123 , fmli124 , fmli131 , fmli131x , fmli132 , fmli133 , fmli134 , fmli141 )

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
for ( i in wtrep ){
	# overwrite all missing values (NA) with zeroes
	fmly[ is.na( fmly[ , i ] ) , i ] <- 0

	# since we've pooled two years, divide all weights by two
	fmly[ , i ] <- fmly[ , i ] / 2
}

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
rm( fmly , fmli121x , fmli122 , fmli123 , fmli124 , fmli131 , fmli131x , fmli132 , fmli133 , fmli134 , fmli141 ) ; gc()
# remove them and clear up RAM


# calculate the 2012-2013 nationwide ratio of transportation spending as a share of total spending
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

# save a continental states copy of the `sf` table for later
sfsave <- sf[ !( sf$state %in% c( 2 , 15 ) ), c( 'state' , 'csa' , 'cbsa' , 'pop100' , 'intptlat' , 'intptlon' ) ]
# trust me, you'll need this.

# merge the available geographies on to the census tract file too
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

# integers are overflowing
sf$pop100 <- as.numeric( sf$pop100 )

# continue being as sparse as possible.  remove columns you no longer need.
sf <- sf[ , ( names( sf ) %in% c( 'state' , 'psu' , 'smsastat' , 'pop100' , 'intptlat' , 'intptlon' , 'akhi' ) ) ]

# clear up RAM
gc()

# confirm that we've created all possible geographies correctly.

# the number of records in our small area statistics..
sas.row <- nrow( sas )

# ..should equal the number of unique-match-merged records..
mrow <- nrow( merge( unique( sf[ , c( 'state' , 'psu' , 'smsastat' ) ] ) , sas ) )

# ..and it does/they do.
stopifnot( sas.row == mrow )

# now the census tract-level nationwide census data *could* merge if you wanted it to.


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

# distribute that weight across all census tracts #


# aggregate the 2010 census block populations to the geographies that you have.
popsum <- aggregate( sf$pop100 , by = ( sf[ , c( 'state' , 'psu' , 'smsastat' ) ] ) , sum )

# make the column name meaningful
names( popsum )[ names( popsum ) == 'x' ] <- 'popsum'

# merge the popsum onto the sasfile
sas <- merge( sas , popsum )

# now.  merge
	# the transportation share of total expenditure (the variable of interest)
	# the inverted standard error (the total weight of the broad geography)
	# the population sum (the total population of all census tracts that are part of that geography)

x <- merge( sf , sas )

# confirm no record loss
stopifnot( nrow( x ) == nrow( sf ) )

# clear up RAM
rm( sf ) ; gc()

# (this is the fun part)
# calculate the weight of each census tract
x$weight <- x$invse * ( x$pop100 / x$popsum )

# note that weight of all census tracts put together
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
library(RColorBrewer)

# before you ever touch surface smoothing or kriging,
# make some decisions about how you generally want
# your map to look:  the projection and coloring

# the options below simply use hadley wickham's ggplot2
# with the census tract-level transportation spending shares and centroids


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

# if you like that projection, store it in the map object.
us.map <- 
	us.map + 
	coord_map( project = "albers" , lat0 = min( x$intptlat ) , lat1 = max( x$intptlat ) )


# check out some purty colors.

# from http://colorbrewer2.org/

# three sequential color schemes
Greys.3.p <- colorRampPalette( rev( brewer.pal( 3 , "Greys" ) ) )
YlGnBu.3.p <- colorRampPalette( rev( brewer.pal( 3 , "YlGnBu" ) ) )
YlGnBu.9.p <- colorRampPalette( rev( brewer.pal( 9 , "YlGnBu" ) ) )

# three diverging color schemes
PRGn.11.p <- colorRampPalette( rev( brewer.pal( 11 , "PRGn" ) ) )
RdYlBu.3.p <- colorRampPalette( rev( brewer.pal( 3 , "RdYlBu" ) ) )
RdYlBu.11.p <- colorRampPalette( rev( brewer.pal( 11 , "RdYlBu" ) ) )

# print all six
us.map + scale_colour_gradientn( colours = Greys.3.p( 100 ) )
us.map + scale_colour_gradientn( colours = YlGnBu.3.p( 100 ) )
us.map + scale_colour_gradientn( colours = YlGnBu.9.p( 100 ) )

us.map + scale_colour_gradientn( colours = PRGn.11.p( 100 ) )
us.map + scale_colour_gradientn( colours = RdYlBu.3.p( 100 ) )
us.map + scale_colour_gradientn( colours = RdYlBu.11.p( 100 ) )

# clear up RAM
rm( us.map ) ; gc()

# # end of step 5 # #
# # # # # # # # # # #


# # # # # # # # # # # #
# # step 6: outline # #

library(maptools)
library(raster)
library(rgeos)
library(stringr)
library(plyr)
library(ggplot2)

shpus.tf <- tempfile()

# use the census bureau's cartographic boundary files
# instead of the regular tiger shapefiles
# unless you want to display transportation shares in the ocean.

download_cached( 
	"http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_state_500k.zip" ,
	shpus.tf ,
	mode = 'wb'
)

shpus.uz <- unzip( shpus.tf , exdir = tempdir() )

us.shp <- readShapePoly( shpus.uz[ grep( 'shp$' , shpus.uz ) ] )

# remove alaska, hawaii, and all territories
us.shp <- subset( us.shp , !( STUSPS %in% c( 'AK' , 'HI' , 'PR' , 'AS' , 'VI' , 'GU' , 'MP' ) ) )

# draw a rectangle 5% bigger than the original 48-state shape
us.shp.out <- as( 1.1 * extent( us.shp ), "SpatialPolygons" )

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

# you could let the `fields` package attempt to guess knots for you,
# xknots <- cover.design( cbind( x$intptlon , x$intptlat ) , 100 )$design
# but with census microdata, you've already got easy access to a relevant geographic grouping

# the continental united states contains
length( unique( sfsave$cbsa ) )
# unique core-based statistical areas and
length( unique( sfsave$csa ) )
# unique combined statistical areas, including `99999` values.

# but you should probably distinguish the `99999` values across states.
# if you have a powerful computer, you could try creating knots table
# that crosses states by cbsas, but for smaller computers (and quicker processing)
nrow( unique( sfsave[ , c( 'state' , 'csa' ) ] ) )
# here are 207 beeeeautiful knots just for you.


# within each state x csa,
# calculate the population-weighted mean of the coordinates
# and (for smoothing) the weighted share at each state-csa centroid
us.knots <- 
	sqldf( 
		"select 
			state , csa ,
			sum( pop100 ) as pop100 , 
			sum( pop100 * intptlon ) / sum( pop100 ) as intptlon ,
			sum( pop100 * intptlat ) / sum( pop100 ) as intptlat
		from sfsave
		group by
			state , csa"
	)
# note: this screws up coordinates that cross the international date line
# or the equator.  in the united states, only alaska's aleutian islands do this
# and those geographies will be thrown out later.  so it doesn't matter.

# how many knots have you gots?
nrow( us.knots )

# you can look at the weighted centroids of those csas by state
plot( us.knots$intptlon , us.knots$intptlat )

# clear up RAM
rm( sfsave ) ; gc()

# interpolation option one #
library(fields)

krig.fit <-
	Krig(
		cbind( x$intptlon , x$intptlat ) ,
		x$share ,
		weights = x$weight ,
		knots = cbind( us.knots$intptlon , us.knots$intptlat )
		# if you prefer to use cover.design, all you'd need is this knots= line instead:
		# knots = xknots
	)

# that is: what is the (weighted) relationship between
# your variable of interest (transportation share of total expenditure) and
# the x/y points on a grid?

# check this out!
surface( krig.fit )
# you're almost there!


# interpolation option two #
library(mgcv)

gam.fit <- 
	gam( 
		share ~ s(intptlon , intptlat ) , 
		weights = weight , 
		data = x
	)
	

# for the third alternative, keep reading.
	
	
# # end of step 7 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # #
# # step 8: make a grid and predict # #

library(raster)

x.range <- bbox( us.shp )[ 1 , ]
y.range <- bbox( us.shp )[ 2 , ]

# add one percent on each side
x.diff <- abs( x.range[ 2 ] - x.range[ 1 ] ) * 0.01
y.diff <- abs( y.range[ 2 ] - y.range[ 1 ] ) * 0.01

x.range[ 1 ] <- x.range[ 1 ] - x.diff
x.range[ 2 ] <- x.range[ 2 ] + x.diff
y.range[ 1 ] <- y.range[ 1 ] - y.diff
y.range[ 2 ] <- y.range[ 2 ] + y.diff

# choose the number of ticks (in each direction) on your grid
grid.length <- 500
# grid.length <- 700
# # note: smaller grids will render much much faster
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
# the transportation spending share?
for ( i in split( seq( nrow( grd ) ) , ceiling( seq( nrow( grd ) ) / 20000 ) ) ){
	krig.grd[ i , 'kout' ] <- as.numeric( predict( krig.fit , krig.grd[ i , c( 'intptlon' , 'intptlat' ) ] ) )
	gc()
}

# alternate grid using gam.fit
for ( i in split( seq( nrow( grd ) ) , ceiling( seq( nrow( grd ) ) / 20000 ) ) ){
	gam.grd[ i , 'gamout' ] <- as.numeric( predict( gam.fit , gam.grd[ i , c( 'intptlon' , 'intptlat' ) ] ) )
	gc()
}

# interpolation option three #
library(spatstat)

smoout <- 
	Smooth(
		ppp( 
			x$intptlon , 
			x$intptlat , 
			x.range ,
			y.range ,
			marks = x$share
		) ,
		# here's a good starting point for sigma, but screw around with this value.
		sigma = 2 ,
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
gam.grd$gamout <- minnmax.at.0595( gam.grd$gamout )
smoo.grd$smoout <- minnmax.at.0595( smoo.grd$smoout )


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

# choose a projection.  here's one using albers on the continental united states borders
co <- coord_map( project = "albers" , lat0 = min( x$intptlat ) , lat1 = max( x$intptlat ) )
# but save this puppy for laytur
# because printing the projected plot takes much more time than printing the unprojected one

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
	
	# blank out other plot elements

	scale_x_continuous( limits = x.range , breaks = NULL , oob = squish ) +

    scale_y_continuous( limits = y.range , breaks = NULL , oob = squish ) +

	theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		panel.background = element_blank(),
		panel.border = element_blank(),
		axis.ticks = element_blank()
	)

# print the plot to the screen
the.plot

# # end of step 9 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # #
# # step 10: blank, color, save # #

library(ggplot2)
library(scales)
library(raster)
library(plyr)
library(RColorBrewer)
library(rgeos)


# draw a rectangle 15% bigger than the original state
us.shp.blank <- as( 1.3 * extent( us.shp ), "SpatialPolygons" )

# compute the difference between connecticut and the rectangle 15% beyond the borders
us.shp.diff <- gDifference( us.shp.blank , us.shp )

# prepare the difference layer for ggplot2
outside <- fortify( us.shp.diff )

# fix the islands
outside2 <- ddply( outside , .(piece) , function(x) rbind( x , outside[ 1 , ] ) )

# blank out coastal areas
blank.layer <- 
	geom_polygon( 
		data = outside2 , 
		aes( x = long , y = lat , group = id ) , 
		fill = 'white' 
	)

# closer, eh?
the.plot + blank.layer

# store this plot
the.plot <- the.plot + blank.layer	

# location of all state borders
sbo <- fortify( us.shp )

# here's a layer with the continental united states borders
sbo.layer <- geom_path( data = sbo , aes( x = long , y = lat , group = group ) , colour = 'lightgray' )

# print that result to the screen.
the.plot + sbo.layer
# okay if we stick with this one?
the.plot <- the.plot + sbo.layer
# good.  now it's part of the plot.

# print all six
the.plot + scale_fill_gradientn( colours = Greys.3.p( 100 ) )
the.plot + scale_fill_gradientn( colours = YlGnBu.3.p( 100 ) )
the.plot + scale_fill_gradientn( colours = YlGnBu.9.p( 100 ) )

the.plot + scale_fill_gradientn( colours = PRGn.11.p( 100 ) )
the.plot + scale_fill_gradientn( colours = RdYlBu.3.p( 100 ) )
the.plot + scale_fill_gradientn( colours = RdYlBu.11.p( 100 ) )

# ooh i like that one mom, can we keep it can we keep it?
final.plot <- the.plot + scale_fill_gradientn( colours = RdYlBu.11.p( 100 ) )

final.plot

# would you like to save this game?

# use cairo-png as your bitmap type
options( bitmapType = "cairo" )

# save the file to your current working directory
ggsave( 
	"2012-2013 transportation spending as a share of total spending - unprojected.png" ,
	plot = final.plot ,
	type = "cairo-png"
)

# add the projection
projected.plot <- final.plot + co

# # # # # # # # # # # # # #
# warning warning warning #

# this next save-line takes a few hours.
# leave it running overnight

# warning warning warning #
# # # # # # # # # # # # # #

# save the file to your current working directory
ggsave( 
	"2012-2013 transportation spending as a share of total spending - projected.png" ,
	plot = projected.plot ,
	type = "cairo-png"
)

# # end of step ten # #
# # # # # # # # # # # #
