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

# create a temporary directory
td <- tempdir()

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

	# unzip the summary file #1 files
	sf1.uz <- unzip( sf1.tf , exdir = td )

	# file layout from http://www.census.gov/prod/cen2010/doc/sf1.pdf#page=18
	sf1 <- read.fwf( sf1.uz[ grep( "geo2010" , sf1.uz ) ] , c( -8 , 3 , -14 , 1 , -1 , 2 , -25 , 6 , 1 , 4 , -47 , 5 , 2 , -5 , 3 , -191 , 9 , -9 , 11 , 12 ) )

	# add columns names matching the census bureau, so it's easy to read
	names( sf1 ) <- c( "sumlev" , "region" , "state" , "tract" , "blkgrp" , "block" , "cbsa" , "cbsasc" , "csa" , "pop100" , "intptlat" , "intptlon" )

	# summary level 101 has metro areas, urban/rural status, and census blocks
	sf1.101 <- subset( sf1 , sumlev == "101" )

	# stack these blocks in with all the other states
	sf <- rbind( sf , sf1.101 )
	
	# remove the single-state data.frame objects and clear up RAM
	rm( sf1.101 , sf1 ) ; gc()
	
	# remove the unzipped files from your local disk
	file.remove( sf1.uz , sf1.tf )

}

# one record per census block in every state.  see?  same number.
table( sf$state )
# https://www.census.gov/geo/maps-data/data/tallies/census_block_tally.html

# and guess what?  the population by state matches as well.
tapply( sf$pop100 , sf$state , sum )
# http://en.wikipedia.org/wiki/2010_United_States_Census#State_rankings

# remove columns you actually don't need
sf <- sf[ , !( names( sf ) %in% c( 'sumlev' , 'block' , 'region' , 'tract' , 'blkgrp' , 'cbsasc' ) ) ]

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

# anyone with a missing flag needs their geography blanked out
sf[ is.na( sf$keep ) , 'state' ] <- 99
sf[ is.na( sf$keep ) , 'psu' ] <- 9999

# remove the flag
sf$keep <- NULL

# continue being as sparse as possible.  remove columns you no longer need.
sf <- sf[ , ( names( sf ) %in% c( 'state' , 'psu' , 'smsastat' , 'pop100' , 'intptlat' , 'intptlon' ) ) ]

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
x <- subset( x , !( state %in% c( 2 , 15 ) )
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
