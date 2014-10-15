# # # # # # # # # # # # # # # # #
# # set the working directory # #
# # # # # # # # # # # # # # # # #

# setwd( "C:/My Directory/SWMAP/" )


# # # # # # # # # # # # # # # #
# # example survey data set # #
# # # # # # # # # # # # # # # #

# american community survey


# # # # # # # # # # # # # # # # # #
# # smallest level of geography # #
# # # # # # # # # # # # # # # # # #

# state, public use microdata areas


# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # asdfree.com blog post for this survey microdata # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# http://www.asdfree.com/search/label/american%20community%20survey%20%28acs%29


# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # r code repository for setup and analysis examples # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# https://github.com/ajdamico/usgsd/tree/master/American%20Community%20Survey


# # # # # # # # # # # # #
# # value of interest # #
# # # # # # # # # # # # #

# race/ethnicity population shares (categorical)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
###################################################################################################################################
# prior to running this analysis script, monetdb must be installed on the local machine.  follow each step outlined on this page: #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# https://github.com/ajdamico/usgsd/blob/master/MonetDB/monetdb%20installation%20instructions.R                                   #
###################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # #
# # step 1: load the survey microdata # #

library(downloader)

# download the 2012 american community survey microdata onto the local disk
# options( "monetdb.sequential" = TRUE )
single.year.datasets.to.download <- 2012
three.year.datasets.to.download <- NULL
five.year.datasets.to.download <- NULL
source_url( "https://raw.github.com/ajdamico/usgsd/master/American%20Community%20Survey/download%20all%20microdata.R" , prompt = FALSE , echo = TRUE )

# # end of step 1 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 2: conduct your analysis of interest at the smallest geography allowed # #

library(sqlsurvey)
library(MonetDB.R)

# following the analysis examples in the r code repository --
# # https://raw.githubusercontent.com/ajdamico/usgsd/master/American%20Community%20Survey/2011%20single-year%20-%20analysis%20examples.R
# -- calculate race/ethnicity shares at the smallest available geographic area



####################################################################
# lines of code to hold on to for all other `acs` monetdb analyses #

# first: specify your batfile.  again, mine looks like this:
# uncomment this line by removing the `#` at the front..
batfile <- paste0( getwd() , "/MonetDB/acs.bat" )

# second: run the MonetDB server
pid <- monetdb.server.start( batfile )

# third: your five lines to make a monet database connection.
# just like above, mine look like this:
dbname <- "acs"
dbport <- 50001

monet.url <- paste0( "monetdb://localhost:" , dbport , "/" , dbname )
db <- dbConnect( MonetDB.R() , monet.url , wait = TRUE )


# # # # run your analysis commands # # # #

# analyze the 2012 single-year acs
load( './acs2012_1yr.rda' )

# open the design connection to the person-level design
acs.m <- open( acs.m.design , driver = MonetDB.R() , wait = TRUE )

# restrict the acs.m object to alaska only
acs.m.alaska <- subset( acs.m , st == 2 )

# percent american indian/alaska native by puma
aian <- svymean( ~I( racaian == 1 ) , acs.m.alaska , byvar = ~ puma )

# percent white non-hispanic by puma
white.nh <- svymean( ~ I( rac1p == 1 & hisp == 1 ) , acs.m.alaska , byvar = ~ puma )

# percent all others by puma
all.others <- svymean( ~ I( ( ( rac1p > 1 ) | ( hisp > 1 ) ) & racaian == 0 ) , acs.m.alaska , byvar = ~ puma )


# note: if your variables of interest aren't easily accessible from the current structure
# you might recode the variables into your desired categories instead
# https://github.com/ajdamico/usgsd/blob/master/American%20Community%20Survey/2011%20single-year%20-%20variable%20recode%20example.R


# create a data.frame with these three results..
results <- cbind( aian , white.nh , all.others )

# ..and standard errors
ses <- cbind( SE( aian ) , SE( white.nh ), SE( all.others ) )

# combine everything into a single data.frame object
alaska.pumas <- 
	data.frame( 
		puma = gsub( "X____racaian___1____:" , "" , rownames( results ) ) ,
		results , 
		ses
	)

# name the three standard error columns
names( alaska.pumas )[ 5:7 ] <- paste0( 'se.' , names( alaska.pumas )[ 2:4 ] )

# remove the row names, since they're useless
rownames( alaska.pumas ) <- NULL

# close the connection to the sqlrepsurvey design object
close( acs.m.alaska )
close( acs.m )

# disconnect from the current monet database
dbDisconnect( db )

# and close it using the `pid`
monetdb.server.stop( pid )

# end of lines of code to hold on to for all other `acs` monetdb analyses #
###########################################################################

# these are the small area statistics to be mapped
print( alaska.pumas )
# the standard errors are a measure of precision,
# their inverse will serve as the mapping weights

# make this object easier to type
sas <- alaska.pumas

# # end of step 2 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 3: download and import necessary geographic crosswalks # #

library(downloader)
library(maptools)

# load the download.cache and related functions
# to prevent re-downloading of files once they've been downloaded.
source_url(
	"https://raw.github.com/ajdamico/usgsd/master/Download%20Cache/download%20cache.R" ,
	prompt = FALSE ,
	echo = FALSE
)


# create a temporary file containing the census bureau's
# 2010 census tract to 2010 puma crosswalk
# then download the file.
ctpxw.tf <- tempfile()

download.cache(
	"http://www.census.gov/geo/maps-data/data/docs/rel/2010_Census_Tract_to_2010_PUMA.txt" ,
	ctpxw.tf ,
	mode = 'wb'
)

# import this csv file into an R data.frame object
ctpxw <- read.csv( ctpxw.tf )

# match the column names of sf1 and of the `sas` output
names( ctpxw ) <- c( 'state' , 'county' , 'tract' , 'puma' )

# immediately limit this to alaskan census tracts
ak.ctpxw <- subset( ctpxw , state == 2 )

# clear up RAM
rm( ctpxw ) ; gc()


# create a temporary file containing the census bureau's
# 2010 census summary file #1 for alaska
# then download the file.
sf1ak.tf <- tempfile()

download.cache( 
	"ftp://ftp2.census.gov/census_2010/04-Summary_File_1/Alaska/ak2010.sf1.zip" ,
	sf1ak.tf ,
	mode = 'wb'
)

# create a temporary directory
td <- tempdir()

# unzip the summary file #1 files
sf1ak.uz <- unzip( sf1ak.tf , exdir = td )

# file layout from http://www.census.gov/prod/cen2010/doc/sf1.pdf#page=18
sf1ak <- read.fwf( sf1ak.uz[ grep( "akgeo2010" , sf1ak.uz ) ] , c( -8 , 3 , -16 , 2 , 3 , -22 , 6 , 1 , 4 , -253 , 9 , -9 , 11 , 12 ) )

# add columns names matching the census bureau, so it's easy to read
names( sf1ak ) <- c( "sumlev" , "state" , "county" , "tract" , "blkgrp" , "block" , "pop100" , "intptlat" , "intptlon" )

# summary level 101 has census tracts and census blocks
sf1ak.101 <- subset( sf1ak , sumlev == "101" )

# merge these files together
sf1ak.101 <- merge( sf1ak.101 , ak.ctpxw )
# the number of records and population sums serve
# as a check to confirm that this merge worked

# one record per census block in alaska.  see?  same number.
nrow( sf1ak.101 )
# https://www.census.gov/geo/maps-data/data/tallies/census_block_tally.html

# and guess what?  the total alaska population matches as well.
sum( sf1ak.101$pop100 )
# http://quickfacts.census.gov/qfd/states/02000.html

# clear up RAM
rm( sf1ak ) ; gc()


# so now we have a data.frame object with
# one record per census block,
# and also with the geography (puma)
# that matches the american community survey
head( sf1ak.101 )

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
mrow <- nrow( merge( unique( sf1ak.101[ "puma" ] ) , sas ) )

# ..and it does/they do.
stopifnot( sas.row == mrow )

# now the census block-level alaska census data *could* merge if you wanted it to.


# but you don't.  yet.


# the standard error (the `se.` fields) are measures of precision.
print( sas )
# the smaller the standard error, the more confident you should be
# that the estimate at a particular geography is correct.


# so invert them.  you heard me.  invert them.
sas$invse.white.nh <- 1 / sas$se.white.nh
sas$invse.aian <- 1 / sas$se.aian
sas$invse.all.others <- 1 / sas$se.all.others
# a smaller standard error indicates more precision.

# for our purposes, precision can be considered weight! #

# now we've got the weight that we should give each of our estimates #

# distribute that weight across all census blocks #


# aggregate the 2010 census block populations to the geographies that you have.
popsum <- aggregate( sf1ak.101$pop100 , by = ( sf1ak.101[ "puma" ] ) , sum )

# make the column name meaningful
names( popsum )[ names( popsum ) == 'x' ] <- 'popsum'

# merge the popsum onto the sasfile
sas <- merge( sas , popsum )

# now.  merge
	# the race/ethnicity shares (the variable of interest)
	# the inverted standard errors (the total weight of the broad geography)
	# the population sum (the total population of all census blocks that are part of that geography)

x <- merge( sf1ak.101 , sas )

# confirm no record loss
stopifnot( nrow( x ) == nrow( sf1ak.101 ) )


# (this is the fun part)
# calculate three weights at each census block
x$weight.aian <- x$invse.aian * ( x$pop100 / x$popsum )
x$weight.white.nh <- x$invse.white.nh * ( x$pop100 / x$popsum )
x$weight.all.others <- x$invse.all.others * ( x$pop100 / x$popsum )

# note that weight of all census blocks put together
# sums to the `invse` on the original analysis file
stopifnot( sum( x$weight.aian ) == sum( sas$invse.aian ) )
stopifnot( sum( x$weight.white.nh ) == sum( sas$invse.white.nh ) )
stopifnot( sum( x$weight.all.others ) == sum( sas$invse.all.others ) )

# remove records with zero population across all three measures
x <- subset( x , weight.aian > 0 | weight.white.nh > 0 | weight.all.others > 0 )

# scale all weights so that they average to one
x$weight.aian <- x$weight.aian / mean( x$weight.aian )
x$weight.white.nh <- x$weight.white.nh / mean( x$weight.white.nh )
x$weight.all.others <- x$weight.all.others / mean( x$weight.all.others )

# you're done preparing your data.
# keep only the columns you need.
x <- x[ , c( 'aian' , 'white.nh' , 'all.others' , 'weight.aian' , 'weight.white.nh' , 'weight.all.others' , 'intptlat' , 'intptlon' ) ]

# # end of step 4 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # #
# # step 5: decide on your map parameters # #

library(ggplot2)
library(scales)
library(mapproj)

# pop quiz: which states are the furthest north, east, south, west?
# if you guessed alaska, maine, hawaii, alaska, you are wrong!
# the answer is alaska, alaska, hawaii, alaska.

# a few of the aleutians cross the international date line.

# do you want to keep the edges of the aleutian islands in your map?
# of course you do!  here's an ultra-simple recode to keep them gridded together.
x <- transform( x , intptlon = ifelse( intptlon > 0 , intptlon - 360 , intptlon ) )


# before you ever touch surface smoothing or kriging,
# make some decisions about how you generally want
# your map to look:  the projection and coloring

# the options below simply use hadley wickham's ggplot2
# with the census block-level race/ethnicity shares and centroids

# add a unique color-identifier to the data.frame
x$color.column <- paste( x$aian , x$white.nh , x$all.others )

# add the hex color identifier
x$color.value <- rgb( x$aian , x$white.nh , x$all.others )

# initiate the simple map
ak.map <- 
	qplot( 
		intptlon , 
		intptlat , 
		data = x , 
		colour = color.column ,
		xlab = NULL ,
		ylab = NULL
	)


# manually add the hex colors to the map
ak.map <- ak.map + scale_color_manual( values = unique( x$color.value ) )

	
# remove all map crap.

ak.map <- 
	ak.map + 

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
ak.map
# personally, i prefer this one for alaska.
# if you do too, just don't add the coord_map() option

# print the map with an albers projection.
ak.map + coord_map( project = "albers" , lat0 = min( x$intptlat ) , lat1 = max( x$intptlat ) )
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


xknots <- cover.design( cbind( x$intptlon , x$intptlat ) , number.of.knots )$design


# run the `Krig` function on all three categories,
# each with their own respective weight.

krig.aian <-
	Krig(
		cbind( x$intptlon , x$intptlat ) ,
		x$aian ,
		weights = x$weight.aian ,
		knots = xknots # ,
		# Covariance = "Matern"
	)

krig.white.nh <-
	Krig(
		cbind( x$intptlon , x$intptlat ) ,
		x$white.nh ,
		weights = x$weight.white.nh ,
		knots = xknots # ,
		# Covariance = "Matern"
	)

krig.all.others <-
	Krig(
		cbind( x$intptlon , x$intptlat ) ,
		x$all.others ,
		weights = x$weight.all.others ,
		knots = xknots # ,
		# Covariance = "Matern"
	)

# # end of step 6 # #
# # # # # # # # # # #


# # # # # # # # # # # #
# # step 7: outline # #

library(maptools)

shpak.tf <- tempfile()

download.cache( 
	"ftp://ftp2.census.gov/geo/pvs/tiger2010st/02_Alaska/02/tl_2010_02_state10.zip" ,
	shpak.tf ,
	mode = 'wb'
)

shpak.uz <- unzip( shpak.tf , exdir = td )

ak.shp <- readShapePoly( shpak.uz[ grep( 'shp$' , shpak.uz ) ] )

# # end of step 7 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # #
# # step 8: make a grid and predict # #

# do you want your map to print decently in a few minutes?
grid.length <- 100
# or beautifully in a few hours?
# grid.length <- 250

# again, adjust for the aleutian islands over the international date line
bb <- bbox( ak.shp )

bb[ 1 , 2 ] <- bb[ 1 , 2 ] - 360
# adjustment over.

# create two identical grid objects
grd <- krig.grd <- 
	expand.grid(
		intptlon = seq( from = bb[1,1] , to = bb[1,2] , length = grid.length ) , 
		intptlat = seq( from = bb[2,1] , to = bb[2,2] , length = grid.length )
	)


# along your rectangular grid,
# what are the predicted values of
# each race/ethnicity category?
krig.grd$aian <- predict( krig.aian , krig.grd[ , 1:2 ] )

krig.grd$white.nh <- predict( krig.white.nh , krig.grd[ , 1:2 ] )

krig.grd$all.others <- predict( krig.all.others , krig.grd[ , 1:2 ] )

# since these predicted values do not sum to one (they need to!)
# calculate an expansion/contraction factor for each record
krig.grd$factor <- 1 / rowSums( krig.grd[ , c( 'aian' , 'white.nh' , 'all.others' ) ] )

# scale each predicted categorical share up or down, proportionally
krig.grd[ , c( 'aian' , 'white.nh' , 'all.others' ) ] <-
	sapply( 
		krig.grd[ , c( 'aian' , 'white.nh' , 'all.others' ) ] ,
		function( z ){ z * krig.grd$factor }
	)

# confirm that each row sums to one.
stopifnot( 
	all.equal( 
		rowSums( krig.grd[ , c( 'aian' , 'white.nh' , 'all.others' ) ] ) , 
		rep( 1 , nrow( krig.grd ) ) 
	) 
)

# # end of step 8 # #
# # # # # # # # # # #
