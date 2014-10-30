# # # # # # # # # # # # # # # # #
# # set the working directory # #
# # # # # # # # # # # # # # # # #

# setwd( "C:/My Directory/SWMAP/" )


# # # # # # # # # # # # # # # #
# # example survey data set # #
# # # # # # # # # # # # # # # #

# american community survey


# # # # # # # # # # # # # # # # # # # # #
# # different from other maps because # #
# # # # # # # # # # # # # # # # # # # # #

# displays a categorical variable
# crosses the international date line


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

# veterans of foreign wars shares (categorical)


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

# download the 2013 american community survey microdata onto the local disk
# options( "monetdb.sequential" = TRUE )
single.year.datasets.to.download <- 2013
three.year.datasets.to.download <- NULL
five.year.datasets.to.download <- NULL
source_url( "https://raw.github.com/ajdamico/usgsd/master/American%20Community%20Survey/download%20all%20microdata.R" , prompt = FALSE , echo = TRUE )

# # end of step 1 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 2: conduct your analysis of interest at the smallest geography allowed # #

library(survey)
library(MonetDB.R)
library(scales)

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

# subset the design to only alaska before actually constructing the design
acs.alaska <- dbGetQuery( db , 'select * from acs2013_1yr_m where st = 2' )
# note: this is not allowed for taylor-series linearized designs

# disconnect from the current monet database
dbDisconnect( db )

# and close it using the `pid`
monetdb.server.stop( pid )

# end of lines of code to hold on to for all other `acs` monetdb analyses #
###########################################################################

alaska.design <-
	svrepdesign(
		weight = ~pwgtp ,
		repweights = 'pwgtp[1-9]' ,
		scale = 4 / 80 ,
		rscales = rep( 1 , 80 ) ,
		mse = TRUE ,
		data = acs.alaska
	)

# create a denominator variable indicating any period of service
alaska.design <- update( alaska.design , vet = as.numeric( vps > 0 ) )

# create a categorical variable indicating era of service
alaska.design <- 
	update( 
		alaska.design , 
		gulf = as.numeric( vps %in% 1:5 ) ,
		vietnam = as.numeric( vps %in% 6:8 ) ,
		other = as.numeric( vps %in% 9:15 )
	)
	


# statewide era of service shares
sw <- svyratio( ~ gulf + vietnam + other , ~ vet , alaska.design , na.rm = TRUE )

# puma-specific era of service shares
ps <- svyby( ~ gulf + vietnam + other , denominator = ~ vet , by = ~ puma , alaska.design , svyratio , na.rm = TRUE )

# find the disproportionate shares
ds <- ps[ , 2:4 ] - matrix( coef( sw ) , 5 , 3 , byrow = T )

# so look at this table.
ds

# pumas 101 and 300 have veterans that disproportionately served during the gulf wars (up to the present)

# pumas 102 and 200 have veterans that disproportionately served during the vietnam war

# puma 400 has veterans that disproportionately served during another era

# hold on to these disproportionate shares and the standard errors of the original ratios.
alaska.pumas <- cbind( ps[ 1 ] , ds , ps[ , 5:7 ] )

# note that the standard error of the ratio statistic
# and the standard error of the ratio of
# the difference between statewide and puma-level statistics
# are probably not the same.  well, i'm sure of it.
# if you're an academic statistician, you might be mad at me
# for making this half-assed calculation right here.

# github makes it easy to patch and edit and update other people's code.

# go for it  ;)

# remove those slashvets from the column names
names( alaska.pumas ) <- gsub( "/vet" , "" , names( alaska.pumas ) )

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
# note: to re-download a file from scratch, add the parameter usecache = FALSE

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
# note: to re-download a file from scratch, add the parameter usecache = FALSE

# unzip the summary file #1 files
sf1ak.uz <- unzip( sf1ak.tf , exdir = tempdir() )

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
sas$invse.gulf <- 1 / sas$se.gulf
sas$invse.vietnam <- 1 / sas$se.vietnam
sas$invse.other <- 1 / sas$se.other
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
	# the disproportionate veteran era in each puma (the variable of interest)
	# the inverted standard errors (the total weight of the broad geography)
	# the population sum (the total population of all census blocks that are part of that geography)

x <- merge( sf1ak.101 , sas )

# confirm no record loss
stopifnot( nrow( x ) == nrow( sf1ak.101 ) )


# (this is the fun part)
# calculate the weight at each census block
x$weight.gulf <- x$invse.gulf * ( x$pop100 / x$popsum )
x$weight.vietnam <- x$invse.vietnam * ( x$pop100 / x$popsum )
x$weight.other <- x$invse.other * ( x$pop100 / x$popsum )

# note that weight of all census blocks put together
# sums to the `invse` on the original analysis file
stopifnot( sum( x$weight.gulf ) == sum( sas$invse.gulf ) )
stopifnot( sum( x$weight.vietnam ) == sum( sas$invse.vietnam ) )
stopifnot( sum( x$weight.other ) == sum( sas$invse.other ) )

# remove records with zero population across all three measures
x <- subset( x , weight.gulf > 0 | weight.vietnam > 0 | weight.other > 0 )

# scale all weights so that they average to one
x$weight.gulf <- x$weight.gulf / mean( x$weight.gulf )
x$weight.vietnam <- x$weight.vietnam / mean( x$weight.viet )
x$weight.other <- x$weight.other / mean( x$weight.other )


# you're done preparing your data.
# keep only the columns you need.
x <- x[ , c( 'gulf', 'vietnam' , 'other' , 'weight.gulf' , 'weight.vietnam' , 'weight.other' , 'intptlat' , 'intptlon' ) ]

# pop quiz: which states are the furthest north, east, south, west?
# if you guessed alaska, maine, hawaii, alaska, you are wrong!
# the answer is alaska, alaska, hawaii, alaska.

# a few of the aleutians cross the international date line.

# do you want to keep the edges of the aleutian islands in your map?
# of course you do!  here's an ultra-simple recode to keep them gridded together.
x <- transform( x , intptlon = ifelse( intptlon > 0 , intptlon - 360 , intptlon ) )


# # end of step 4 # #
# # # # # # # # # # #


# # # # # # # # # # # #
# # step 5: outline # #

library(maptools)
library(raster)
library(rgeos)
library(rgdal)
library(ggplot2)

# make a character vector containing the shapefiles to download
shftd <- 
	c( 
		# download the clipped alaska public use microdata area map, described
		# https://www.census.gov/geo/maps-data/maps/2010puma/st02_ak.html
		'http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_02_puma10_500k.zip' ,

		# download the clipped nationwide state outlines
		'http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_state_500k.zip' ,

		# download the roads in alaska
		'http://www2.census.gov/geo/tiger/TIGER2013/PRISECROADS/tl_2013_02_prisecroads.zip'
	)

# initiate a function to download and import all census bureau shapefiles
daiacbsf <-
	function( fn , myproj = "+init=epsg:2163" ){
		tf <- tempfile()

		# # note: to re-download a file from scratch, add the parameter usecache = FALSE # #
		download.cache( fn , tf , mode = 'wb' )

		# unzip the downloaded file to a temporary directory
		shp.uz <- unzip( tf , exdir = tempdir() )

		# figure out which filename ends with "shp"
		sfname <- grep( 'shp$' , shp.uz , value = TRUE )
		
		# read in the shapefile, using the correct layer
		sf <- readOGR( sfname  , layer = gsub( "\\.shp" , "" , basename( sfname ) ) )

		# project this shapefile immediately
		# this projection (and a few others) keeps
		# the aleutian islands that cross the
		# international date line easy to work with.
		spTransform( sf , CRS( myproj ) )
	}

# run all downloads at once, store the result in a list.
asf <- sapply( shftd , daiacbsf )

# pull out the clipped state borders of alaska only
alaska.borders <- subset( asf[[2]] , STATEFP == '02' )

# plot as-is.  see how the aleutians screw up the map?
plot( alaska.borders )

# add puma boundaries
plot( asf[[1]] , add = TRUE )

# refresh the map with state borders only
plot( alaska.borders )

# add roads
plot( asf[[3]] , add = TRUE , col = 'red' )

# draw a rectangle 15% bigger than the original state
ak.shp.blank <- as( 1.3 * extent( alaska.borders ) , "SpatialPolygons" )

# calculate the difference between the rectangle and the actual shape
ak.shp.diff <- gDifference( ak.shp.blank , alaska.borders )
# this will be used to cover up points outside of alaska's state borders

# this box will later blank out the surrounding area
plot( ak.shp.diff )

# # end of step 5 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # #
# # step 6: tie knots and krige # #

library(sqldf)

# # warning warning # # # # warning warning # # 
# alaska has a vast geography and highly skewed population centers
# kriging functions might not converge.  that's why there are other options ;)
# # warning warning # # # # warning warning # # 


# how many knots should you make? #

# knots are the computationally-intensive part of this process,
# choose as many as your computer and your patience can handle.

# you should aim for between 100 - 999 knots,
# but numbers closer to 1,000 will overload smaller computers

# you could let the `fields` package attempt to guess knots for you,
# xknots <- cover.design( cbind( x$intptlon , x$intptlat ) , 100 )$design
# but with census microdata, you've already got easy access to a relevant geographic grouping

# the sqldf() function doesn't like `.` in data.frame object names
sf1s <- sf1ak.101

# exactamundo same transform operation as you saw previously on `x`
sf1s <- transform( sf1s , intptlon = ifelse( intptlon > 0 , intptlon - 360 , intptlon ) )

# within each county x census tract
# calculate the population-weighted mean of the coordinates
ct.knots <- 
	sqldf( 
		"select 
			county , tract ,
			sum( pop100 ) as pop100 , 
			sum( pop100 * intptlon ) / sum( pop100 ) as intptlon ,
			sum( pop100 * intptlat ) / sum( pop100 ) as intptlat
		from sf1s
		group by
			county , tract"
	)
# note: this screws up coordinates that cross the international date line
# or the equator.  in the united states, only alaska's aleutian islands do this
# and we're mapping alaska, aren't we?  good thing we fixed it, huh?


# interpolation option one #
library(fields)

krig.fit.gulf <-
	Krig(
		cbind( x$intptlon , x$intptlat ) ,
		x$gulf ,
		weights = x$weight ,
		knots = cbind( ct.knots$intptlon , ct.knots$intptlat )
		# if you prefer to use cover.design, all you'd need is this knots= line instead:
		# knots = xknots
	)

krig.fit.vietnam <-
	Krig(
		cbind( x$intptlon , x$intptlat ) ,
		x$vietnam ,
		weights = x$weight ,
		knots = cbind( ct.knots$intptlon , ct.knots$intptlat )
		# if you prefer to use cover.design, all you'd need is this knots= line instead:
		# knots = xknots
	)

krig.fit.other <-
	Krig(
		cbind( x$intptlon , x$intptlat ) ,
		x$other ,
		weights = x$weight ,
		knots = cbind( ct.knots$intptlon , ct.knots$intptlat )
		# if you prefer to use cover.design, all you'd need is this knots= line instead:
		# knots = xknots
	)


# that is: what is the (weighted) relationship between
# your variable of interest (veteran service eras) and
# the x/y points on a grid?

# check this out!
surface( krig.fit.gulf )
surface( krig.fit.vietnam )
surface( krig.fit.other )
# you're almost there!


# interpolation option two #
library(mgcv)


gam.gulf <- 
	gam( 
		gulf ~ s( intptlon , intptlat ) , 
		weights = weight.gulf , 
		data = x
	)

gam.vietnam <- 
	gam( 
		vietnam ~ s( intptlon , intptlat ) , 
		weights = weight.vietnam , 
		data = x
	)

gam.other <- 
	gam( 
		other ~ s( intptlon , intptlat ) , 
		weights = weight.other , 
		data = x
	)

# # end of step 6 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # #
# # step 7: make a grid and predict # #

# use as fine of a grid as your computer can handle
grid.length <- 750

x.range <- c( min( x$intptlon ) , max( x$intptlon ) )
y.range <- c( min( x$intptlat ) , max( x$intptlat ) )

# add five percent on each side
x.diff <- abs( x.range[ 2 ] - x.range[ 1 ] ) * 0.2
y.diff <- abs( y.range[ 2 ] - y.range[ 1 ] ) * 0.2

x.range[ 1 ] <- x.range[ 1 ] - x.diff
x.range[ 2 ] <- x.range[ 2 ] + x.diff
y.range[ 1 ] <- y.range[ 1 ] - y.diff
y.range[ 2 ] <- y.range[ 2 ] + y.diff


grd <- krig.grd <- gam.grd <- 
	expand.grid(
		intptlon = seq( x.range[ 1 ] , x.range[ 2 ] , length = grid.length ) , 
		intptlat = seq( y.range[ 1 ] , y.range[ 2 ] , length = grid.length )
	)

	
# along your rectangular grid,
# what are the predicted values of
# each veteran era category
krig.grd$gulf <- predict( krig.fit.gulf , krig.grd[ , 1:2 ] )

krig.grd$vietnam <- predict( krig.fit.vietnam , krig.grd[ , 1:2 ] )

krig.grd$other <- predict( krig.fit.other , krig.grd[ , 1:2 ] )

gam.grd$gulf <- predict( gam.gulf , gam.grd[ , 1:2 ] )

gam.grd$vietnam <- predict( gam.vietnam , gam.grd[ , 1:2 ] )

gam.grd$other <- predict( gam.other , gam.grd[ , 1:2 ] )



# remember that these values have been re-scaled
# as how disproportionate they are from the state-wide averages.
# therefore, negative values are possible.
sapply( krig.grd , summary )
sapply( gam.grd , summary )

# what we're really hoping for is that
# the overall mean averages out to zero
sum( sapply( gam.grd , summary )[ 4 , 3:5 ] )

# in general, these predictions at each point should approximately sum to zero
summary( rowSums( krig.grd[ , 3:5 ] ) )
summary( rowSums( gam.grd[ , 3:5 ] ) )

# # end of step 7 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # #
# # step 8: limit information and color # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # warning # # # warning # # # # # # warning # # # # # # warning # # # # # # warning # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# if your data is are not binomial, then by mapping with a single image, you lose clarity #
# if you have three levels of information and you generate two maps, you can get an idea  #
# about the entire distribution of the variable.  if you attempt encoding three levels or #
# more into a single map, you will explode. just kidding rofl lmao but you will lose info #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # warning # # # warning # # # # # # warning # # # # # # warning # # # # # # warning # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


library(scales)

# from among the three categories, find the maximum disproportionate share
krig.grd$svccat <- c( 'gulf' , 'vietnam' , 'other' )[ apply( krig.grd[ , 3:5 ] , 1 , which.max ) ]

# save only that max
krig.grd$statistic <- apply( krig.grd[ , 3:5 ] , 1 , max )

# it's important to note that i've thrown out a lot of information here
krig.grd <- krig.grd[ , c( 'intptlon' , 'intptlat' , 'statistic' , 'svccat' ) ]

# do any points not make sense?
summary( krig.grd$statistic )

# yup, the minimum is below zero.
krig.grd$statistic <- pmax( 0 , krig.grd$statistic )


# from among the three categories, find the maximum disproportionate share
gam.grd$svccat <- c( 'gulf' , 'vietnam' , 'other' )[ apply( gam.grd[ , 3:5 ] , 1 , which.max ) ]

# save only that max
gam.grd$statistic <- apply( gam.grd[ , 3:5 ] , 1 , max )

# it's important to note that i've thrown out a lot of information here
gam.grd <- gam.grd[ , c( 'intptlon' , 'intptlat' , 'statistic' , 'svccat' ) ]

# again, do any points not make sense?
summary( gam.grd$statistic )

# another point below zero.
gam.grd$statistic <- pmax( 0 , gam.grd$statistic )

# our complex sample survey-computed statistics rely on categories,
# but the final map only shows the _highest_ disproportionate item
# from each puma.  for example,

# puma 300 is slightly disproportionately more gulf veterans
# and it's also near evenly-split between being
# slightly disproportionately less vietnam vets and
# slightly disproportionately less other era vets

# puma 101 is disproportionately more gulf vets too,
# but it has very heavily disproportionately fewer vietnam vets
# and has close to state-average veterans from other eras.
sas

# only the "disproportionately more" share variable gets retained
# in these predictions.  all other information gets thrown away.
# this is the nature of mapping categorical variables

# if you are intent on showing a multi-color gradient with all information,
# you can use the rgb() function, but fair warning:
# the values mush together quickly and your map will probably look like ass.
# i tried building color gradients to map multi-dimensional categorical values
# like the multi-category values in
ps
# but on the color gradient, the red/green/blue values on the palette tend to mush together.
# for example, on this plot right here..
plot( 1:5 , rep( 1 , 5 ) , cex = 3 , pch = 16 , col = mapply( rgb , ps[ , 2 ] , ps[ , 3 ] , ps[ , 4 ] ) )
# red is gulf veterans, green is vietnam veterans, blue is other veterans.
# the colors end up just looking drab.

# even when re-scaled..
rsps <- apply( ps[ , 2:4 ] , 2 , rescale )
# ..the points with high relative rates in two categories (because they're the lowest in the third)
# have a lot of mixture (puma 300) and are therefore indecipherable.  what is the color brown here?
plot( 1:5 , rep( 1 , 5 ) , cex = 3 , pch = 16 , col = mapply( rgb , rsps[ , 1 ] , rsps[ , 2 ] , rsps[ , 3 ] ) )
# high vietnam era and also high gulf era service.
text( 1:5 , rep( 1.2 , 5 ) , ps[ , 1 ] )
# multi-dimensional categorical variable coloring is a nightmare.

# you have to simplify it.
# simplifying it means throwing out information.

# now where were we?


library(RColorBrewer)

# draw three gradients
tg <-
	lapply( 
		brewer.pal( 3 , 'Set1' ) , 
		function( z ) colorRampPalette( c( 'white' , z ) )( 101 )
	)

# check out each of these three colors, mapped from opaque to intense.
plot( rep( 0:100 , 3 ) , rep( c( -1 , 0 , 1 ) , each = 101 ) , col = unlist( tg ) , pch = 16 , cex = 3 )

# draw an alternate three gradients
# that start at ~20% ( that is: 25 / 125 )
# and also use a different palette from colorbrewer2.org
tag <-
	lapply( 
		brewer.pal( 3 , 'Dark2' ) , 
		function( z ) colorRampPalette( c( 'white' , z ) )( 125 )[ 25:125 ]
	)

# check out each of these three colors, mapped from opaque to intense.
plot( rep( 0:100 , 3 ) , rep( c( -1 , 0 , 1 ) , each = 101 ) , col = unlist( tag ) , pch = 16 , cex = 3 )


# # rescale both of the interpolated grids
krig.grd$statistic <- krig.grd$statistic * ( 1 / max( krig.grd$statistic ) )
gam.grd$statistic <- gam.grd$statistic * ( 1 / max( gam.grd$statistic ) )
# note that the re-scaling gets done across all categories,
# and not individually within each category.

# add the hex color identifier
krig.grd$color.value <- 
		ifelse( krig.grd$svccat == 'gulf' , tg[[1]][ round( krig.grd$statistic * 100 ) ] ,
		ifelse( krig.grd$svccat == 'vietnam' , tg[[2]][ round( krig.grd$statistic * 100) ] ,
		ifelse( krig.grd$svccat == 'other' , tg[[3]][ round( krig.grd$statistic * 100 ) ] , 
			NA ) ) )

# awwwwwwww yeah, something's happening now.
plot( krig.grd$intptlon , krig.grd$intptlat , col = krig.grd$color.value , pch = 16 , cex = 3 )

# add the alternate hex color identifier
krig.grd$alt.color <- 
		ifelse( krig.grd$svccat == 'gulf' , tag[[1]][ round( krig.grd$statistic * 100 ) ] ,
		ifelse( krig.grd$svccat == 'vietnam' , tag[[2]][ round( krig.grd$statistic * 100) ] ,
		ifelse( krig.grd$svccat == 'other' , tag[[3]][ round( krig.grd$statistic * 100 ) ] , 
			NA ) ) )

# that looks a bit better to me
plot( krig.grd$intptlon , krig.grd$intptlat , col = krig.grd$alt.color , pch = 16 , cex = 3 )


# lower-bound the alternate color to remove the white lines
krig.grd$bound.color <- 
		ifelse( krig.grd$svccat == 'gulf' , tag[[1]][ pmax( 5 , round( krig.grd$statistic * 100 ) ) ] ,
		ifelse( krig.grd$svccat == 'vietnam' , tag[[2]][ pmax( 5 , round( krig.grd$statistic * 100) ) ] ,
		ifelse( krig.grd$svccat == 'other' , tag[[3]][ pmax( 5 , round( krig.grd$statistic * 100 ) ) ] , 
			NA ) ) )

# that's smoothing by hand for you.
plot( krig.grd$intptlon , krig.grd$intptlat , col = krig.grd$bound.color , pch = 16 , cex = 3 )


# put that color band on the `gam.grd` data.frame as well
gam.grd$bound.color <- 
		ifelse( gam.grd$svccat == 'gulf' , tag[[1]][ pmax( 5 , round( gam.grd$statistic * 100 ) ) ] ,
		ifelse( gam.grd$svccat == 'vietnam' , tag[[2]][ pmax( 5 , round( gam.grd$statistic * 100) ) ] ,
		ifelse( gam.grd$svccat == 'other' , tag[[3]][ pmax( 5 , round( gam.grd$statistic * 100 ) ) ] , 
			NA ) ) )


# # end of step 8 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # #
# # step 9: ggplot and choose options # #

library(ggplot2)
library(mapproj)
library(scales)


# initiate the krige-based plot
krig.grd$color.column <- as.factor( krig.grd$bound.color )

krg.plot <- 
	ggplot( data = krig.grd , aes( x = intptlon , y = intptlat ) ) +
	geom_point( shape = 15 , colour = krig.grd$color.column ) +
	scale_fill_manual( values = unique( krig.grd$bound.color ) )


# initiate the gam-based plot
gam.grd$color.column <- as.factor( gam.grd$bound.color )

gam.plot <- 
	ggplot( data = gam.grd , aes( x = intptlon , y = intptlat ) ) +
	geom_point( shape = 15 , colour = gam.grd$color.column ) +
	scale_fill_manual( values = unique( gam.grd$bound.color ) )

# view both grids!
krg.plot
gam.plot


# initiate the entire plot
the.plot <-

	# choose only one of the two interpolation grids
	krg.plot +
	# gam.plot +
	
	# blank out the legend and axis labels
	theme(
		legend.position = "none" ,
		axis.title.x = element_blank() ,
		axis.title.y = element_blank()		
	) + 
	
	xlab( "" ) + ylab( "" ) +

	# force the x and y axis limits at the shape of the city and don't do anything special for off-map values
	scale_x_continuous( limits = c( -191 , -127 ) , breaks = NULL , oob = squish ) +
	# since we're going to add lots of surrounding-area detail!
    scale_y_continuous( limits = c( 50 , 73 ) , breaks = NULL , oob = squish ) +

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


# initiate an aleutian islands-focused wrap-around function
s360 <- function( z ){ z[ z$long > 0 , 'long' ] <- z[ z$long > 0 , 'long' ] - 360 ; z }


# # alaskan state borders # #

# convert the alaskan borders to longlat,
# prepare for ggplot2 with `fortify`
# wrap edge points around
ab <- s360( fortify( spTransform( alaska.borders , CRS( "+proj=longlat" ) ) ) )

# store this information in a layer
state.border.layer <- geom_path( data = ab , aes( x = long , y = lat , group = group ) , colour = 'darkgrey' )

# plot the result
the.plot + state.border.layer


# # alaskan main roads # #

# convert the alaskan borders to longlat,
# prepare for ggplot2 with `fortify`
# wrap edge points around
akr <- s360( fortify( spTransform( asf[[3]] , CRS( "+proj=longlat" ) ) ) )

# store this information in a layer
state.roads.layer <- geom_path( data = akr , aes( x = long , y = lat , group=group ) , colour = 'darkred' )

# plot the result
the.plot + state.border.layer + state.roads.layer

# # end of step 9 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # #
# # step 10: project, blank, and save # #

library(ggplot2)
library(scales)
library(raster)
library(plyr)
library(rgeos)


# exclude outer alaska if you hate the wilderness or something
the.plot + state.border.layer + coord_cartesian( xlim = c( -155 , max( x$intptlon ) ) , ylim = c( min( x$intptlat ) , 70 ) )

# distort the map with simple latitude/longitude scaling
the.plot + state.border.layer + coord_fixed( 2.5 )

# this looks crappy, who knows what it is
the.plot + state.border.layer + coord_equal()

# check out a bunch of other options #
the.plot + state.border.layer + coord_map( project = "cylequalarea" , mean( x$intptlat ) )

# here's the one that makes the most sense for alaska
the.plot + state.border.layer + coord_map( project = "conic" , mean( x$intptlat ) , orientation = c( 90 , 0 , -141 ) )

# see ?mapproject and the ?coord_* functions for a zillion alternatives

# store this projection, but not the state border
the.plot <- the.plot + coord_map( project = "conic" , mean( x$intptlat ) , orientation = c( 90 , 0 , -141 ) )
# into `the.plot`


# force the difference shapefile's projection
proj4string( ak.shp.diff ) <- "+init=epsg:2163"

# initiate the outside blanking layer
outside <- s360( fortify( spTransform( ak.shp.diff , CRS( "+proj=longlat" ) ) ) )

# fix islands piecing together
outside2 <- ddply( outside , .( piece ) , function( x ) rbind( x , outside[ 1 , ] ) )

# convert this fortified object to a ggplot layer
outside.layer <- geom_polygon( data = outside2 , aes( x = long , y = lat , group = id ) , fill = 'white' )

# plot this -- the layer doesn't work, does it?
the.plot + outside.layer

# five points need to change so we have a real bounding box.
subset( outside , lat < 45 | lat > 75 | long < -190 | long > -125 )

# move all of them counter-clockwise by hand
outside[ outside$order %in% c( 1 , 5 ) , 'long' ] <- -116.6568
# outside[ outside$order %in% c( 1 , 5 ) , 'lat' ] <- 20

# outside[ outside$order %in% 4 , 'long' ] <- -220
outside[ outside$order %in% 4 , 'lat' ] <- 37.56767

outside[ outside$order %in% 3 , 'long' ] <- -195.4295
# outside[ outside$order %in% 3 , 'lat' ] <- 100

# outside[ outside$order %in% 2 , 'long' ] <- -100
outside[ outside$order %in% 2 , 'lat' ] <- 79.36447


# fix islands piecing together
outside2 <- ddply( outside , .( piece ) , function( x ) rbind( x , outside[ 1 , ] ) )

# convert this fortified object to a ggplot layer
outside.layer <- geom_polygon( data = outside2 , aes( x = long , y = lat , group = id ) , fill = 'white' )

# plot this. 
the.plot + outside.layer
# that's not so bad, i guess.

# i don't care for the state border layer,
# but if you want the state border layer,
# use this save line:
final.plot <- the.plot + outside.layer + state.border.layer
# otherwise use this save line:
# final.plot <- the.plot + outside.layer
# you can airbrush the outside blue border
# in microsoft paint or something
# if you want, right? like a boss.


# save the file to your current working directory
ggsave( 
	"2013 alaskan veteran service eras.png" ,
	plot = final.plot ,
	scale = 3
)
# happy?

# # end of step ten # #
# # # # # # # # # # # #
