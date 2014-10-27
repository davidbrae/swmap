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
plot( asf[[1]] ), add = TRUE )

# refresh the map with state borders only
plot( alaska.borders )

# add roads
plot( asf[[3]] , add = TRUE , col = 'red' )

# draw a rectangle 2.5% bigger than the original state
ak.shp.out <- as( 1.05 * extent( alaska.borders ) , "SpatialPolygons" )

# draw a rectangle 5% bigger than the original state
ak.shp.blank <- as( 1.1 * extent( alaska.borders ) , "SpatialPolygons" )

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
x.diff <- abs( x.range[ 2 ] - x.range[ 1 ] ) * 0.05
y.diff <- abs( y.range[ 2 ] - y.range[ 1 ] ) * 0.05

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


# # end of step 8 # #
# # # # # # # # # # #






plot <- ggplot( data = krig.grd , aes( x = intptlon , y = intptlat ) )

krig.grd$color.column <- as.factor( krig.grd$bound.color )

layer1 <- geom_point(shape=15,colour=krig.grd$color.column)

p <- plot + layer1 + scale_fill_manual( values = unique( krig.grd$alt.color ) )

s360 <- function( z ){ z[ z$long > 0 , 'long' ] <- z[ z$long > 0 , 'long' ] - 360 ; z }

ab <- spTransform( alaska.borders , CRS( "+proj=longlat" ) )
akpts <- fortify( ab )
akpts <- s360( akpts )

layer2 <- geom_path( data = akpts , aes( x = long , y = lat , group=group ) )

p+layer2

roads <- spTransform( asf[[3]] , CRS( "+proj=longlat" ) )
akroads <- fortify( roads )
akroads <- s360( akroads )
layer3 <- geom_path( data = akroads , aes( x = long , y = lat , group=group ) , colour = 'darkgray' )

p+layer2+layer3


library(plyr)
blank <- spTransform( ak.shp.diff , CRS( "+proj=longlat" ) )
outside <- fortify( blank )
outside <- s360( outside )

# five points need to change so we have a real bounding box.
subset( outside , lat < 45 | lat > 75 | long < -190 | long > -125 )

# move all of them counter-clockwise
outside[ round( outside$long , 4 ) == -160.1909 , 'lat' ] <- 50
outside[ round( outside$long , 4 ) == -160.1909 , 'long' ] <- -120
outside[ round( outside$long , 4 ) == -190.4537 , 'lat' ] <- 50
outside[ round( outside$lat , 5 ) == 76.14976 , 'long' ] <- -195
outside[ round( outside$long , 4 ) == -121.9465 , 'lat' ] <- 76.14976

outside2 <- ddply( outside , .( piece ) , function( x ) rbind( x , outside[ 1 , ] ) )



layer4 <- geom_polygon( data = outside2 , aes( x = long , y = lat , group = id ) , fill = 'white' )


# p+layer3+layer4+layer2
# ak.map <- p+layer4+layer2
ak.map <- p+layer4
# got it.

ak.map

# exclude outer alaska if you hate the wilderness or something
ak.map + coord_cartesian( xlim = c( -155 , max( x$intptlon ) ) , ylim = c( min( x$intptlat ) , 70 ) )

# distort the map with simple latitude/longitude scaling
ak.map + coord_fixed( 2.5 )

# this looks crappy
ak.map + coord_equal()

# check out a bunch of other options #
ak.map + coord_map( project = "cylequalarea" , mean( x$intptlat ) )

ak.map + coord_map( project = "conic" , mean( x$intptlat ) , orientation = c( 90 , 0 , -141 ) )

# see ?mapproject and the ?coord_* functions for a zillion alternatives







proj4string( ak.shp.diff ) <- proj4string( asf[[3]] )
asfill <- spTransform( ak.shp.diff , CRS( "+proj=longlat" ) )
asfi <- fortify( asfill )
layer4 <- geom_polygon( data = asfi , aes( x = long , y = lat , group=group ) , fill = 'white' )


p+layer4 + coord_cartesian( xlim = c( -155 , max( x$intptlon ) ) , ylim = c( min( x$intptlat ) , 70 ) )



	
	
layer2 <- geom_path( data = akpts , aes( x = long , y = lat , group=group ) )

gam.grd$color.column <- as.factor( gam.grd$alt.color )

plot <- ggplot( data = gam.grd , aes( x = intptlon , y = intptlat ) )
layer1 <- geom_point(shape=15,colour=gam.grd$color.column)
plot+layer1+  scale_fill_manual( values = unique( gam.grd$alt.color ) )+layer2


























library(plyr)
outside <- fortify( ak.shp.diff )
outside2 <- ddply( outside , .( piece ) , function( x ) rbind( x , outside[ 1 , ] ) )
layer2 <- geom_polygon( data = outside2 , aes( x = long , y = lat , group = id ) , fill = 'white' )

krig.grd$color.column <- as.factor( krig.grd$bound.color )
plot <- ggplot( data = krig.grd , aes( x = intptlon , y = intptlat ) )
layer1 <- geom_point(shape=15,colour=krig.grd$color.column)
plot+layer1+  scale_fill_manual( values = unique( krig.grd$bound.color ) )+layer2


# that's it.

# add some city names.
















# add the hex color identifier
gam.grd$color.value <- 
		ifelse( gam.grd$svccat == 'gulf' , tg[[1]][ round( gam.grd$statistic * 100 ) ] ,
		ifelse( gam.grd$svccat == 'vietnam' , tg[[2]][ round( gam.grd$statistic * 100) ] ,
		ifelse( gam.grd$svccat == 'other' , tg[[3]][ round( gam.grd$statistic * 100 ) ] , 
			NA ) ) )

# awwwwwwww yeah, something's happening now.
plot( gam.grd$intptlon , gam.grd$intptlat , col = gam.grd$color.value , pch = 16 , cex = 3 )


# # # # # # alternative coloring that starts at 25% # # # # # #

# add the hex color identifier
gam.grd$alt.color <- 
		ifelse( gam.grd$svccat == 'gulf' , tg[[1]][ pmax( 10 , round( gam.grd$statistic * 100 ) ) ] ,
		ifelse( gam.grd$svccat == 'vietnam' , tg[[2]][ pmax( 10 , round( gam.grd$statistic * 100) ) ] ,
		ifelse( gam.grd$svccat == 'other' , tg[[3]][ pmax( 10 , round( gam.grd$statistic * 100 ) ) ] , 
			NA ) ) )

# awwwwwwww yeah, something's happening now.
plot( gam.grd$intptlon , gam.grd$intptlat , col = gam.grd$alt.color , pch = 16 , cex = 3 )


# # end of step 8 # #
# # # # # # # # # # #




akpts <- fortify( ak.shp )
akpts[ akpts$long > 0 , 'long' ] <- 
	akpts[ akpts$long > 0 , 'long' ] - 360

	

# example of points alone
	
y <- x

y$statistic <- apply( y[ , 1:3 ] , 1 , max )

y$svccat <- c( 'gulf' , 'vietnam' , 'other' )[ apply( y[ , 1:3 ] , 1 , which.max ) ]

y$statistic <- y$statistic * ( 1 / max( y$statistic ) )
# y<-subset(y,statistic<.5)
# add the hex color identifier
y$color.value <- 
		ifelse( y$svccat == 'gulf' , tg[[1]][ round( y$statistic * 100 ) ] ,
		ifelse( y$svccat == 'vietnam' , tg[[2]][ round( y$statistic * 100) ] ,
		ifelse( y$svccat == 'other' , tg[[3]][ round( y$statistic * 100 ) ] , 
			NA ) ) )

# awwwwwwww yeah, something's happening now.
plot( y$intptlon , y$intptlat , col = y$color.value , pch = 16 )




# best below

layer2 <- geom_path( data = akpts , aes( x = long , y = lat , group=group ) )

gam.grd$color.column <- as.factor( gam.grd$alt.color )

plot <- ggplot( data = gam.grd , aes( x = intptlon , y = intptlat ) )
layer1 <- geom_point(shape=15,colour=gam.grd$color.column)
plot+layer1+  scale_fill_manual( values = unique( gam.grd$alt.color ) )+layer2


krig.grd$color.column <- as.factor( krig.grd$alt.color )
plot <- ggplot( data = krig.grd , aes( x = intptlon , y = intptlat ) )
layer1 <- geom_point(shape=15,colour=krig.grd$color.column)
plot+layer1+  scale_fill_manual( values = unique( krig.grd$alt.color ) )+layer2

# # # # best above.



# best below

layer2 <- geom_path( data = akpts , aes( x = long , y = lat , group=group ) )

plot <- ggplot( data = gam.grd , aes( x = intptlon , y = intptlat ) )
layer1 <- geom_point(shape=15,colour=gam.grd$color.column)
plot+layer1+  scale_fill_manual( values = unique( gam.grd$color.value ) )+layer2

plot <- ggplot( data = krig.grd , aes( x = intptlon , y = intptlat ) )
layer1 <- geom_point(shape=15,colour=krig.grd$color.column)
plot+layer1+  scale_fill_manual( values = unique( krig.grd$color.value ) )+layer2

# # # # best above.








plot <- ggplot( data = krig.grd , aes( x = intptlon , y = intptlat ) )
layer1 <- geom_point(shape=15,colour=krig.grd$color.column)
plot+layer1+  scale_fill_manual( values = unique( krig.grd$color.value ) )+layer2+coord_equal()


	
gam.grd$color.column <- as.factor( gam.grd$color.value )

plot <- ggplot( data = gam.grd , aes( x = intptlon , y = intptlat ) )
layer1 <- geom_tile( aes( fill = gam.grd$color.column ) )
layer2 <- geom_path( data = akpts , aes( x = long , y = lat , group = group ) , colour = 'black' )
plot + layer1 + layer2 + scale_fill_manual( values = unique( gam.grd$color.value ) )



krig.grd$color.column <- as.factor( krig.grd$color.value )

plot <- ggplot( data = krig.grd , aes( x = intptlon , y = intptlat ) )
layer1 <- geom_tile( aes( fill = krig.grd$color.column ) )
layer2 <- geom_path( data = akpts , aes( x = long , y = lat , group = group ) , colour = 'black' )
plot + layer1 + layer2 + scale_fill_manual( values = unique( krig.grd$color.value ) )



# this one???
krig.grd$color.column <- as.factor( krig.grd$color.value )

plot <- ggplot( data = krig.grd , aes( x = intptlon , y = intptlat ) )
layer1 <- geom_tile( aes( fill = krig.grd$color.column ) )
layer2 <- geom_path( data = akpts , aes( x = long , y = lat , group = group ) , colour = 'black' )
plot + layer1 + layer2 + scale_fill_manual( values = unique( krig.grd$color.value ) )















max.colors <- 
	mapply( 
		function( y , z ){ y[ round( z * 100 ) ] } , 
		tg , 
		tapply( krig.grd$statistic , krig.grd$svccat , max )
	)

krig.gulf <- subset( krig.grd , svccat == 'gulf' )
krig.vietnam <- subset( krig.grd , svccat == 'vietnam' )
krig.other <- subset( krig.grd , svccat == 'other' )
	
plot <- ggplot( data = krig.grd , aes( x = intptlon , y = intptlat ) )
layer1 <- geom_tile( aes( fill = as.numeric( krig.gulf$statistic ) ) ) + scale_fill_gradient( low = "#FFFFFF" , high = max.colors[ 1 ] )



# add a unique color-identifier to the data.frame




plot <- ggplot( data = krig.grd , aes( x = intptlon , y = intptlat ) )
layer1 <- geom_tile( aes( fill = krig.grd$color.column ) )
plot + layer1 + scale_fill_manual( values = unique( krig.grd$color.value ) ) 



ak.map <-
	qplot( 
		intptlon , 
		intptlat , 
		data = krig.grd , 
		colour = color.column
	)


# manually add the hex colors to the map
ak.map <- ak.map + scale_fill_manual( values = unique( krig.grd$color.value ) )

plot <- ggplot( data = krig.grd , aes( x = intptlon , y = intptlat ) )

layer1 <- geom_tile( aes( col = krig.grd$color.value ) )

plot + layer1 + scale_fill_identity( values =  )






outside <- fortify( ak.shp.diff )
















# # # # # # # # # # # # # # # # # # # # # # #
# # step 5: decide on your map parameters # #

library(ggplot2)
library(scales)
library(mapproj)

# before you ever touch surface smoothing or kriging,
# make some decisions about how you generally want
# your map to look:  the projection and coloring

# the options below simply use hadley wickham's ggplot2
# with the census block-level race/ethnicity shares and centroids

# re-scale the statistic column into another column
x$rsstat <- rescale( x$stat , from = c( 0 , max( x$stat ) ) )

# add the hex color identifier
x$color.value <- 
	rgb( 
		ifelse( x$svccat == 'gulf/vet' , x$rsstat , 0 ) ,
		ifelse( x$svccat == 'vietnam/vet' , x$rsstat , 0 ) ,
		ifelse( x$svccat == 'other/vet' , x$rsstat , 0 )
	)

x$color.column <- factor( x$color.value )
	
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

# see those brown dots?  those are just values lower on the red scale.
plot( 1:10 , rep( 1 , 10 ) , col = colorRampPalette( c( 'black' , 'red' ) )( 10 ) , cex = 3 , pch = 16 )
# on the gradient between black (no color) to red (its own primary color).
# yet another reason that coloring the multi-dimensional distributions of categorical variables
# is hard.

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

# exclude outer alaska if you hate the wilderness or something
ak.map + coord_cartesian( xlim = c( -155 , max( x$intptlon ) ) , ylim = c( min( x$intptlat ) , 70 ) )

# distort the map with simple latitude/longitude scaling
ak.map + coord_fixed( 2.5 )

# this looks crappy
ak.map + coord_equal()

# check out a bunch of other options #
ak.map + coord_map( project = "mercator" )

ak.map + coord_map( project = "cylequalarea" , mean( x$intptlat ) )

ak.map + coord_map( project = "conic" , mean( x$intptlat ) )

# see ?mapproject and the ?coord_* functions for a zillion alternatives

# # end of step 5 # #
# # # # # # # # # # #


















alaska.vps <- subset( alaska.design , vps > 0 )

# statewide distribution
sw <- svyratio( ~ vetcat + one , acs.m.alaska.vps , se = TRUE )

# puma-specific distributions
ps <- svytotal( ~ vetcat + one , acs.m.alaska.vps , byvar = ~ puma , se = TRUE )

ratio.estimates <- ps[ 1:15 ] / rep( ps[ 16:20 ] , each = 3 )


# disproportionate shares of veteran eras by puma

# # recode the variable using the "variable recode examples" script

# denominator: all military
# calculate the statewide ratios

# standard error solution!
# the square root of the variance for the state is the same for all comparisons
# it might draw the ire of a snotty academic statistician, but it's probably
# reasonably safe to only use the standard errors computed from the pumas
# since each gets compared to the same statewide value.


# # # # # # # # # # # # # # #
# ratio calculation example #
# # # # # # # # # # # # # # #
# calculate both the numerator and denominator of poverty, but not by state
num_den <- svytotal( ~ mult_nmorpob1 + dom_count_pes , valid.dom , se = TRUE )
# this gets computationally-intensive very fast. either write a loop to perform one state at a time,
# or leave your computer running for a week, or buy a bigger computer. ;)
# calculate the ratio estimate
ratio.estimate <- coef( num_den )[ 1 ] / coef( num_den )[ 2 ]
# print the ratio estimate to the screen
ratio.estimate
# calculate the variance of the ratio
vcov.num_den <- vcov( num_den )
variance.pob1 <-
( 1 / coef( num_den )[ 2 ] ^ 2 ) *
(
vcov.num_den[ 1 , 1 ] -
2 * coef( num_den )[ 1 ] / coef( num_den )[ 2 ] * vcov.num_den[ 1 , 2 ] +
( coef( num_den )[ 1 ] / coef( num_den )[ 2 ]) ^ 2 * vcov.num_den[ 2 , 2 ]
)
# print the standard error of the ratio to the screen
sqrt( variance.pob1 )
# since R's sqlsurvey package does not yet have `svyratio` capabilities
# this brute-force approach calculates a ratio and accompanying standard error











# september 2001 or later
svymean( ~I( mlpa == 1 ) , acs.m.alaska.mil , byvar = ~ puma )
# 1990 to august 2001 (including persian gulf war)
svymean( ~I( mlpb == 1 ) , acs.m.alaska.mil , byvar = ~ puma )
# august 1964 to april 1975 (vietnam era)
svymean( ~I( mlpe == 1 ) , acs.m.alaska.mil , byvar = ~ puma )




# # # after you compute the four categories,
# the colorings for this will be based on which areas
# have disproportionate shares.








library(RColorBrewer)

# three categories
# red, blue, green

# use multiple diverging palettes,
# *not* a qualitative palette.

# play with this number.
pl <- 7
# it can be three through eleven.

# combine the red-yellow-green and the red-yellow-blue palettes
twopal <- c( rev( brewer.pal( pl , "RdYlGn" ) ) , brewer.pal( pl  , "RdYlBu" ) )

# red is in the middle twice, so remove it
twopal <- twopal[ -round( length( twopal ) / 2 ) ]

# green is a 0.00
# red is 0.5
# blue is 1.00

# ultimately, we'll have to coerce our analysis results into a single linear variable

# initiate a color ramp palette function.
crp <- colorRampPalette( twopal )

# here's what the gradient looks like across one dot per color
plot( 1:( pl * 2 - 1 ) , rep( 1 , ( pl * 2 - 1 ) ) , col = crp( ( pl * 2 - 1 ) ) , pch = 16 , cex = 3 )

# here's what it looks like as a gradient
plot( 1:( 100 * ( pl * 2 - 1 ) ) , rep( 1 , 100 * ( pl * 2 - 1 ) ) , col = crp( 100 * ( pl * 2 - 1 ) ) , pch = 16 , cex = 3 )

# no reason this cannot be expanded to four or five categories.
# your map will start to get busy
# and not the good kind of getting busy





library(sqldf)
ctk <- sf1ak.101
ctk[ ctk$intptlon > 0 , 'intptlon' ] <-
	ctk[ ctk$intptlon > 0 , 'intptlon' ] - 360

ctract.knots <- 
	sqldf( 
		"select 
			county , tract ,
			sum( pop100 ) as pop100 , 
			sum( pop100 * intptlon ) / sum( pop100 ) as intptlon ,
			sum( pop100 * intptlat ) / sum( pop100 ) as intptlat
		from ctk
		group by
			county , tract"
	)
library(RANN)
a <- nn2( x[ , 1:3 ] )

# # # # # # # # # # # # # # # # # # # #
# # step 8: make a grid and predict # #

# use as fine of a grid as your computer can handle
grid.length <- 500

# again, adjust for the aleutian islands over the international date line
bb <- bbox( ak.shp )

bb[ 1 , 2 ] <- bb[ 1 , 2 ] - 360
# adjustment over.

# create two identical grid objects
# grd <- gam.grd <- 
	# expand.grid(
		# intptlon = seq( from = bb[1,1] , to = bb[1,2] , length = grid.length ) , 
		# intptlat = seq( from = bb[2,1] , to = bb[2,2] , length = grid.length )
	# )

grd <- gam.grd <- 
	expand.grid(
		intptlon = c( bb[1,1] , unique( ctract.knots$intptlon ) , bb[1,2] ) , 
		intptlat = c( bb[2,1] , unique( ctract.knots$intptlat ) , bb[2,2] )
	)

# along your rectangular grid,
# what are the predicted values of
# each race/ethnicity category?
gam.grd$aian <- predict( gam.aian , gam.grd[ , 1:2 ] )

gam.grd$white.nh <- predict( gam.white.nh , gam.grd[ , 1:2 ] )

gam.grd$all.others <- predict( gam.all.others , gam.grd[ , 1:2 ] )

# the gam() function occasionally predicts impossible values.
# confirm that did not happen.
sapply( gam.grd , summary )
# whoops, it did.

# note that some predictions are below zero or above one.  min and max these out
gam.grd[ c( 'aian' , 'white.nh' , 'all.others' ) ] <-
	sapply( 
		gam.grd[ c( 'aian' , 'white.nh' , 'all.others' ) ] ,
		function( z ) pmax( pmin( z , 1 ) , 0 )
	)
	
			

# since these predicted values do not sum to one (they need to!)
# calculate an expansion/contraction factor for each record
gam.grd$factor <- 1 / rowSums( gam.grd[ , c( 'aian' , 'white.nh' , 'all.others' ) ] )

# scale each predicted categorical share up or down, proportionally
gam.grd[ , c( 'aian' , 'white.nh' , 'all.others' ) ] <-
	sapply( 
		gam.grd[ , c( 'aian' , 'white.nh' , 'all.others' ) ] ,
		function( z ){ z * gam.grd$factor }
	)

# confirm that each row sums to one.
stopifnot( 
	all.equal( 
		rowSums( gam.grd[ , c( 'aian' , 'white.nh' , 'all.others' ) ] ) , 
		rep( 1 , nrow( gam.grd ) ) 
	) 
)

# # # # # 
kmc <- kmeans( gam.grd[ , c( 'aian' , 'white.nh' , 'all.others' ) ] , 99 )
gam.grd$kmc <- factor( kmc$cluster )

# centers of your clusters
kmc$centers

# three of each color, plus one white separator
# ( 3 x number of categories ) + ( number of categories minus one )

# which categories make the most sense?
# looking at the row.names

# order by aian
aian.33 <- row.names( kmc$centers[ order( -kmc$centers[,1] ) ,] )[ 1:50 ]
# top 33

# order by white non-hispanic
wnh.33 <- row.names( kmc$centers[ order( -kmc$centers[,2] ) ,] )[ 1:50 ]

# order by all others
ao.33 <- row.names( kmc$centers[ order( -kmc$centers[,3] ) ,] )[ 1:50 ]

aian.ni <- aian.33[ !( aian.33 %in% c( wnh.33 , ao.33 ) ) ]
wnh.ni <- wnh.33[ !( wnh.33 %in% c( aian.33 , ao.33 ) ) ]
ao.ni <- ao.33[ !( ao.33 %in% c( wnh.33 , aian.33 ) ) ]


# overlaps.. 9, 12
# not included.. 3, 11



# blue to white: 
# green to white: 

# # end of step 8 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 9: create a polygon to cover everything outside the boundary # #

library(rgeos)

# convert grd to SpatialPoints object
# coordinates( outer.grd ) <- c( "intptlon" , "intptlat" )

# draw a rectangle around the grd
# ak.shp.diff <- gEnvelope( outer.grd )
ak.shp.out <- gEnvelope( ak.shp )


# Create a bounding box 10% bigger than the bounding box of connecticut
# x_excess = (ak.shp@bbox['x','max'] - ak.shp@bbox['x','min'])*0.1
# y_excess = (ak.shp@bbox['y','max'] - ak.shp@bbox['y','min'])*0.1
# x_min = ak.shp@bbox['x','min'] - x_excess
# x_max = ak.shp@bbox['x','max'] + x_excess
# y_min = ak.shp@bbox['y','min'] - y_excess
# y_max = ak.shp@bbox['y','max'] + y_excess
# bbox = matrix(c(x_min,x_max,x_max,x_min,x_min,
                # y_min,y_min,y_max,y_max,y_min),
              # nrow = 5, ncol =2)
# bbox = Polygon(bbox, hole=FALSE)
# bbox = Polygons(list(bbox), "bbox")
# ak.shp.out = SpatialPolygons(Srl=list(bbox), pO=1:1, proj4string=ak.shp@proj4string)




# proj4string( ak.shp.diff ) <- projection
# ak.shp.diff <- spTransform( ak.shp.diff , CRS( projection ) )

# get the difference between your boundary and the rectangle
# ak.shp.diff <- gDifference( bbox , ak.shp )
ak.shp.diff <- gDifference( ak.shp.out , ak.shp )

# # end of step 9 # #
# # # # # # # # # # #



stop( "can you use scale_fill_gradientn() for this?  isn't that its purpose?  why aren't you using it?" )


stop( "capping your outliers is critically important.  the scale is much more visible if they are maxxed and minned" )



library(ggplot2)
library(scales)
library(mapproj)



# add the hex color identifier
gam.grd$color.value <- rgb( gam.grd$aian , gam.grd$white.nh , gam.grd$all.others )

# add a unique color-identifier to the data.frame
gam.grd$color.column <- factor( gam.grd$color.value )

outside <- fortify( ak.shp.diff )


# this one works for manual color filling.

stop( "i swore this worked before.  now it does not work." )


plot <- ggplot( data = gam.grd , aes( x = intptlon , y = intptlat ) )

plot <-
	plot + 

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


layer1 <- geom_tile( aes( fill = color.column ) )

plot + layer1 + scale_fill_manual( values = gam.grd$color.value ) 

layer1.clu <- geom_tile( aes( fill = kmc ) )

library(RColorBrewer)
# one qualitative color scheme
RdGy.11.p <- colorRampPalette( rev( brewer.pal( 8 , "Set1" ) ) )

plot + layer1 + scale_fill_gradientn( 


# + layer2

# layer2 <- geom_polygon(data=outside, aes(x=long,y=lat,group=group), fill='white')



stop( "don't forget about your promise in the early notes:" )

# i'll show an alternative using the rgb() function,
# but it looks like crap










