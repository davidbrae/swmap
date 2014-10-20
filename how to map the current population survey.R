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

# restrict the survey object to connecticut *plus adjacent state* records only
cpas.design <- 
	subset( 
		cps.design , 
		gestfips %in% c( 
			9 ,		# connecticut
			25 , 	# massachusetts
			36 , 	# new york
			44		# rhode island
		)
	)

# the original national survey object is no longer necessary
rm( cps.design ) ; gc()
# remove it and clear up RAM


# calculate the 2012 connecticut state-wide poverty rate
svymean( ~ as.numeric( povll %in% 1:3 ) , subset( cpas.design , gestfips == 9 & pov_univ == 1 ) )
# using only records in the poverty universe

# note: this estimate and standard error precisely matches
# the census bureau's table 19 (historical poverty by state)
# https://www.census.gov/hhes/www/poverty/data/historical/hstpov19.xls


# # examine which geographies are available # #

# the current population survey identifies records
# from six different core-based statistical areas (cbsa)
svytable( ~ gtcbsa , cpas.design )

# the current population survey identifies records
# from both metro and non-metro respondents
svytable( ~ gtmetsta , cpas.design )

# the current population survey identifies records
# from every state in the nation,
# but we've restricted this design to
# connecticut + adjacent states
svytable( ~ gestfips , cpas.design )

# the smallest geography reasonably extracted
# from this survey microdata set will be
# cbsa + metro status combined
svytable( ~ gtcbsa + gtmetsta + gestfips , cpas.design )

# simply use both of those geographies in the by= argument
# of the `svyby` command, and re-calculate the poverty rates
smallest.area.statistics <-
	svyby( 
		~ as.numeric( povll %in% 1:3 ) , 
		~ gtcbsa + gtmetsta + gestfips , 
		subset( cpas.design , pov_univ == 1 ) , 
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
# 2010 census summary file #1 for all four states
# then download the file.
sf1.tf <- tempfile()

# create two vectors with the names and abbreviations of all four states to download
sn <- c( "Connecticut" , "Massachusetts" , "New_York" , "Rhode_Island" )
sa <- c( "ct" , "ma" , "ny" , "ri" )

# create an empty data.frame
sf1.stack <- NULL

# loop through all four connecticut-adjacent states
for ( i in 1:4 ){

	# create a single-element character string containing the ftp path
	ftp.loc <- 
		paste0( 
			"ftp://ftp2.census.gov/census_2010/04-Summary_File_1/" ,
			sn[ i ] ,
			"/" ,
			sa[ i ] ,
			"2010.sf1.zip"
		)

	# download the current state's summary file
	download.cache( ftp.loc , sf1.tf , mode = 'wb' )
	# note: to re-download a file from scratch, add the parameter usecache = FALSE
	
	# unzip the summary file #1 files to the current working directory
	sf1.uz <- unzip( sf1.tf , exdir = tempdir() )

	# file layout from http://www.census.gov/prod/cen2010/doc/sf1.pdf#page=18
	sf1 <- 
		read.fwf( 
			sf1.uz[ grep( "geo2010" , sf1.uz ) ] , 
			c( -8 , 3 , -16 , 2 , 3 , -4 , 5 , -4 , 5 , -4 , 6 , 1 , 4 , -47 , 5 , -10 , 5 , -186 , 9 , -9 , 11 , 12 , -116 , 1 , 1 ) 
		)

	# add columns names matching the census bureau, so it's easy to read
	names( sf1 ) <- c( "sumlev" , "state" , "county" , "cousub" , "place" , "tract" , "blkgrp" , "block" , "cbsa" , "necta" , "pop100" , "intptlat" , "intptlon" , "memi" , "nmemi" )

	# summary level 101 has NECTA and census blocks
	sf1.101 <- subset( sf1 , sumlev == "101" )

	# stack all four states into one object
	sf1.stack <- rbind( sf1.stack , sf1.101 )

	# remove some data.frames and clear up RAM
	rm( sf1.101 , sf1 ) ; gc()
	
}

# just as a check, limit the summary file #1 to connecticut.
sf1ct <- subset( sf1.stack , state == 9 )

# one record per census block in connecticut.  see?  same number.
nrow( sf1ct )
# https://www.census.gov/geo/maps-data/data/tallies/census_block_tally.html

# and guess what?  the total connecticut population matches as well.
sum( sf1ct$pop100 )
# http://quickfacts.census.gov/qfd/states/09000.html


# so now we have a data.frame object with
# one record per census block,
# and also with the two geography-levels
# that match the current population survey
head( sf1.stack )
# in connecticut, rhode island, and massachusetts
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
sf1.merge <-
	
	transform(
	
		sf1.stack ,
	
		gestfips = state ,
	
		gtcbsa = 
			# if the record is in new york state,
			# check if the cbsa is in the current population survey, and if it is, keep it.
			ifelse( ( state %in% 36 ) & cbsa %in% unique( sas$gtcbsa ) , cbsa ,
			
			# if the record is in connecticut, rhode island, or massachusetts,
			# check if the new england city and town area (necta) is in the cps result,
			# and if it is, keep it.  otherwise zero-it.
			ifelse( !( state %in% 36 ) & necta %in% unique( sas$gtcbsa ) , necta , 0 ) ) ,
		
		gtmetsta = 
			# check whether to use the new england variable or not..
			ifelse( state %in% 36 ,
				
				# if the census block is metro, `gtmetsta` is a one.  otherwise it's a two.
				ifelse( memi == 1 , 1 , 2 ) ,
				
				ifelse( nmemi == 1 , 1 , 2 )
			)
		
	)
	
# confirm that we've created all possible geographies correctly.
merge( unique( sf1.merge[ , c( 'gtcbsa' , 'gtmetsta' , 'gestfips' ) ] ) , sas , all = TRUE )
# nope.  there are missings.

# recode anyone in new york state not in a cbsa to be non-metro
sf1.merge[ 
	sf1.merge$gestfips == 36 & 
	sf1.merge$gtmetsta == 1 & 
	sf1.merge$gtcbsa == 0 , 
	'gtmetsta' ] <- 2

# recode 100% of rhode island to be a part of the providence cbsa
sf1.merge[ sf1.merge$gestfips == 44 , 'gtcbsa' ] <- 77200
sf1.merge[ sf1.merge$gestfips == 44 , 'gtmetsta' ] <- 1

# recode connecticut residents in the
# worcester, ma or springfield, ma cbsas to unavailable cbsas
sf1.merge[ sf1.merge$gestfips == 9 & sf1.merge$gtcbsa %in% c( 78100 , 79600 ) , 'gtcbsa' ] <- 0


# the number of records in our small area statistics..
sas.row <- nrow( sas )

# ..should equal the number of unique-match-merged records..
mrow <- nrow( merge( unique( sf1.merge[ , c( 'gtcbsa' , 'gtmetsta' , 'gestfips' ) ] ) , sas ) )

# ..and it does/they do.
stopifnot( sas.row == mrow )

# now the census block-level connecticut+adjacent state census data
# *could* merge if you wanted it to.


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
popsum <- 
	aggregate( 
		sf1.merge$pop100 , 
		by = ( sf1.merge[ , c( 'gestfips' , 'gtcbsa' , 'gtmetsta' ) ] ) , 
		sum 
	)

# make the column name meaningful
names( popsum )[ names( popsum ) == 'x' ] <- 'popsum'

# merge the popsum onto the sasfile
sas <- merge( sas , popsum )

# now.  merge
	# the poverty rate (the variable of interest)
	# the inverted standard error (the total weight of the broad geography)
	# the population sum (the total population of all census blocks that are part of that geography)

x <- merge( sf1.merge , sas )

# confirm no record loss
stopifnot( nrow( x ) == nrow( sf1.merge ) )


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
x <- x[ , c( 'povrate' , 'weight' , 'intptlat' , 'intptlon' , 'gestfips' ) ]
# be sure to save the state identifier for easy subsets


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
		data = subset( x , gestfips == 9 ) , 
		colour = povrate ,
		xlab = NULL ,
		ylab = NULL
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

# if you like that projection, store it in the map object.
ct.map <- 
	ct.map + 
	coord_map( project = "albers" , lat0 = min( x$intptlat ) , lat1 = max( x$intptlat ) )


# check out some purty colors.
ct.map + scale_colour_gradient( low = 'green' , high = 'red' )

ct.map + scale_colour_gradient( low = 'white' , high = 'blue' )

ct.map + scale_colour_gradient( low = muted( 'blue' ) , high = muted( 'red' ) )

# notice how the dotted delineations match the census bureau's 2006 necta definitions
# http://www2.census.gov/geo/maps/metroarea/us_wall/Dec2006/necta_1206_large.gif

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

shpstate.tf <- tempfile()

# connnecticut borders the ocean,
# so use the census bureau's cartographic boundary files
# instead of the regular tiger shapefiles
# unless you want to display poverty rates in the ocean.

download.cache( 
	"http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_state_500k.zip" ,
	shpstate.tf ,
	mode = 'wb'
)

shpstate.uz <- unzip( shpstate.tf , exdir = tempdir() )

state.shp <- readShapePoly( shpstate.uz[ grep( 'shp$' , shpstate.uz ) ] )

ct.shp <- subset( state.shp , STATEFP == '09' )

# draw a rectangle 10% bigger than the original state
ct.shp.out <- as( 1.2 * extent( ct.shp ), "SpatialPolygons" )

# draw a rectangle 15% bigger than the original state
ct.shp.blank <- as( 1.3 * extent( ct.shp ), "SpatialPolygons" )

# compute the difference between connecticut and the rectangle 15% beyond the borders
ct.shp.diff <- gDifference( ct.shp.blank , ct.shp )

# # end of step 6 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # #
# # step 7: tie knots and krige # #

library(fields)
library(sqldf)

# how many knots should you make? #

# knots are the computationally-intensive part of this process,
# choose as many as your computer and your patience can handle.

# you should aim for between 100 - 999 knots,
# but numbers closer to 1,000 will overload smaller computers

# you could let the `fields` package attempt to guess knots for you,
# xknots <- cover.design( cbind( x$intptlon , x$intptlat ) , 100 )$design
# but with census microdata, you've already got easy access to a relevant geographic grouping

# the state of connecticut has 833 census tracts.
# https://www.census.gov/geo/maps-data/data/tallies/tractblock.html
# if you have a powerful computer, you could group based on weighted tracts
# however, to keep the processing requirements lower,
# i'll use county subdivisions

# for the knotting, note that adjacent states are no longer necessary,
# so subset the census summary file #1 to only connecticut and nearby census blocks.

# the sqldf() function doesn't like `.` in data.frame object names
sf1s <- sf1.stack

# within each county x county subdivision,
# calculate the population-weighted mean of the coordinates
ct.knots <- 
	sqldf( 
		"select 
			state , county , cousub ,
			sum( pop100 ) as pop100 , 
			sum( pop100 * intptlon ) / sum( pop100 ) as intptlon ,
			sum( pop100 * intptlat ) / sum( pop100 ) as intptlat
		from sf1s
		group by
			state , county , cousub"
	)
# note: this screws up coordinates that cross the international date line
# or the equator.  in the united states, only alaska's aleutian islands do this
# and those geographies will be thrown out later.  so it doesn't matter.

# how many knots have you gots?
nrow( ct.knots )
# too many, because you've included
# all of the adjacent states

# retain only knots within the bounding box
ct.knots <- 
	subset( 
		ct.knots ,
		( bbox( ct.shp.out )[ 1 , 1 ] < intptlon ) &
		( bbox( ct.shp.out )[ 2 , 1 ] < intptlat ) &
		( bbox( ct.shp.out )[ 1 , 2 ] > intptlon ) &
		( bbox( ct.shp.out )[ 2 , 2 ] > intptlat ) 
	)
	
# count again
nrow( ct.knots )
# that's more like it.

# you can look at the weighted centroids of those remaining tracts
plot( ct.knots$intptlon , ct.knots$intptlat )
# and look at that, bits of long island will be influencing our results
# since it's within a 15% range of the state of connecticut box

krig.fit <-
	Krig(
		cbind( x$intptlon , x$intptlat ) ,
		x$povrate ,
		weights = x$weight ,
		knots = cbind( ct.knots$intptlon , ct.knots$intptlat )
		# if you prefer to use cover.design, all you'd need is this knots= line instead:
		# knots = xknots
	)

# that is: what is the (weighted) relationship between
# your variable of interest (poverty rate) and
# the x/y points on a grid?

# check this out!
surface( krig.fit )
# you're almost there!


# here's an alternate approach using the `gam` function
library(mgcv)

gam.fit <- 
	gam( 
		povrate ~ s(intptlon , intptlat ) , 
		weights = weight , 
		data = x
	)
	

# for the third alternative, keep reading.
	
	
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


# do you want your map to print decently in a few minutes?
# grid.length <- 100
# or beautifully in a few hours?
grid.length <- 500


# create three identical grid objects
# grd <- gam.grd <- krig.grd <- 
	# expand.grid(
		# intptlon = seq( from = bbox( ct.shp )[1,1] , to = bbox( ct.shp )[1,2] , length = grid.length ) , 
		# intptlat = seq( from = bbox( ct.shp )[2,1] , to = bbox( ct.shp )[2,2] , length = grid.length )
	# )

grd <- gam.grd <- krig.grd <- smooth.grd <-
	expand.grid(
		intptlon = seq( from = x.range[1] , to = x.range[2] , length = grid.length ) , 
		intptlat = seq( from = y.range[1] , to = y.range[2] , length = grid.length )
	)


# along your rectangular grid,
# what are the predicted values of
# the poverty rate?
krig.grd$kout <- predict( krig.fit , krig.grd )

# alternate grid using gam.fit
gam.grd$gamout <- predict( gam.fit , gam.grd )


library(spatstat)
# alternate grid using smoothing
smooth.grd$smoout <- 
	Smooth(
		ppp( 
			x$intptlon , 
			x$intptlat , 
			summary( smooth.grd$intptlon )[ c( 1 , 6 ) ] ,
			summary( smooth.grd$intptlat )[ c( 1 , 6 ) ] ,
			marks = x$povrate
		) ,
		weights = x$weight ,
		at = "points"
	)

# # end of step 8 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 9: create a polygon layer to blank stuff # #






# # end of step 9 # #
# # # # # # # # # # #



stop( "capping your outliers is critically important.  the scale is much more visible if they are maxxed and minned" )



library(ggplot2)
library(scales)
library(mapproj)


outside <- fortify( ct.shp.diff )
# outside <- ct.shp.diff

# islands fix
library(plyr)
outside2 <- ddply(outside, .(piece), function(x)rbind(x, outside[1, ]))


# weighted.
krg.plot <- 
	ggplot( data = krig.grd , aes( x = intptlon , y = intptlat ) )
	geom_tile( data = krig.grd , aes( fill = kout ) )
	
gam.plot <- 
	ggplot( data = gam.grd , aes( x = intptlon , y = intptlat ) )
	geom_tile( data = gam.grd , aes( fill = gamout ) )

smooth.plot <- 
	ggplot( data = smooth.grd , aes( x = intptlon , y = intptlat ) )
	geom_tile( data = smooth.grd , aes( fill = smoout ) )



layer2 <- geom_polygon(data=outside2, aes(x=long,y=lat,group=id), fill='white')
co <- coord_map( project = "albers" , lat0 = min( x$intptlat ) , lat1 = max( x$intptlat ) )
# print this to a pdf instead, so it formats properly
# plot + layer1 + layer2 + co + scale_fill_gradient( low = muted( 'blue' ) , high = muted( 'red' ) )
# plot + layer1 + layer2 + scale_fill_gradient( low = muted( 'blue' ) , high = muted( 'red' ) )


# plot + layer1 + layer2 + co + scale_fill_gradient( low = 'white' , high = muted( 'red' ) )


co2 <- co
class(co2) <- c("hoge", class(co2))
is.linear.hoge <- function(coord) TRUE

p <-
	# krg.plot +
	# gam.plot +
	# smooth.plot +
	co + 
	layer1 + 
	layer2 + 
	coord_fixed() +
	scale_fill_gradient( low = 'green' , high = 'red' ) + 
	theme(
		legend.position = "none" ,
		axis.title.x = element_blank() ,
		axis.title.y = element_blank()		
	) + 
	scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
	theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		panel.background = element_blank(),
		panel.border = element_blank(),
		axis.ticks = element_blank()
	)

# plot that.
p



# weighted.
# plot <- ggplot(data = gam.grd, aes(x = intptlon, y = intptlat))  #start with the base-plot 
# layer1 <- geom_tile(data = gam.grd, aes(fill = kout ))  #then create a tile layer and fill with predicted values
# sol <- fortify( ct.shp )
# layer2 <- geom_path(data = sol, aes(long, lat), colour = "grey40", size = 1)
# co <- coord_map( project = "albers" , lat0 = min( x$intptlat ) , lat1 = max( x$intptlat ) )
# print this to a pdf instead, so it formats properly
# plot + layer1 + layer2 + co + scale_fill_gradient( low = muted( 'blue' ) , high = muted( 'red' ) )
# plot + layer1 + co + scale_fill_gradient( low = muted( 'blue' ) , high = muted( 'red' ) )



# raster plots

# coordinates(krig.grd) <- coordinates(gam.grd) <- c("intptlon", "intptlat")
# gridded(krig.grd) <- gridded(gam.grd) <- TRUE
# krig.r <- raster(krig.grd)
# gam.r <- raster(gam.grd)

# colRamp <- colorRampPalette(c(muted("blue"),muted("red")))
# plot(krig.r, axes=FALSE, col=colRamp(100), main="Krig")
# plot(ct.shp.diff, add=TRUE, col="white", border="white", lwd=5)
# degAxis(1)
# degAxis(2)
# box()

# plot(gam.r, axes=FALSE, col=colRamp(100), main="GAM")
# plot(ct.shp.diff, add=TRUE, col="white", border="white", lwd=5)
# degAxis(1)
# degAxis(2)
# box()


# water files for all state/county combos
ascc <- unique( sf1ct[ , c( 'state' , 'county' ) ] )

# location of all water files within the state of connecticut
water.files <-
	paste0( 
		"ftp://ftp2.census.gov/geo/tiger/TIGER2013/AREAWATER/tl_2013_" ,
		str_pad( ascc[ , 1 ] , 2 , pad = '0' ) ,
		str_pad( ascc[ , 2 ] , 3 , pad = '0' ) ,
		"_areawater.zip"
	)

watemp <- tempfile()

all.water <- NULL

for ( fn in water.files ){

	download.cache( fn , watemp )
	
	z <- unzip( watemp , exdir = tempdir() )

	w <- readShapePoly( z[ grep( 'shp$' , z ) ] )

	wo <- fortify( w )

	w2 <- ddply( wo , .( piece ) , function( x ) rbind( x , wo[ 1 , ] ) )

	wl <- geom_polygon( data = w2 , aes( x = long , y = lat , group = group ) , fill = 'white' )

	p <- p + wl
	
}




# print with all water blanked out.
p
