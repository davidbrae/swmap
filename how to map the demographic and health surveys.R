# # # # # # # # # # # # # # # # #
# # set the working directory # #
# # # # # # # # # # # # # # # # #

# setwd( "C:/My Directory/SWMAP/" )


# # # # # # # # # # # # # # # #
# # example survey data set # #
# # # # # # # # # # # # # # # #

# demographic and health surveys


# # # # # # # # # # # # # # # # # # # # #
# # different from other maps because # #
# # # # # # # # # # # # # # # # # # # # #

# uses the prevR package, which automates the interpolation
# kernel density estimation with adaptive bandwidth, rather than kriging


# # # # # # # # # # # # # # # # # #
# # smallest level of geography # #
# # # # # # # # # # # # # # # # # #

# exact interview locations within the country


# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # asdfree.com blog post for this survey microdata # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# http://www.asdfree.com/search/label/demographic%20and%20health%20surveys%20%28dhs%29


# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # r code repository for setup and analysis examples # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# https://github.com/ajdamico/usgsd/tree/master/Demographic%20and%20Health%20Surveys


# # # # # # # # # # # # #
# # value of interest # #
# # # # # # # # # # # # #

# infant mortality


# # # # # # #
# # flaws # #
# # # # # # #

# egypt's nile-dwelling population can distort estimates
# requires microdata with gps coordinates, often not available


# # # # # # # # # # # # # # # # # # # # #
# # step 1: load the survey microdata # #

library(downloader)

# download the 2008 egyptian demographic and health survey microdata onto the local disk
# note that this requires (free) registration before the download will work
# http://dhsprogram.com/data/Access-Instructions.cfm
your.username <- "username"
your.password <- "password"
your.project <- "project"
source_url( "https://raw.github.com/ajdamico/usgsd/master/Demographic%20and%20Health%20Surveys/download%20and%20import.R" , prompt = FALSE , echo = TRUE )


# # end of step 1 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 2: calculate interview cluster-specific values # #

library(foreign)
library(sqldf)

# load the children's recode file as a data.frame `ch`
ch <- read.dta( "./Egypt/Standard DHS 2008/Children's Recode/stata/EGKR5AFL.DTA" )

# retain only the columns necessary for the analysis
# v001 cluster number
# v005 Sample weight
# b2 year of birth
# b5 child is alive
# b6 age at death (1xx in days, 2xx in months, 3xx in years)
ch <- ch[ , c( 'v001' , 'v005' , 'b2' , 'b5' , 'b6' ) ]

# limit the sample to only children born between 2004 and 2008
# with a non-missing age at death
ch <- subset( ch , b2 %in% 2004:2008 & !( b6 %in% 997:999 ) )
# that is, in the past three years.

# create a binary infant mortality variable
ch$im <- as.numeric( ch$b5 %in% 'no' & ch$b6 %in% 100:301 )

# create weigth variable
ch$w <- ch$v005 / 1000000

# note that this is very close to (but not exactly the same as)
# the nationwide egyptian infant mortality rate given on their report
# table 10.1 http://dhsprogram.com/pubs/pdf/FR220/FR220.pdf#page=148
weighted.mean( ch$im , ch$w ) * 1000
# their report says 24.5 per 1,000.
# the current microdata shows 24.2 per 1,000.  big whoop.

# calculate four statistics, grouped by survey cluster
# count, weighted count, infant deaths, weighted infant deaths
cl <- sqldf( "select v001 as dhsclust , count(*) as denom , sum( im ) as numer , sum( w ) as wdenom , sum( im * w ) as wnumer from ch group by v001" )

# that was easy, huh?  want to look at your resultant cluster-level information?
head( cl )

tail( cl )

# # end of step 2 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # #
# # step 3: prepare your prevR object # #

library(prevR)
library(foreign)

# what country are you mapping?  egypt?
# oh, okay.  well then let's grab egypt's boundaries.
bounds <- create.boundary( "Egypt" )
# that was it?  that was too easy.

# import the longitude and latitude data from the
# geographical information system (gis) files (this is a special request)
longlat <- read.dbf( "./Egypt/Standard DHS 2008/Supplemental/flat/EGGE5DFL.dbf" )
	
# convert all column names to lowercase
names( longlat ) <- tolower( names( longlat ) )

# merge this cluster information onto the cluster-level results data.frame
x <- merge( cl , longlat[ , c( 'dhsclust' , 'longnum' , 'latnum' , 'source' ) ] )

# confirm that every cluster that you have infant mortality information for
# also has a longitude & latitude variable now
stopifnot( nrow( x ) == nrow( cl ) )

# check how many clusters are missing coordinates
miss.coord <- nrow( subset( x , source == 'MIS' ) )

# discard records with missing longitudes & latitudes
x <- subset( x , source != 'MIS' )

# confirm you've tossed the correct number of records
stopifnot( nrow( x ) + miss.coord == nrow( cl ) )

# identify which columns are integer types
ic <- sapply( x , is.integer )

# coerce every integer column to numeric
x[ , ic ] <- sapply( x[ , ic ] , as.numeric )

# create a prevR object like a professional.
pro <- 
	as.prevR(
		x , 
		c( id = "dhsclust" , x = "longnum" , y = "latnum" , n = "denom" , wn = "wdenom" , pos = "numer" , wpos = "wnumer" ) , 
		bounds
	)

# want to take a first glance at the sampling clusters
# of the 2008 egypt demographic and health surveys?
plot( pro )
# woah.

# # end of step 3 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # #
# # step 4: make a simple map # #

# compute bandwidths
pro <- rings( pro , N = 1000 )

# compute surfaces
pro.map <- kde( pro , N = 1000 , nb.cells = 250 )

# plot a simple map comparing
# weighted and unweighted surfaces
spplot( pro.map )

# re-create a simple weighted surface,
# but with a prevR palette
spplot( 
	pro.map, 
	'k.wprev.N1000.RInf' , 
	cuts = 100 , 
	col.regions = prevR.colors.red( 101 ) , 
	main = "regional trends of infant mortality"
)

# # end of step 4 # #
# # # # # # # # # # #


# # # # # # # # # # # #
# # step 5: outline # #

library(rgeos)
library(raster)
library(rgdal)
library(ggplot2)
library(downloader)

# load the download.cache and related functions
# to prevent re-downloading of files once they've been downloaded.
source_url(
	"https://raw.github.com/ajdamico/usgsd/master/Download%20Cache/download%20cache.R" ,
	prompt = FALSE ,
	echo = FALSE
)

# # note on outline geography selection
# # i am intentionally using sites that host
# # data from many/most/every country worldwide
# # so that this script can be easily extended
# # to whatever country you're working on ;)

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
world.shp <- spTransform( world.shp , CRS( "+proj=longlat" ) )

# here's the outline of every country in the world
plot( world.shp , fill = 'gray' )

# # # map of the nile # # #

# use ucdavis's map of egypt's waterways
ucd.fn <- "http://biogeo.ucdavis.edu/data/diva/wat/EGY_wat.zip"

# store it to the local disk
download.cache( ucd.fn , tf )

# unzip it
ucd.uz <- unzip( tf , exdir = tempdir() )

# this file contains lots of information.
ucd.uz

# identify the waterways shapefile
waterways.sfn <- grep( "water_areas(.*)shp$" , ucd.uz , value = TRUE )

# read it in
waterways.shp <- readOGR( waterways.sfn  , layer = gsub( "\\.shp" , "" , basename( waterways.sfn ) ) )
waterways.shp <- spTransform( waterways.shp , CRS( "+proj=longlat" ) )

# here's the outline of all water in egypt
plot( waterways.shp )

# here's the outline of only the nile + lakes + suez canal
plot( subset( waterways.shp , grepl( "LAKE|CANAL|NILE" , NAME ) ) )

# # # keep going into sudan # # #

# use ucdavis's map of sudan's waterways
sucd.fn <- "http://biogeo.ucdavis.edu/data/diva/wat/SDN_wat.zip"

# store it to the local disk
download.cache( sucd.fn , tf )

# unzip it
sucd.uz <- unzip( tf , exdir = tempdir() )

# this file contains lots of information.
sucd.uz

# identify the waterways shapefile
sudanwater.sfn <- grep( "water_areas(.*)shp$" , sucd.uz , value = TRUE )

# read it in
sudanwater.shp <- readOGR( sudanwater.sfn  , layer = gsub( "\\.shp" , "" , basename( sudanwater.sfn ) ) )
sudanwater.shp <- spTransform( sudanwater.shp , CRS( "+proj=longlat" ) )

# here's the outline of all water in sudan
plot( sudanwater.shp )

# here's the outline of the nile into sudan
plot( subset( sudanwater.shp , grepl( "NILE|LAKE" , NAME ) ) )


# # # map of egyptian states # # #

# use uc davis's administrative regions
admin.fn <- "http://biogeo.ucdavis.edu/data/diva/adm/EGY_adm.zip"

# store it to the local disk
download.cache( admin.fn , tf )

# unzip it
admin.uz <- unzip( tf , exdir = tempdir() )

# this file contains a few different levels
# of administrative borders.
admin.uz

# identify the national and state border shapefiles
nation.sfn <- grep( "adm0(.*)shp$" , admin.uz , value = TRUE )
states.sfn <- grep( "adm1(.*)shp$" , admin.uz , value = TRUE )

# read in both
nation.shp <- readOGR( nation.sfn  , layer = gsub( "\\.shp" , "" , basename( nation.sfn ) ) )
states.shp <- readOGR( states.sfn  , layer = gsub( "\\.shp" , "" , basename( states.sfn ) ) )

nation.shp <- spTransform( nation.shp , CRS( "+proj=longlat" ) )
states.shp <- spTransform( states.shp , CRS( "+proj=longlat" ) )

# # ready to stack all four maps?

# calculate the bounding box of egypt, 10% bigger than the country
bb <- bbox( as( 1.05 * extent( states.shp ) , "SpatialPolygons" ) )

# initiate the lowest layer (the world)
plot( world.shp , xlim = bb[ 1 , ] , ylim = bb[ 2 , ] , col = 'gray' , fill = TRUE , border = 'white' )

# turn gray off for egypt only, then add state boundaries in gray
plot( states.shp , add = TRUE , col = 'white' , fill = TRUE , border = 'gray'  )

# add the nile
plot( subset( waterways.shp , grepl( "NILE" , NAME ) ) , add = TRUE , col = 'lightblue' , border = 'lightblue' )

# you can add the national border in black if you want
plot( nation.shp , add = TRUE , border = 'black' )
# but i don't think it's necessary

# # not bad for a start, huh?

# draw a rectangle 100% bigger than the original state
eg.shp.blank <- as( 1.5 * extent( states.shp ) , "SpatialPolygons" )

# draw a rectangle 0% bigger than the original state
eg.box <- as( extent( states.shp ) , "SpatialPolygons" )


# # worldwide coastlines # #

coast.fn <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_ocean.zip"

# store it to the local disk
download.cache( coast.fn , tf )

# unzip it
coast.uz <- unzip( tf , exdir = tempdir() )

# this file contains every coastline everywhere
coast.uz

# identify the worldwide ocean
coast.sfn <- grep( "ocean(.*)shp$" , coast.uz , value = TRUE )

# read in the ocean
coast.shp <- readOGR( coast.sfn  , layer = gsub( "\\.shp" , "" , basename( coast.sfn ) ) )

# put the ocean in longlat format
coast.shp <- spTransform( coast.shp , CRS( "+proj=longlat" ) )

# limit the egyptian coast shapefile
# to only points within the larger bounding box
eg.coast <- gIntersection( eg.shp.blank , coast.shp )
# so you're not carrying around the whole world's ocean

# one more note: `XK` is a disputed region.  see the difference?
plot( subset( world.shp , CNTR_ID %in% 'EG' ) )
plot( subset( world.shp , ( CNTR_ID %in% c( 'EG' , 'XK' ) ) ) )

# these are huge objects, so in order to conserve ram
# fortify what you need for ggplot2 and toss all other objects.
fcoast <- fortify( eg.coast ) ; rm( eg.coast )
# keep everything but the disputed region plus egypt proper
wshape <- fortify( subset( world.shp , !( CNTR_ID %in% c( 'EG' , 'XK' ) ) ) ) ; rm( world.shp )
fnation <- fortify( nation.shp )
fstate <- fortify( states.shp ) ; rm( states.shp )
fnile <- fortify( subset( waterways.shp , grepl( "NILE|LAKE|CANAL" , NAME ) ) ) ; rm( waterways.shp )
snile <- fortify( subset( sudanwater.shp , grepl( "NILE|LAKE" , NAME ) ) ) ; rm( sudanwater.shp )
# got 'em all, i think.  clear up RAM
gc()

# # end of step 5 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 6: re-create the prevR object on a rectangle # #

library(prevR)

# re-create a prevR object like a professional.
bbpro <- 
	as.prevR(
		x , 
		c( id = "dhsclust" , x = "longnum" , y = "latnum" , n = "denom" , wn = "wdenom" , pos = "numer" , wpos = "wnumer" ) , 
		as( eg.box , "SpatialPolygons" )
	)
# note the use of `nation.shp`
# which has slightly cleaner borders
# than the borders obtained from
# the create.boundary function

your.N <- 1000

# re-compute bandwidths
bbpro <- rings( bbpro , N = your.N )

# re-compute surfaces
bbpro.map <- kde( bbpro , N = your.N , nb.cells = 250 )

# coerce this result to a data.frame object
map.df <- na.omit( as.data.frame( bbpro.map ) )

# name your variable something less mathy
map.df$im <- map.df[ , paste0( "k.wprev.N" , your.N , ".RInf" ) ]

# sort and move on.
map.df <- map.df[ order( map.df$x , map.df$y ) , ]

# # end of step 6 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # #
# # step 7: decide on your map parameters # #

library(ggplot2)
library(scales)
library(mapproj)


# initiate the simple map
first.map <- 
	qplot( 
		x , 
		y , 
		data = map.df , 
		colour = im ,
		xlab = NULL ,
		ylab = NULL
	)

# look at that.. not bad not bad
first.map

# give this map a geom_tile layer
eg.map <-
	ggplot(
		map.df , 
		aes( x = x , y = y )
	) + geom_tile( aes( fill = im ) )

# set the bounding box limits that
# you and i agreed to earlier in the script
# oh, also, remove all map crap
eg.map <- 
	eg.map + 

	xlab( "" ) + ylab( "" ) +

	scale_x_continuous( limits = bb[ 1 , ] , breaks = NULL , oob = squish ) +

    scale_y_continuous( limits = bb[ 2 , ] , breaks = NULL , oob = squish ) +

    theme(
		legend.position = "none" ,
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		panel.background = element_blank(),
		panel.border = element_blank(),
		axis.ticks = element_blank()
	)

# cleaner still.
eg.map

# # end of step 7 # #
# # # # # # # # # # #


# # # # # # # # # # #
# # step 8: color # #

library(ggplot2)

eg.map + scale_fill_gradient( low = 'green' , high = 'red' )

eg.map + scale_fill_gradient( low = 'white' , high = 'blue' )

eg.map + scale_fill_gradient( low = muted( 'blue' ) , high = muted( 'red' ) )

# and some prevR-custom colors.
eg.map + scale_fill_gradientn( colours = prevR.colors.gray( 20 ) )

eg.map + scale_fill_gradientn( colours = prevR.colors.blue( 20 ) )

eg.map + scale_fill_gradientn( colours = prevR.colors.red( 20 ) )

# let's save that default
eg.map <- eg.map + scale_fill_gradientn( colours = prevR.colors.red( 20 ) )

# # end of step 8 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # #
# # step 9: draw borders and waters # #

library(plyr)
library(ggplot2)

# store this information in a layer
national.border.layer <- geom_path( data = fnation , aes( x = long , y = lat , group = group ) , colour = 'lightgray' )

# plot the result if you like
eg.map + national.border.layer

# store this information in a layer
state.border.layer <- geom_path( data = fstate , aes( x = long , y = lat , group = group ) , colour = 'lightgray' )

# plot the result if you like
eg.map + state.border.layer 

# # international borders # #

# store this information in a layer
international.border.layer <- geom_polygon( data = wshape , aes( x = long , y = lat , group = group ) , fill = 'lightgray' , color = 'white' )

# plot the result if you like
eg.map + international.border.layer

# # coastal areas to blank # #

# fix islands piecing together
fcoast2 <- ddply( fcoast , .( piece ) , function( x ) rbind( x , fcoast[ 1 , ] ) )

# convert this fortified object to a ggplot layer
ocean.layer <- geom_polygon( data = fcoast2 , aes( x = long , y = lat , group = id ) , fill = 'lightblue' )

eg.map + ocean.layer

# construct the nile layer
nile.layer <- 
	geom_polygon( 
		data = fnile , 
		aes( x = long , y = lat , group = group ) , 
		color = 'lightblue' , fill = 'lightblue'
	)
	
# everything you do with the egyptian nile,
# also do with the sudanese nile..
# to keep the river flowing off of the map
snile.layer <-
	geom_polygon( 
		data = snile , 
		aes( x = long , y = lat , group = group ) , 
		color = 'lightblue' , fill = 'lightblue'
	)


# closer, eh?
eg.map + nile.layer + snile.layer


# # external rectangle to blank # #

# initiate an external rectangle at the edges of the bounding box
# to blank out the furthest extent of the grid
orect <- 
	geom_rect( 
		xmin = bb[ 1 , 1 ] , 
		xmax = bb[ 1 , 2 ] , 
		ymin = bb[ 2 , 1 ] , 
		ymax = bb[ 2 , 2 ] , 
		color = 'white' , 
		fill = NA , 
		size = 4 
	)

eg.map + orect

# # end of step 9 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # #
# # step 10: project and save # #

library(ggplot2)
library(mapproj)

# save each of the layers,
# in order from bottom to top
final.map <-
	eg.map +
	international.border.layer +
	nile.layer + snile.layer +
	ocean.layer +
	orect

# here's the final plot
final.map

# save the file to your current working directory
ggsave( 
	"2004-2008 infant mortality rate - unprojected.png" ,
	plot = final.map
)
# but that's unprojected.  you might prefer a projected map.

# # # pick your projection # # #

# here are lots of choices.  choose wisely.
# final.map + coord_map( project = "albers" , lat0 = bb[ 2 , 1 ] , lat1 = bb[ 2 , 2 ] )
# final.map + coord_fixed()
# final.map + coord_cartesian()
# final.map + coord_map( "gilbert" )
# final.map + coord_map( "lagrange" )
# final.map + coord_map( "stereographic" )


# project this pup.  i prefer albers for egypt.
projected.map <- final.map + coord_map( project = "albers" , lat0 = bb[ 2 , 1 ] , lat1 = bb[ 2 , 2 ] )

# would you like to save this game?

# # # fair warning # # #
# the projected map takes hours to render.
# choose carefully from the shapes above,
# then leave this save command running overnight.

# save the projected plot, which takes longer doesn't it.
ggsave( 
	"2004-2008 infant mortality rate - projected.png" ,
	plot = projected.map
)

# # end of step ten # #
# # # # # # # # # # # #
