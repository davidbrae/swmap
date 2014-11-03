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

# multi-national
# approximate (unweighted) centroid calculations


# # # # # # # # # # # # # # # # # #
# # smallest level of geography # #
# # # # # # # # # # # # # # # # # #

# many regions across many countries


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

# average hours of television watched daily


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


# and now, in the blink of an eye, calculate the average hours of television consumed
# across almost every region available in the european social survey
smallest.area.statistics <- svyby( ~ tvtot , ~ region , integrated.design , svymean , na.rm = TRUE )

# these are the statistics to be mapped
print( smallest.area.statistics )
# the standard errors are a measure of precision,
# their inverse will serve as the mapping weights

# make this object easier to type
sas <- smallest.area.statistics

# trim the regions, and use the variable name
# that eurostat uses on its shapefiles
sas$NUTS_ID <- str_trim( sas$region )

# # end of step 2 # #
# # # # # # # # # # #


# # # # # # # # # # # #
# # step 3: outline # #

library(rgdal)

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
plot( matches.shp , col = 'red' )
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
plot( matches.shp , col = 'red' )
plot( world.shp , add = TRUE )
plot( alb , add = TRUE , col = 'red' )
plot( kos , add = TRUE , col = 'red' )
plot( ukr , add = TRUE , col = 'red' )
plot( rus , add = TRUE , col = 'red' )


# # end of step 3 # #
# # # # # # # # # # #










# calculate the centroids
alb.cen <- gCentroid( alb , byid = TRUE )
plot( alb.cen , add = TRUE )


kos.cen <- gCentroid( kos , byid = TRUE )

# calculate the centroids
ukr.cen <- gCentroid( ukr , byid = TRUE )

plot( ukr )
plot( ukr.cen , add = TRUE )


rus.cen <- gCentroid( rus , byid = TRUE )
plot(rus)



# not sure if you want to do this? #

# it would be a lot easier to just distribute the 1/se = weight across the centroids of each small administrative region
# so kaliningrad gets 1/8th of the north-west russia province



# regional populations by nuts3 #

# http://epp.eurostat.ec.europa.eu/portal/page/portal/population/data/database
# shows the tablename, and then the "bulk download" feature makes it easy to download
download.cache( "http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=data%2Fdemo_r_pjanaggr3.tsv.gz" , tf )

tf2 <- tempfile()
library(R.utils)
gunzip( tf , tf2 )
z <- read.table( tf2 , sep = '\t' , h = TRUE , stringsAsFactors = FALSE )
z$a <- sapply( strsplit( z[ , 1 ] , "," ) , '[[' , 1 )
z$b <- sapply( strsplit( z[ , 1 ] , "," ) , '[[' , 2 )
z$d <- sapply( strsplit( z[ , 1 ] , "," ) , '[[' , 3 )
z <- subset( z , a == "T" & b == "TOTAL" )

sum( as.numeric( as.character( z$X2013 ) ) , na.rm = TRUE ) / 4