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

# median property value among owner-occupied homes


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
# -- calculate the transportation share of total expenditure at the smallest available geographic area



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

# open the design connection to the household-level design, not the person-level design
acs.h <- open( acs.h.design , driver = MonetDB.R() , wait = TRUE )

# restrict the acs.h object to alaska only
acs.h.alaska <- subset( acs.h , st == 2 )

# determine the unique pumas in alaska
alaska.pumas <- dbGetQuery( db , 'select distinct puma from acs2012_1yr_h where st = 2 order by puma' )

# there's the starting data.frame object
class( alaska.pumas )

# loop through this data.frame object with one record per alaskan puma
for ( i in seq( nrow( alaska.pumas ) ) ){

	# sqlsurvey's subsetting is funky and svyby() and byvar= do not work on svyquantile
	puma.ss.line <-
		paste( 
			"this.puma <- subset( acs.h.alaska , ( valp > 0 ) & ( puma = " , 
			alaska.pumas[ i , 'puma' ] , 
			") )"	
		)

	# execute the current string to create the alaskan household object,
	# subsetted to the current puma
	eval( parse( text = puma.ss.line ) )
		
	# calculate the median and standard error
	this.result <- svyquantile( ~ valp , this.puma , quantiles = 0.5 , se = TRUE )
		
	# store the median home value
	alaska.pumas[ i , 'mhv' ] <- as.numeric( this.result )
	
	# store the standard error
	alaska.pumas[ i , 'se' ] <- attr( this.result , 'ci' )[ 3 ]

}

# close the connection to the sqlrepsurvey design object
close( acs.h.alaska )
close( acs.h )

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
head( sf1ct.101 )

# and guess what?
# we've now got the census 2010 weighted populations (field pop100)
# and also each census block's centroid latitude & longitude (fields intptlat + intptlon)

# # end of step 3 # #
# # # # # # # # # # #



































# create a temporary file containing the census bureau's
# 2010 census tract shapefile for alaska
# then download the file.
shpak.tf <- tempfile() ; td <- tempdir()

download.cache( 
	"ftp://ftp2.census.gov/geo/tiger/TIGER2010/TRACT/2010/tl_2010_02_tract10.zip" ,
	shpak.tf ,
	mode = 'wb'
)

shpak.uz <- unzip( shpak.tf , exdir = td )

ak.shp <- readShapePoly( shpak.uz[ grep( 'shp$' , shpak.uz ) ] )




# from the census tract shapefile,
# we can merge on puma-level statistics and also
# calculate the total 2010 census population, by tract
sum( sf1ct.101$pop100 )
# http://quickfacts.census.gov/qfd/states/02000.html


# so now we have a data.frame object with
# one record per census block,
# and also with the two geography-levels
# that match the current population survey
head( sf1ct.101 )
# in connecticut,
# necta is the cbsa and
# nmemi indicates metropolitan status

# and guess what?
# we've now got the census 2010 weighted populations (field pop100)
# and also each census block's centroid latitude & longitude (fields intptlat + intptlon)

# # end of step 3 # #
# # # # # # # # # # #
