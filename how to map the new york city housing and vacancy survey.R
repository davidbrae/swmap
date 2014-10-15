# # # # # # # # # # # # # # # # #
# # set the working directory # #
# # # # # # # # # # # # # # # # #

# setwd( "C:/My Directory/SWMAP/" )


# # # # # # # # # # # # # # # #
# # example survey data set # #
# # # # # # # # # # # # # # # #

# new york city housing and vacancy survey


# # # # # # # # # # # # # # # # # #
# # smallest level of geography # #
# # # # # # # # # # # # # # # # # #

# city boro and subboro areas
# (subboro areas are neighborhoods that map to collections of census tracts)


# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # asdfree.com blog post for this survey microdata # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# http://www.asdfree.com/search/label/new%20york%20city%20housing%20and%20vacancy%20survey%20%28nychvs%29


# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # r code repository for setup and analysis examples # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# https://github.com/ajdamico/usgsd/tree/master/New%20York%20City%20Housing%20and%20Vacancy%20Survey


# # # # # # # # # # # # #
# # value of interest # #
# # # # # # # # # # # # #

# ratio of persons per room


# # # # # # # # # # # # # # # # # # # # #
# # step 1: load the survey microdata # #

library(downloader)

# download the 2011 new york city housing and vacancy survey microdata onto the local disk
years.to.download <- 2011
source_url( "https://raw.github.com/ajdamico/usgsd/master/New%20York%20City%20Housing%20and%20Vacancy%20Survey/2002%20-%202011%20-%20download%20all%20microdata.R" , prompt = FALSE , echo = TRUE )

# # end of step 1 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 2: conduct your analysis of interest at the smallest geography allowed # #

library(survey)

# following the analysis examples in the r code repository --
# https://github.com/ajdamico/usgsd/blob/master/New%20York%20City%20Housing%20and%20Vacancy%20Survey/2011%20analysis%20examples.R
# -- calculate the persons per room rate at the smallest available geographic area


# note the large cautionary text that the standard errors for this survey are garbage #


# set R to produce conservative standard errors instead of crashing
# http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )
# this setting matches the MISSUNIT option in SUDAAN

# load the 2011 data files
load( 'nychvs11.rda' )

# create a fake survey design object with just the `hhweight`
# (household weight) variable
occ.d <- svydesign( ~1 , data = occ , weights = ~hhweight )
# this svydesign() call is incorrect because nychvs
# does not release its sampling clusters

# this script is useful as an example of how to map city-wide survey data
# but this actual new york city microdata has incorrect standard errors


# the persons per room variable has two decimals
occ.d <- update( occ.d , pproom = as.numeric( crowd100 ) / 100 )

# calculate the 2011 persons per room rate
svymean( ~ pproom , occ.d )

# # examine which geographies are available # #

# the new york city housing and vacancy survey identifies records
# from subboro (neighborhood) areas within all five boros
svytable( ~ borough + subboro , occ.d )

# simply use both of those geographies in the by= argument
# of the `svyby` command, and re-calculate the poverty rates
smallest.area.statistics <-
	svyby( 
		~ pproom , 
		~ borough + subboro , 
		occ.d , 
		svymean 
	)
# this is the same command as the city-wide calculation above,
# except these results have been broken into smaller areas.	

# these are the statistics to be mapped
print( smallest.area.statistics )
# the standard errors are a measure of precision,
# their inverse will serve as the mapping weights

# again!  don't forget that
# the new york city housing and vacancy survey's
# standard errors are not computable.

# make this object easier to type
sas <- smallest.area.statistics

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
# 2010 census summary file #1 for new york state
# then download the file.
sf1ny.tf <- tempfile()


download.cache( 
	"ftp://ftp2.census.gov/census_2010/04-Summary_File_1/New_York/ny2010.sf1.zip" ,
	sf1ny.tf ,
	mode = 'wb'
)


# create a temporary directory
td <- tempdir()


# unzip the summary file #1 files
sf1ny.uz <- unzip( sf1ny.tf , exdir = td )


# file layout from http://www.census.gov/prod/cen2010/doc/sf1.pdf#page=18
sf1ny <- read.fwf( sf1ny.uz[ grep( "nygeo2010" , sf1ny.uz ) ] , c( -8 , 3 , -16 , 2 , 3 , -22 , 6 , 1 , 4 , -253 , 9 , -9 , 11 , 12 ) )

# add columns names matching the census bureau, so it's easy to read
names( sf1ny ) <- c( "sumlev" , "state" , "county" , "tract" , "blkgrp" , "block" , "pop100" , "intptlat" , "intptlon" )

# summary level 101 has census tracts and census blocks
sf1ny.101 <- subset( sf1ny , sumlev == "101" )

# one record per census block in new york state.  see?  same number.
nrow( sf1ny.101 )
# https://www.census.gov/geo/maps-data/data/tallies/census_block_tally.html

# and guess what?  the total new york population matches as well.
sum( sf1ny.101$pop100 )
# http://quickfacts.census.gov/qfd/states/36000.html


# separately, read in the crosswalk between new york census tracts and nychvs subboro areas #
# http://www.census.gov/housing/nychvs/data/2011/11subcom1.pdf
# http://www.census.gov/housing/nychvs/data/2011/11subcom2.pdf
nycsb.tf <- tempfile()

download( "https://raw.githubusercontent.com/ajdamico/usgsd/master/New%20York%20City%20Housing%20and%20Vacancy%20Survey/boro%20and%20subboro%20to%20census%20tract%20crosswalk.csv" , nycsb.tf )

nychvs.subboro <- read.csv( nycsb.tf )

# ahh, we also need a county fips code to new york city borough match.
boro.to.county.fips <-
	data.frame( 
		boroname = c( 'Bronx' , 'Brooklyn' , 'Manhattan' , 'Queens' , 'Staten Island' ) , 
		county = c( 5 , 47 , 61 , 81 , 85 ) 
	)

# merge on the borough county fips codes	
bo <- merge( nychvs.subboro , boro.to.county.fips )

# confirm no record loss from the previous merge
stopifnot( nrow( bo ) == nrow( nychvs.subboro ) )

# rename the `bo` data.frame's census tract column to match `sf1ny.101`
names( bo )[ names( bo ) == 'ctract' ] <- 'tract'

# rename the `bo` data.frame's boro column to match `sas`
names( bo )[ names( bo ) == 'boro' ] <- 'borough'

# merge this with the new york state summary file #1..
sf1.bo <- merge( sf1ny.101 , bo )

# ..and guess what?  now we have a perfect match with new york city's 2010 population.
sum( sf1.bo$pop100 )
# http://quickfacts.census.gov/qfd/states/36/3651000.html

# so now we have a data.frame object with
# one record per census block,
# and also with the two geography-levels
# that match the new york city housing and vacancy survey
head( sf1.bo )

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
mrow <- nrow( merge( unique( sf1.bo[ , c( 'borough' , 'subboro' ) ] ) , sas ) )

# ..and it does/they do.
stopifnot( sas.row == mrow )

# now the census block-level new york city census data *could* merge if you wanted it to.


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
popsum <- aggregate( sf1.bo$pop100 , by = ( sf1.bo[ , c( 'borough' , 'subboro' ) ] ) , sum )

# make the column name meaningful
names( popsum )[ names( popsum ) == 'x' ] <- 'popsum'

# merge the popsum onto the sasfile
sas <- merge( sas , popsum )

# now.  merge
	# the persons per room rate (the variable of interest)
	# the inverted standard error (the total weight of the broad geography)
	# the population sum (the total population of all census blocks that are part of that geography)

x <- merge( sf1.bo , sas )

# confirm no record loss
stopifnot( nrow( x ) == nrow( sf1.bo ) )


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
x <- x[ , c( 'pproom' , 'weight' , 'intptlat' , 'intptlon' ) ]

# # end of step 4 # #
# # # # # # # # # # #

