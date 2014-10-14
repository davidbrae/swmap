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
