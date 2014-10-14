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

# percent of the population with russian ancestry


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
batfile <- "./MonetDB/acs.bat"

# second: run the MonetDB server
pid <- monetdb.server.start( batfile )

# third: your five lines to make a monet database connection.
# just like above, mine look like this:
dbname <- "acs"
dbport <- 50001

monet.url <- paste0( "monetdb://localhost:" , dbport , "/" , dbname )
db <- dbConnect( MonetDB.R() , monet.url , wait = TRUE )


# # # # run your analysis commands # # # #

# analyze the 2011 single-year acs
load( './acs2012_1yr.rda' )

# open the design connection
acs.m <- open( acs.m.design , driver = MonetDB.R() , wait = TRUE )	# merged design

# restrict the acs.m object to alaska only
acs.m.alaska <- subset( acs.m , state == 2 )

# percent with russian ancestry, statewide
svymean( ~ I( anc1p == 148 | anc2p == 148 ) , acs.m.alaska )

# break that result out by public use microdata area (puma)
smallest.area.statistics <-
	svymean( ~ I( anc1p == 148 | anc2p == 148 ) , acs.m.alaska , byvar = ~ puma )

# these are the statistics to be mapped
print( smallest.area.statistics )
# the standard errors are a measure of precision,
# their inverse will serve as the mapping weights

# make this object easier to type..
sas <- smallest.area.statistics

# ..and also easier to read
names( sas )[ names( sas ) == 'transexp/totalexp' ] <- 'share'
names( sas )[ names( sas ) == 'se.transexp/totalexp' ] <- 'se'

# # end of step 2 # #
# # # # # # # # # # #
