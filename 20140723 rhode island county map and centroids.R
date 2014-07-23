
library(maptools)
library(foreign)

tf <- tempfile() ; td <- tempdir()

download.file( "ftp://ftp2.census.gov/geo/tiger/TIGER2011/COUNTY/tl_2011_us_county.zip" , tf , mode = 'wb' )

z <- unzip( tf , exdir = td )

# read in the entire county map of the united states
y <- readShapePoly( z[grep('shp$',z)] )

# limit it to rhode island
v <- subset( y , STATEFP == 44 )

# there are the five counties
plot( v )

# read in the dbf of the united states by count
x <- read.dbf( z[ grep('dbf$',z)])

# also limit that to rhode island
w <- subset( x , STATEFP == 44 )

# here are your five county centroids
w[ , c( 'INTPTLAT' , 'INTPTLON' ) ]

# make some fake data
a <- w[ , c( 'GEOID' , 'INTPTLAT' , 'INTPTLON' ) ]

a$VALUE <- seq( 0 , 50 , length.out = nrow( a ) )
a$SE <- c( 3 , 10 , 5 , 0 , 10 )

# here's some fake data to plot
a


