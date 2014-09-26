
# specify some population data to download
mydata <- "http://www.census.gov/popest/data/metro/totals/2012/tables/CBSA-EST2012-01.csv"

# load mydata
x <- read.csv( mydata , skip = 9 , h = F )

# keep only the GEOID and the 2010 population estimate
x <- x[ , c( 'V1' , 'V6' ) ]

# name the GEOID column to match the CBSA shapefile
# and name the weight column the weight column!
names( x ) <- c( 'GEOID10' , "weight" )

# throw out the bottom few rows
x <- x[ 1:950 , ]

# convert the weight column to numeric
x$weight <- as.numeric( gsub( ',' , '' , as.character( x$weight ) ) )

# now just make some fake trinary data
x$trinary <- c( rep( 0:2 , 316 ) , 0:1 )

# simple tabulation
table( x$trinary )

# so now the `x` data file looks like this:
head( x )

# and say we just wanted to map
# something easy like
# 0=red, 1=green, 2=blue,
# weighted simply by the population of the cbsa

# # # end of data read-in # # #

library(rgdal)
library(raster)
library(gstat)

# read in a base map
m <- getData("GADM", country="United States", level=1)
m <- m[!m$NAME_1 %in% c("Alaska","Hawaii"),]

# specify the tiger file to download
tiger <- "ftp://ftp2.census.gov/geo/tiger/TIGER2010/CBSA/2010/tl_2010_us_cbsa10.zip"

# create a temporary file and a temporary directory
tf <- tempfile() ; td <- tempdir()

# download the tiger file to the local disk
download.file( tiger , tf , mode = 'wb' )

# unzip the tiger file into the temporary directory
z <- unzip( tf , exdir = td )

# isolate the file that ends with ".shp"
shapefile <- z[ grep( 'shp$' , z ) ]

# read the shapefile into working memory
cbsa.map <- readOGR( shapefile, layer="tl_2010_us_cbsa10" )

# remove CBSAs ending with alaska, hawaii, and puerto rico
cbsa.map <- cbsa.map[ !grepl( "AK$|HI$|PR$" , cbsa.map$NAME10 ) , ]

# cbsa.map$NAME10 now has a length of 933
length( cbsa.map$NAME10 )

# extract centroid for each CBSA
cbsa.centroids <- data.frame(coordinates(cbsa.map), cbsa.map$GEOID10)
names(cbsa.centroids) <- c("lon","lat","GEOID10")

# add lat lon to popualtion data
nrow(x)
x <- merge(x, cbsa.centroids, by="GEOID10")
nrow(x) # centroids could not be assigned to all records for some reason

# create a raster object
r <- raster(nrow=500, ncol=500, 
            xmn=bbox(m)["x","min"], xmx=bbox(m)["x","max"],
            ymn=bbox(m)["y","min"], ymx=bbox(m)["y","max"],
            crs=proj4string(m))

# run inverse distance weighted model - modified code from ?interpolate...needs more research
model <- gstat(id = "trinary", formula = trinary~1, weights = "weight", locations = ~lon+lat, data = x,
               nmax = 7, set=list(idp = 0.5))
r <- interpolate(r, model, xyNames=c("lon","lat"))
r <- mask(r, m) # discard interpolated values outside the states

# project map for plotting (optional)
# North America Lambert Conformal Conic
nalcc <- CRS("+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
m <- spTransform(m, nalcc)
r <- projectRaster(r, crs=nalcc)

# plot map
par(mar=c(0,0,0,0), bty="n")
cols <- c(rgb(0.9,0.8,0.8), rgb(0.9,0.4,0.3),
          rgb(0.8,0.8,0.9), rgb(0.4,0.6,0.9),
          rgb(0.8,0.9,0.8), rgb(0.4,0.9,0.6))
col.ramp <- colorRampPalette(cols) # custom colour ramp
plot(r, axes=FALSE, legend=FALSE, col=col.ramp(100))
plot(m, add=TRUE) # overlay base map
legend("right", pch=22, pt.bg=cols[c(2,4,6)], legend=c(0,1,2), bty="n")

