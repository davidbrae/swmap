# just run these first three lines once to load the american housing survey on your computer..

# library(downloader)
# setwd( "C:/My Directory/AHS/" )
# source_url( "https://raw.github.com/ajdamico/usgsd/master/American%20Housing%20Survey/download%20all%20microdata.R" , prompt = FALSE , echo = TRUE )

# the three lines above will download all the data you need for this.. just let it run till it at least finishes 2011 and moves on to 2009


library(foreign)
library(survey)	# load survey package (analyzes complex design surveys)
library(GISTools)
library(maptools)


# example of how to calculate beautiful centroids -- in case you wanna play with it
# sids <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1], 
                      # proj4string=CRS("+proj=longlat +ellps=clrk66"))
# trueCentroids = gCentroid(sids,byid=TRUE)
# plot(sids)
# points(coordinates(sids),pch=1)
# points(trueCentroids,pch=2)



# R will exactly match SUDAAN results and Stata with the MSE option results
options( survey.replicates.mse = TRUE )
# otherwise if it is commented out or set to FALSE

# load the ahs microdata
load( "./2011/v1.4/tnewhouse_trepwgt.rda" )

# initiation of the replicate-weighted survey design object
ahs.design <-
	svrepdesign(
		weights = ~repwgt0,
		repweights = "repwgt[1-9]" ,
		type = "Fay" ,
		rho = ( 1 - 1 / sqrt( 4 ) ) ,
		data = tnewhouse_trepwgt
	)
	

# subset the survey design to occupied housing
occupied <- subset( ahs.design , status == 1 )

# calculate the nationwide share owner-occupied
svymean( ~ as.numeric( tenure == 1 ) , occupied )

# calculate the share owner-occupied for 29 msas
( occ.output <- svyby( ~as.numeric( tenure == 1 ) , ~ smsa , occupied , svymean , na.rm = TRUE ) )
# at the same time, print the results to the screen
oo <- data.frame( occ.output )
names( oo )[ 2 ] <- 'statistic'

# now you have a data frame with all smsas and the statistic and standard error
head( oo )

# read in the shapefiles of the metro area zones
ahs.zones <- readShapePoly( "./2011/metro_zones_shapefile/AHS2011_Zones.shp" )

# needs to be buffered, don't understand why
# http://r-sig-geo.2731867.n2.nabble.com/Unable-to-apply-gIntersection-successfully-td7240529.html
ahs.zones <- gBuffer( ahs.zones , byid = TRUE , width = 0 )

# zones are sub-msas, so combine them into the full msa region
ahs.smsas <- unionSpatialPolygons(ahs.zones, ahs.zones$SMSA)

# calculate the centroid of each msa
trueCentroids = gCentroid( ahs.smsas , byid = TRUE )

# see a map if you like..
plot(ahs.smsas)
points(coordinates(ahs.smsas),pch=3)
points(trueCentroids,pch=3)

# but here are the real 29 centroids
plot( trueCentroids )


# pull the available MSAs from the occ.output table
available.msas <- oo[ oo$smsa %in% ahs.zones$SMSA , ]

# re-sort the available.msas data frame so it matches the plot-centroid order
sorted.msas <- available.msas[ 	match( names( ahs.smsas ) , available.msas$smsa ) , ]


# here's your data!!!
cbind( sorted.msas , trueCentroids )
# hoooooraaaaaaaaaayyyyyyyyyyyyy



