# # # # # # # # # # # # # # # # #
# # set the working directory # #
# # # # # # # # # # # # # # # # #

# setwd( "C:/My Directory/SWMAP/" )


# # # # # # # # # # # # # # # #
# # example survey data set # #
# # # # # # # # # # # # # # # #

# pesquisa nacional por amostra de domicilios


# # # # # # # # # # # # # # # # # # # # #
# # different from other maps because # #
# # # # # # # # # # # # # # # # # # # # #

# displays a non-ordinal categorical variable
# weights centroid calculations off of census microdata


# # # # # # # # # # # # # # # # # #
# # smallest level of geography # #
# # # # # # # # # # # # # # # # # #

# urban/rural within each state


# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # asdfree.com blog post for this survey microdata # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# http://www.asdfree.com/search/label/pesquisa%20nacional%20por%20amostra%20de%20domicilios%20%28pnad%29
# http://www.asdfree.com/search/label/censo%20demografico%20no%20brasil%20%28censo%29


# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # r code repository for setup and analysis examples # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# https://github.com/ajdamico/usgsd/tree/master/Pesquisa%20Nacional%20por%20Amostra%20de%20Domicilios
# https://github.com/ajdamico/usgsd/tree/master/Censo%20Demografico


# # # # # # # # # # # # #
# # value of interest # #
# # # # # # # # # # # # #

# most common occupational group


# # # # # # #
# # flaws # #
# # # # # # #

# 
# 


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

# download brazil's 2010 census microdata onto the local disk
options( "monetdb.sequential" = TRUE )
source_url( "https://raw.github.com/ajdamico/usgsd/master/Censo%20Demografico/download%20and%20import.R" , prompt = FALSE , echo = TRUE )
# this will be used to calculate weights used in the interpolation model

# download the 2013 pesquisa nacional por amostra de domicilios (national sample survey of households)
years.to.download <- 2013
source_url( "https://raw.github.com/ajdamico/usgsd/master/Pesquisa%20Nacional%20por%20Amostra%20de%20Domicilios/download%20all%20microdata.R" , prompt = FALSE , echo = TRUE )
# this is the actual survey to be analyzed and displayed

# # end of step 1 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 2: conduct your analysis of interest at the smallest geography allowed # #

library(downloader)
library(survey)
library(RSQLite)

# set R to produce conservative standard errors instead of crashing
# http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )
# this setting matches the MISSUNIT option in SUDAAN

# load pnad-specific functions (to remove invalid SAS input script fields and postStratify a database-backed survey object)
source_url( "https://raw.github.com/ajdamico/usgsd/master/Pesquisa Nacional por Amostra de Domicilios/pnad.survey.R" , prompt = FALSE )

# create survey design object with PNAD design information
# using existing table of PNAD data
sample.pnad <-
	svydesign(
		id = ~v4618 ,
		strata = ~v4617 ,
		data = "pnad2013" ,
		weights = ~pre_wgt ,
		nest = TRUE ,
		dbtype = "SQLite" ,
		dbname = "pnad.db"
	)
# note that the above object has been given the unwieldy name of `sample.pnad`
# so that it's not accidentally used in analysis commands.
# this object has not yet been appropriately post-stratified, as necessitated by IBGE
# in order to accurately match the brazilian 2010 census projections
	
# this block conducts a post-stratification on the un-post-stratified design
# and since the R `survey` package's ?postStratify currently does not work on database-backed survey objects,
# this uses a function custom-built for the PNAD.
y <- 
	pnad.postStratify( 
		design = sample.pnad ,
		strata.col = 'v4609' ,
		oldwgt = 'pre_wgt'
	)


# construct a four-group occupational category off of `v4809`
y <- 
	update( 

		y , 

		occcat = 

			# 01 Agrícola
			ifelse( v4809 %in% '1' , 1 ,

			# 02 Outras atividades industriais
			# 03 Indústria de transformação
			# 04 Construção
			ifelse( v4809 %in% c( '2' , '3' , '4' ) , 2 , 
			
			# 05 Comércio e reparação
			# 06 Alojamento e alimentação
			# 07 Transporte, armazenagem e comunicação
			ifelse( v4809 %in% c( '5', '6' , '7' ) , 3 ,
			
			# 08 Administração pública
			# 09 Educação, saúde e serviços sociais
			# 10 Serviços domésticos
			# 11 Outros serviços coletivos, sociais e pessoais
			ifelse( v4809 %in% c( '8' , '9' , '10' , '11' ) , 4 , 
			
				# 12 Outras atividades
				# 13 Atividades maldefinidas 
				NA ) ) ) ) 
	)
# these four categories are the main distribution to be analyzed.

# convert this new category to a factor variable
y <- update( y , occcat = factor( occcat ) )

# make the urban/rural variable easier to read.
y <- update( y , urban = as.numeric( v4105 < 4 ) )

# within each brazilian state x urban/rural category,
# calculate the four-category distribution of occupational groups
small.area.statistics <- svyby( ~ occcat , ~ uf + urban , y , svymean , na.rm = TRUE )

# these are the statistics to be mapped
print( small.area.statistics )
# the standard errors are a measure of precision,
# their inverse will serve as the mapping weights

# note that the final map will only display the most frequent
# occupational categories, in each geographic region
# therefore any category that's never a maximum will *never* be displayed
# making it sort of a pointless category, don't you think?

# it might take you more than one definition attempt before
# you figure out which categorizations you can make so that every category
# is the most-frequent/maximum in at least *one* geographic region.

# confirm that all four categories are the maximum at least somewhere
table( apply( small.area.statistics[ , paste0( "occcat" , 1:4 ) ] , 1 , which.max ) )
# yep.  all four categories are the largest category in at least one geographic area.

# we are in business.

# make this object easier to type
sas <- small.area.statistics

# clear up RAM
rm( y , sample.pnad ) ; gc()

# # end of step 2 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 3: calculate small-area geographic population-weighted centroids # #

library(MonetDB.R)
library(downloader)
library(RCurl)
library(rgdal)
library(rgeos)
library(sqldf)

# load the download.cache and related functions
# to prevent re-downloading of files once they've been downloaded.
source_url(
	"https://raw.github.com/ajdamico/usgsd/master/Download%20Cache/download%20cache.R" ,
	prompt = FALSE ,
	echo = FALSE
)


# get the directory listing of ibge's smallest area shapefile
# that matches some field on the publicly-available census microdata
u <- 'ftp://geoftp.ibge.gov.br/malhas_digitais/censo_2010/setores_censitarios/'
f <- paste0(u, strsplit(getURL(u, ftp.use.epsv = FALSE, ftplistonly = TRUE), 
                        '\\s+')[[1]])

# keep only the zipped files
f <- grep( "\\.zip$" , f , value = TRUE )

# download and extract to tempdir/shps
invisible(sapply(f, function(x) {
  path <- file.path(tempdir(), basename(x))
  download.cache(x, destfile=path)
  unzip(path, exdir=file.path(tempdir(), 'shps'))
}))

# find all `see250` shapes within the temporary directory
see250.shp <- grep( "\\SEE250GC_SIR.shp$" , list.files( file.path( tempdir() , 'shps' ) , recursive = TRUE ) , value = TRUE )

# read in all shps, and prepend shapefile name to IDs
shps <- lapply(see250.shp, function(x) {
	shp <- readOGR(file.path(tempdir(), 'shps',dirname(x)), gsub( "\\.shp$" , "" , basename(x)) )
	shp <- spChFIDs(shp, paste0(x, '_', sapply(slot(shp, "polygons"), slot, "ID")))
	shp <- spTransform( shp , CRS( "+proj=longlat" ) )
	shp
})

# rbind to a single object
shp <- do.call(rbind, as.list(shps))

# extract the unweighted centroid of each very small-area shape
centroids <- gCentroid( shp , byid = TRUE )

# merge those centroids with the urban/rural, census geocodm, mesocode, and microcode
ur_codm <- cbind( shp@data[ , c( 'TIPO' , 'CD_GEOCODM' , 'NM_MESO' , 'NM_MICRO' ) ] , centroids )

# count the number of records per urban/rural x geocodm combination
ur_codm_cts <- sqldf( "select TIPO , CD_GEOCODM , count(*) as count from ur_codm group by TIPO , CD_GEOCODM" ) 

# merge those counts back on
ur_codm_wcts <- merge( ur_codm , ur_codm_cts )

# convert all column names to lowercase
names( ur_codm_wcts ) <- tolower( names( ur_codm_wcts ) )

# coerce a few fields to character
ur_codm_wcts[ c( 'cd_geocodm' , 'nm_meso' , 'nm_micro' ) ] <- 
	sapply( 
		ur_codm_wcts[ c( 'cd_geocodm' , 'nm_meso' , 'nm_micro' ) ] , 
		as.character 
	)


##################################################################################
# lines of code to hold on to for all other `censo_demografico` monetdb analyses #

# first: specify your batfile.  again, mine looks like this:
# uncomment this line by removing the `#` at the front..
batfile <- file.path( getwd() , "MonetDB/censo_demografico.bat" )

# second: run the MonetDB server
pid <- monetdb.server.start( batfile )

# third: your five lines to make a monet database connection.
# just like above, mine look like this:
dbname <- "censo_demografico"
dbport <- 50011

monet.url <- paste0( "monetdb://localhost:" , dbport , "/" , dbname )
db <- dbConnect( MonetDB.R() , monet.url , wait = TRUE )

# extract one record per state per geocodm per urban/rural,
# with the 2010 censo demografico populations in tow
adp <- 
	dbGetQuery( 
		db , 
		'select 
			v0001 as UF , 
			SUBSTRING( v0011 , 1 , 7 ) as CD_GEOCODM , 
			v1006 as TIPO , 
			sum( pes_wgt ) as pop10_pre 
		from c10 
		group by 
			UF , 
			CD_GEOCODM , 
			TIPO' 
	)

# disconnect from the current monet database
dbDisconnect( db )

# and close it using the `pid`
monetdb.server.stop( pid )

# end of lines of code to hold on to for all other `censo_demografico` monetdb analyses #
#########################################################################################

# recode the one/two variable to urban/rural
adp$tipo <- c( 'URBANO' , 'RURAL' )[ as.numeric( adp$tipo ) ]

# merge this result with the centroids
ucca <- merge( ur_codm_wcts , adp )

# distribute the population equally across cd_geocodm regions with multiple records
# note: this isn't as perfect as it could be.  but the map doesn't zoom in that far anyway.
ucca$pop10 <- ucca$pop10_pre / ucca$count
ucca$count <- ucca$pop10_pre <- NULL

# convert `tipo` to a linear variable that matches `sas`
ucca$urban <- as.numeric( ucca$tipo == 'URBANO' )
ucca$tipo <- NULL

# aggregate this `ucca` object to both the micro- and meso-area levels
# this provides two options for kriging later, in case you have a weak computer.

# both of these commands are equivalent to tying knots (below)
# and are population-weighted, which is most excellente.
ucca.micro <- 
	sqldf( 
		"select 
			uf , urban , nm_micro ,
			sum( pop10 ) as pop10 ,
			sum( pop10 * x ) / sum( pop10 ) as x ,
			sum( pop10 * y ) / sum( pop10 ) as y
		from ucca
		group by
			uf , urban , nm_micro"
	)
	
ucca.meso <- 
	sqldf( 
		"select 
			uf , urban , nm_meso ,
			sum( pop10 ) as pop10 ,
			sum( pop10 * x ) / sum( pop10 ) as x ,
			sum( pop10 * y ) / sum( pop10 ) as y
		from ucca
		group by
			uf , urban , nm_meso"
	)


# and guess what?
# we've now got the rough censo demografico 2010 weighted populations (field pop100)
# and a very small area estimate of each area's centroid latitude & longitude (fields x + y)

# clear up RAM
rm( shps , shp , centroids , ur_codm , ur_codm_cts , ur_codm_wcts , adp , ucca ) ; gc()

# # end of step 3 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 4: merge the results of your survey analysis with the small-area geography # #

# confirm that we've created all possible geographies correctly.

# the number of records in our small area statistics..
sas.row <- nrow( sas )

# ..should equal the number of unique-match-merged records..
mrow <- nrow( merge( unique( ucca.micro[ c( "uf" , "urban" ) ] ) , sas ) )

# ..and it does/they do.
stopifnot( sas.row == mrow )

# now the geocodm x urban/rural-level censo data *could* merge if you wanted it to.


# but you don't.  yet.


# the standard error (the `se.` fields) are measures of precision.
print( sas )
# the smaller the standard error, the more confident you should be
# that the estimate at a particular geography is correct.


# so invert them.  you heard me.  invert them.
sas[ paste0( 'invse.occcat' , 1:4 ) ] <- sapply( sas[ paste0( 'se.occcat' , 1:4 ) ] , function( z ) 1 / z )
# a smaller standard error indicates more precision.

# for our purposes, precision can be considered weight! #

# now we've got the weight that we should give each of our estimates #

# distribute that weight across all census blocks #


# aggregate the 2010 census block populations to the geographies that you have.
popsum <- aggregate( ucca.micro$pop10 , by = ( ucca.micro[ c( 'uf' , 'urban' ) ] ) , sum )

# make the column name meaningful
names( popsum )[ names( popsum ) == 'x' ] <- 'popsum'

# merge the popsum onto the sasfile
sas <- merge( sas , popsum )

# now.  merge
	# the occupational category in each state x urban/rural (the variable of interest)
	# the inverted standard errors (the total weight of the broad geography)
	# the population sum (the total population of all census blocks that are part of that geography)

x <- merge( ucca.micro , sas )

# confirm no record loss
stopifnot( nrow( x ) == nrow( ucca.micro ) )

# (this is the fun part)
for ( i in 1:4 ){

	# calculate the weight at each small census area
	x[ , paste0( "weight" , i ) ] <- x[ , paste0( "invse.occcat" , i ) ] * ( x$pop10 / x$popsum )
	
	# note that weight of all census areas put together
	# sums to the `invse` on the original analysis file
	stopifnot( all.equal( sum( x[ , paste0( "weight" , i ) ] ) , sum( sas[ , paste0( "invse.occcat" , i ) ] ) ) )
}

# you're done preparing your data.
# keep only the columns you need.
x <- x[ , c( paste0( "occcat" , 1:4 ) , paste0( "weight" , 1:4 ) , "x" , "y" ) ]

# # end of step 4 # #
# # # # # # # # # # #


# # # # # # # # # # # #
# # step 5: outline # #

library(raster)
library(rgdal)
library(ggplot2)

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

# # # map of the amazon # # #

# use geofabrik's map of brazilian waterways
geofabrik.fn <- "http://download.geofabrik.de/south-america/brazil-latest.shp.zip"

# store it to the local disk
download.cache( geofabrik.fn , tf )

# unzip it
geofabrik.uz <- unzip( tf , exdir = tempdir() )

# this file contains lots of information.
geofabrik.uz

# identify the waterways shapefile
# waterways.sfn <- grep( "water(.*)shp$" , geofabrik.uz , value = TRUE )

# read it in
# waterways.shp <- readOGR( waterways.sfn  , layer = gsub( "\\.shp" , "" , basename( waterways.sfn ) ) )
# waterways.shp <- spTransform( waterways.shp , CRS( "+proj=longlat" ) )

# look at which geographic features this file has
# table( waterways.shp@data$type )

# here's the outline of brazilian rivers
# plot( subset( waterways.shp , type == 'river' ) )

# # separately # #

# identify the natural objects shapefile
natural.sfn <- grep( "natural(.*)shp$" , geofabrik.uz , value = TRUE )

# read it in
natural.shp <- readOGR( natural.sfn  , layer = gsub( "\\.shp" , "" , basename( natural.sfn ) ) )
natural.shp <- spTransform( natural.shp , CRS( "+proj=longlat" ) )


# look at which geographic features this file has
table( natural.shp@data$type )

# here's the outline of brazilian rivers
plot( subset( natural.shp , type %in% c( 'riverbank' , 'water' ) ) )

# # # map of brazilian states # # #

# use uc davis's administrative regions
admin.fn <- "http://biogeo.ucdavis.edu/data/diva/adm/BRA_adm.zip"

# store it to the local disk
download.cache( admin.fn , tf )

# unzip it
admin.uz <- unzip( tf , exdir = tempdir() )

# this file contains a few different levels
# of administrative borders.
admin.uz

# identify the national, state border, and smaller-area shapefiles
nation.sfn <- grep( "adm0(.*)shp$" , admin.uz , value = TRUE )
states.sfn <- grep( "adm1(.*)shp$" , admin.uz , value = TRUE )
small.sfn <- grep( "adm2(.*)shp$" , admin.uz , value = TRUE )

# read all three in
nation.shp <- readOGR( nation.sfn  , layer = gsub( "\\.shp" , "" , basename( nation.sfn ) ) )
states.shp <- readOGR( states.sfn  , layer = gsub( "\\.shp" , "" , basename( states.sfn ) ) )
small.shp <- readOGR( small.sfn  , layer = gsub( "\\.shp" , "" , basename( small.sfn ) ) )

nation.shp <- spTransform( nation.shp , CRS( "+proj=longlat" ) )
states.shp <- spTransform( states.shp , CRS( "+proj=longlat" ) )
small.shp <- spTransform( small.shp , CRS( "+proj=longlat" ) )

# # ready to stack all four maps?

# calculate the bounding box of brazil
bb <- bbox( states.shp )
# *but* this bounding box includes
# mostly-uninhabited islands out in the
# atlantic ocean that should probably be tossed.

# the easternmost point in continental brazil
# (and actually all of continental south america)
# is in pariaba state at ponta do seixas

# therefore, replace the xmax value with pariaba state's city where the sun rises first.
bb[ 1 , 2 ] <- bbox( subset( small.shp , NAME_2 == "JoÃ£o Pessoa" & NAME_1 == "ParaÃ­ba" ) )[ 1 , 2 ]

# initiate the lowest layer (the world)
plot( world.shp , xlim = bb[ 1 , ] , ylim = bb[ 2 , ] , col = 'gray' , fill = TRUE , border = 'white' )

# turn gray off for brazil only, then add state boundaries in gray
plot( states.shp , add = TRUE , col = 'white' , fill = TRUE , border = 'gray'  )

# add the amazon
plot( subset( natural.shp , type %in% c( 'riverbank' , 'water' ) ) , add = TRUE , col = 'lightblue' , fill = TRUE , border = 'lightblue' )

# you can add the national border in black if you want
# plot( nation.shp , add = TRUE , border = 'black' )
# but i don't think it's necessary

# # not bad for a start, huh?

# draw a rectangle 30% bigger than the original state
br.shp.blank <- as( 1.15 * extent( bb ) , "SpatialPolygons" )

# calculate the difference between the rectangle and the actual shape
br.shp.diff <- gDifference( br.shp.blank , nation.shp )
# this will be used to cover up points outside of the national borders

# this box will later blank out the surrounding area
plot( br.shp.diff )
# starting to make sense?


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

# limit the brazilian coast shapefile
# to only points within the larger bounding box
br.coast <- gIntersection( br.shp.blank , coast.shp )
# so you're not carrying around the whole world's ocean

# these are huge objects, so in order to conserve ram
# fortify what you need for ggplot2 and toss all other objects.
fcoast <- fortify( br.coast ) ; rm( br.coast )
wshape <- fortify( subset( world.shp , CNTR_ID != 'BR' ) ) ; rm( world.shp )
fstate <- fortify( states.shp ) ; rm( states.shp )
fwater <- fortify( subset( natural.shp , type %in% c( 'riverbank' , 'water' ) )  ) ; rm( natural.shp )
outside <- fortify( br.shp.diff ) ; rm( br.shp.diff )
rm( small.shp )
# got 'em all, i think.  clear up RAM
gc()

# # end of step 5 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # #
# # step 6: tie knots and krige # #

library(fields)

for ( i in 1:4 ){

	this.krig.fit <-
		Krig(
			cbind( x$x , x$y ) ,
			x[ , paste0( 'occcat' , i ) ] ,
			weights = x[ , paste0( 'weight' , i ) ] 
		)
		
	assign( paste0( 'krig.fit' , i ) , this.krig.fit )
	
	rm( this.krig.fit ) ; gc()
	
}
# what is the (weighted) relationship between
# your variable of interest (occupational category)
# and the x/y points on a grid?

# check this out!
surface( krig.fit1 )	# agricultural occupations
surface( krig.fit2 )	# industrial occupations
surface( krig.fit3 )	# commercial occupations
surface( krig.fit4 )	# government and service occupations
# you're almost there!

# # end of step 6 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # #
# # step 7: make a grid and predict # #

# use as fine of a grid as your computer can handle
grid.length <- 750
# # note: smaller grids will render faster
# # (so they're better if you're just playing around)
# # but larger grids will prevent your final plot from
# # being too pixelated, even when zooming in

# create a bounding box ten percent bigger than the nation
bb10 <- bbox( as( 1.05 * extent( bb ) , "SpatialPolygons" ) )

krig.grd <- 
	expand.grid(
		intptlon = seq( bb10[ 1 , 1 ] , bb10[ 1 , 2 ] , length = grid.length ) , 
		intptlat = seq( bb10[ 2 , 1 ] , bb10[ 2 , 2 ] , length = grid.length )
	)

	
# along your rectangular grid, what are
# the predicted values of each category?
for ( i in 1:4 ){
	for ( j in split( seq( nrow( krig.grd ) ) , ceiling( seq( nrow( krig.grd ) ) / 5000 ) ) ){
		krig.grd[ j , paste0( 'occcat' , i ) ] <- as.numeric( predict( get( paste0( 'krig.fit' , i ) ) , krig.grd[ j  , 1:2 ] ) )
	}
	gc()
}

# clear up RAM
rm( list = paste0( 'krig.fit' , 1:4 ) ) ; gc()

# remember that these values have been re-scaled
# as how disproportionate they are from the state-wide averages.
# therefore, negative values are possible.
sapply( krig.grd , summary )

# in general, these predictions at each point should approximately sum to one
summary( rowSums( krig.grd[ , 3:6 ] ) )
# the fact that they don't is an indication that we're using too many categories.

# we will re-scale these values, but the fewer categories, the better.
# linear variables are always superior to categories,
# they throw out less informatin

# alright, here's the major test.  we have four categories.
# the final map will only display the most frequent
# occupational category in each geographic region
# any category that's never a maximum will *never* be displayed

# confirm that all four of our categories are the maximum
# at least somewhere on the grid.  are they?
table( apply( krig.grd[ , 3:6 ] , 1 , which.max ) )
# they are!  we're in business.

# alright, let's re-scale every row so it sums to one.
krig.grd[ , 3:6 ] <- krig.grd[ , 3:6 ] * ( 1 / rowSums( krig.grd[ , 3:6 ] ) )
# double-check we did that right?
summary( rowSums( krig.grd[ , 3:6 ] ) )
# that was easy.

# # end of step 7 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # #
# # step 8: limit information and color # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # warning # # # warning # # # # # # warning # # # # # # warning # # # # # # warning # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# if your data is are not binomial, then by mapping with a single image, you lose clarity #
# if you have three levels of information and you generate two maps, you can get an idea  #
# about the entire distribution of the variable.  if you attempt encoding three levels or #
# more into a single map, you will explode. just kidding rofl lmao but you will lose info #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # warning # # # warning # # # # # # warning # # # # # # warning # # # # # # warning # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


library(scales)

# from among the four categories, find the maximum occupation category
krig.grd$occcat <- c( 'agriculture' , 'industry' , 'commercial' , 'service' )[ apply( krig.grd[ , 3:6 ] , 1 , which.max ) ]

# save only that max
krig.grd$statistic <- apply( krig.grd[ , 3:6 ] , 1 , max )

# it's important to note that i've thrown out a lot of information here
krig.grd <- krig.grd[ , c( 'intptlon' , 'intptlat' , 'statistic' , 'occcat' ) ]

# do any points not make sense?
summary( krig.grd$statistic )

# you have to simplify this.
# simplifying it means throwing out information.

library(RColorBrewer)

# draw five gradients
tg <-
	lapply( 
		brewer.pal( 5 , 'Set1' ) , 
		function( z ) colorRampPalette( c( 'white' , z ) )( 101 )
		# start at ~25% ( that is: 25 / 125 ) with this function instead
		# function( z ) colorRampPalette( c( 'white' , z ) )( 125 )[ 25:125 ]
	)

# check out each of these five colors, mapped from opaque to intense.
plot( rep( 0:100 , 5 ) , rep( 1:5 , each = 101 ) , col = unlist( tg ) , pch = 16 , cex = 3 )

# if you use this one, i'd recommend tossing the blue
# so that your audience doesn't confuse it with river


# draw an alternate four gradients
# and also use a different palette from colorbrewer2.org
tag <-
	lapply( 
		# flip the colors,
		# because the lowest is harder to see
		brewer.pal( 4 , 'Set3' ) , 
		# the lines demarcate very strongly in the krig.grd below
		# skipping the first values of this gradient
		# lowers the demarcation but might have sharper borders
		function( z ) colorRampPalette( c( 'white' , z ) )( 130 )[ 31:130 ]
	)

# check out each of these four colors, mapped from opaque to intense.
plot( rep( 0:99 , 4 ) , rep( 1:4 , each = 100 ) , col = unlist( tag ) , pch = 16 , cex = 3 )


# # rescale the interpolated grids
krig.grd$statistic <- round( rescale( krig.grd$statistic , c( 0.501 , 100.499 ) ) )
# note that the re-scaling gets done across all categories,
# and not individually within each category.

# add the alternate hex color identifier
krig.grd$alt.color <- 
		ifelse( krig.grd$occcat == 'agriculture' , tag[[1]][ krig.grd$statistic ] ,
		ifelse( krig.grd$occcat == 'industry' , tag[[4]][ krig.grd$statistic ] ,
		ifelse( krig.grd$occcat == 'commercial' , tag[[3]][ krig.grd$statistic ] , 
		ifelse( krig.grd$occcat == 'service' , tag[[2]][ krig.grd$statistic ] , 
			NA ) ) ) )

# that looks a bit better to me
plot( krig.grd$intptlon , krig.grd$intptlat , col = krig.grd$alt.color , pch = 16 , cex = 3 )

# clear up RAM
krig.grd$occcat <- NULL ; gc()

# # end of step 8 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # #
# # step 9: ggplot and choose options # #

library(ggplot2)
library(mapproj)
library(scales)

# initiate the krige-based plot

# krig.grd$color.column <- as.factor( krig.grd$color.value )
krig.grd$color.column <- as.factor( krig.grd$alt.color )

krg.plot <- 
	ggplot( data = krig.grd , aes( x = intptlon , y = intptlat ) ) +
	geom_point( shape = 15 , color = krig.grd$color.column ) +
	scale_fill_manual( values = unique( krig.grd$alt.color ) )

krg.plot

# initiate the entire plot
the.plot <-

	krg.plot +
	
	# blank out the legend and axis labels
	theme(
		legend.position = "none" ,
		axis.title.x = element_blank() ,
		axis.title.y = element_blank()		
	) + 
	
	xlab( "" ) + ylab( "" ) +

	# force the x and y axis limits at the shape of the city and don't do anything special for off-map values
	scale_x_continuous( limits = bb10[ 1 , ] , breaks = NULL , oob = squish ) +
	# since we're going to add lots of surrounding-area detail!
    scale_y_continuous( limits = bb10[ 2 , ] , breaks = NULL , oob = squish ) +

	theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		panel.background = element_blank(),
		panel.border = element_blank(),
		axis.ticks = element_blank()
	)

# print the plot to the screen
the.plot
# this is the bottom layer.

# # state borders # #

# store this information in a layer
state.border.layer <- geom_path( data = fstate , aes( x = long , y = lat , group = group ) , colour = 'lightgray' )

# plot the result
# the.plot + state.border.layer

# # international borders # #

# store this information in a layer
international.border.layer <- geom_polygon( data = wshape , aes( x = long , y = lat , group = group ) , fill = 'lightgray' , color = 'white' )

# plot the result
# the.plot + international.border.layer + state.border.layer




library(plyr)

orect <- geom_rect( xmin = bb10[ 1 , 1 ] , xmax = bb10[ 1 , 2 ] , ymin = bb10[ 2 , 1 ] , ymax = bb10[ 2 , 2 ] , color = 'white' , fill = NA , size = 4 )


# fix islands piecing together
fcoast2 <- ddply( fcoast , .( piece ) , function( x ) rbind( x , fcoast[ 1 , ] ) )

# convert this fortified object to a ggplot layer
ocean.layer <- geom_polygon( data = fcoast2 , aes( x = long , y = lat , group = group ) , fill = 'white' )


# fix islands piecing together
outside2 <- ddply( outside , .( piece ) , function( x ) rbind( x , outside[ 1 , ] ) )

# convert this fortified object to a ggplot layer
outside.layer <- geom_polygon( data = outside2 , aes( x = long , y = lat , group = id ) , fill = 'white' )

# plot this. 
# the.plot + outside.layer
# that's not so bad, i guess.

the.plot + outside.layer + international.border.layer + state.border.layer + orect + ocean.layer


the.plot + outside.layer + international.border.layer + state.border.layer + orect + ocean.layer + coord_map( "albers" , bb10[ 2 , 1 ] , bb10[ 2 , 2 ] )



# # end of step 9 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # #
# # step 10: project, blank, and save # #

library(ggplot2)
library(scales)
library(raster)
library(plyr)
library(rgeos)


# distort the map with simple latitude/longitude scaling
the.plot + state.border.layer + coord_fixed( 2.5 )

# this looks crappy, who knows what it is
the.plot + state.border.layer + coord_equal()

# check out a bunch of other options #
the.plot + state.border.layer + coord_map( project = "cylequalarea" , mean( x$intptlat ) )

# here's the one that makes the most sense for alaska
the.plot + state.border.layer + coord_map( project = "conic" , mean( x$intptlat ) , orientation = c( 90 , 0 , -141 ) )

# see ?mapproject and the ?coord_* functions for a zillion alternatives

# store this projection, but not the state border
the.plot <- the.plot + coord_map( project = "conic" , mean( x$intptlat ) , orientation = c( 90 , 0 , -141 ) )
# into `the.plot`

# initiate the outside blanking layer
# outside <- fortify( spTransform( br.shp.diff , CRS( "+proj=longlat" ) ) )

# i don't care for the state border layer,
# but if you want the state border layer,
# use this save line:
final.plot <- the.plot + outside.layer + state.border.layer
# otherwise use this save line:
# final.plot <- the.plot + outside.layer
# you can airbrush the outside blue border
# in microsoft paint or something
# if you want, right? like a boss.


# save the file to your current working directory
ggsave( 
	"2013 alaskan veteran service eras.png" ,
	plot = final.plot ,
	scale = 3
)
# happy?

# # end of step ten # #
# # # # # # # # # # # #
















guides(
	colour = 
		guide_legend(
			override.aes = 
				list( 
					linetype = c( 1 , 0 ) , 
					shape = c( NA , 16 ) 
				)
		)
)





# # ultra-high resolution??


# # # include surrounding countries so it doesn't appear alone on the continent!


# you can also use the world/europe shapefile to draw surrounding countries.

# from geofabrik.. natural shapefile..
plot(subset(a,type=='water')) # # gets only the water


# for knots?  you might not need knots.
> nrow(unique(shp@data[,c('TIPO','NM_MESO')]))
[1] 274
> nrow(unique(shp@data[,c('NM_MESO')]))
NULL
> nrow(unique(shp@data[c('NM_MESO')]))
[1] 137
