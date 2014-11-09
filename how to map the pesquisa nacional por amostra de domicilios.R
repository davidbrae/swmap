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
			# 07 Transporte, armazenagem e comunicação
			ifelse( v4809 %in% c( '2' , '3' , '4' , '7') , 2 , 
			
			# 05 Comércio e reparação
			# 06 Alojamento e alimentação
			# 10 Serviços domésticos
			ifelse( v4809 %in% c( '5', '6' , '10' ) , 3 ,
			
			# 08 Administração pública
			# 09 Educação, saúde e serviços sociais
			# 11 Outros serviços coletivos, sociais e pessoais
			ifelse( v4809 %in% c( '8' , '9' , '11' ) , 4 , 
			
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

# it might take you a more than one definition attempt before
# you figure out which categories you can make so that every category
# is the most-frequent/maximum in at least *one* geographic region.

# confirm that all four categories are the maximum at least somewhere
table( apply( small.area.statistics[ , paste0( "occcat" , 1:4 ) ] , 1 , which.max ) )
# yep.  all four categories are the largest category in at least one geographic area.

# we are in business.

# make this object easier to type
sas <- small.area.statistics

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


# get the directory listing
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

see250.shp <- grep( "\\SEE250GC_SIR.shp$" , list.files( file.path( tempdir() , 'shps' ) , recursive = TRUE ) , value = TRUE )

# read in all shps, and prepend shapefile name to IDs
shps <- lapply(see250.shp, function(x) {
	shp <- readOGR(file.path(tempdir(), 'shps',dirname(x)), gsub( "\\.shp$" , "" , basename(x)) )
	# shp <- gBuffer( shp , width = 0 , byid = TRUE )
	# shp <- gUnaryUnion( shp , as.character( shp$CD_GEOCODM ) )
	shp <- spChFIDs(shp, paste0(x, '_', sapply(slot(shp, "polygons"), slot, "ID")))
	# if( is.na( proj4string( shp ) ) ) proj4string( shp ) <- "+proj=longlat"
	# shp <- spTransform( shp , CRS( "+proj=longlat" ) )
	# names( shp@data ) <- tolower( names( shp@data ) )
	# if( !( "cd_aponde" %in% names( shp@data ) ) ) names( shp@data )[ grepl( "pond" , names( shp@data ) ) ] <- "cd_aponde"
	# shp@data <- shp@data[ "cd_aponde" ]
	shp
})

# rbind to a single object
shp <- do.call(rbind, as.list(shps))

centroids <- gCentroid( shp , byid = TRUE )
ur_codm <- cbind( shp@data[ , c( 'TIPO' , 'CD_GEOCODM' , 'NM_MESO' ) ] , centroids )
ur_codm_cts <- sqldf( "select TIPO , CD_GEOCODM , count(*) as count from ur_codm group by TIPO , CD_GEOCODM" ) 
ur_codm_wcts <- merge( ur_codm , ur_codm_cts )
names( ur_codm_wcts ) <- tolower( names( ur_codm_wcts ) )
ur_codm_wcts$cd_geocodm <- as.character( ur_codm_wcts$cd_geocodm )
ur_codm_wcts$nm_meso <- as.character( ur_codm_wcts$nm_meso )

# plot (note: clipping to contiguous states for display purposes)
# plot(shp, axes=T, las=1)






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

# one record per state per geocodm per urban/rural, with the 2010 censo demografico populations in tow
adp <- dbGetQuery( db , 'select v0001 as UF , SUBSTRING( v0011 , 1 , 7 ) as CD_GEOCODM , v1006 as TIPO , sum( pes_wgt ) as pop10_pre from c10 group by UF , CD_GEOCODM , TIPO' )

# recode the one/two variable to urban/rural
adp$tipo <- c( 'URBANO' , 'RURAL' )[ as.numeric( adp$tipo ) ]

# merge this result with the centroids
ucca <- merge( ur_codm_wcts , adp )

# distribute the population equally across cd_geocodm regions with multiple records
# note: this isn't as perfect as it could be.  but the map doesn't zoom in that far anyway.
ucca$pop10 <- ucca$pop10_pre / ucca$count


# and now, you have to do the unfortunate task of aggregating the population-weighted centroids
# up to the state x urban/rural-level.  all of the censo demografico 2010 work was just to find
# the correct latitude & longitude so your state x urban/rural points are positioned well.
x <- 
	sqldf( 
		"select 
			uf , ( tipo='URBANO' )*1 AS urban,
			sum( pop10 * x ) / sum( pop10 ) as x ,
			sum( pop10 * y ) / sum( pop10 ) as y
		from ucca
		group by
			uf , tipo"
	)

# these had better be the same.
stopifnot( nrow( x ) == nrow( sas ) )

x <- merge( x , sas )

# these had better still be the same.
stopifnot( nrow( x ) == nrow( sas ) )









# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 4: merge the results of your survey analysis with the small-area geography # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # step 4: merge the results of your survey analysis with the small-area geography # #

# # # # # # # # # # # #
# # step 5: outline # #

# # # # # # # # # # # # # # # # # #
# # step 6: tie knots and krige # #

# # # # # # # # # # # # # # # # # # # #
# # step 7: make a grid and predict # #

# # # # # # # # # # # # # # # # # # # # # #
# # step 8: limit information and color # #

# # # # # # # # # # # # # # # # # # # # #
# # step 9: ggplot and choose options # #

# # # # # # # # # # # # # # # # # # # # #
# # step 10: project, blank, and save # #



# # ultra-high resolution??


# # # include surrounding countries so it doesn't appear alone on the continent!




plot(subset(a,type=='water')) # # gets only the water


# for knots?  you might not need knots.
> nrow(unique(shp@data[,c('TIPO','NM_MESO')]))
[1] 274
> nrow(unique(shp@data[,c('NM_MESO')]))
NULL
> nrow(unique(shp@data[c('NM_MESO')]))
[1] 137
