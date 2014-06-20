# pretty cool, huh?

# load a few mapping libraries
library(rgdal)
library(maptools)
library(ggplot2)
library(plyr)
library(mapproj)
library(Imap)

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


# # # shapefile read-in? # # #

# specify the tiger file to download
tiger <- "ftp://ftp2.census.gov/geo/tiger/TIGER2010/CBSA/2010/tl_2010_us_cbsa10.zip"
filedownload<-"/tl_2010_us_cbsa10"

# create a temporary file and a temporary directory
tf <- tempfile() ; td <- tempdir()

# download the tiger file to the local disk
download.file( tiger , tf , mode = 'wb' )

# unzip the tiger file into the temporary directory
z <- unzip( tf , exdir = td )

# isolate the file that ends with ".shp"
shapefile <- z[ grep( 'shp$' , z ) ]

# read the shapefile into working memory

shapefile<-strsplit(shapefile[1],".shp")
filelocation<-paste(strsplit(as.character(shapefile),filedownload))

cbsa.map<-readOGR(filelocation,layer=gsub("/","",filedownload))


# remove CBSAs ending with alaska, hawaii, and puerto rico
cbsa.map <- cbsa.map[ !grepl( "AK$|HI$|PR$" , cbsa.map$NAME10 ) , ]

# convert the shapefile to a dataframe
cbsa.map.points <- fortify( cbsa.map , region = "GEOID10" )

names(x)[1]<-"id"
plotcbsa<-merge(cbsa.map.points,x,by="id")


# calculate the centroids of each CBSA
mid_range <- function( x ) mean( range( x , na.rm = TRUE ) )
centres <- ddply( plotcbsa, . ( id ) , colwise( mid_range, . ( lat , long ) ) )


centrevalues<-lapply(centres[,1],FUN=function(x) plotcbsa$trinary[x==plotcbsa$id][1])
centres<-data.frame(centres,unlist(centrevalues))

centreweights<-apply(as.array(centres[,1]),1,FUN=function(x) plotcbsa$weight[x==plotcbsa$id][1]) 
centres<-data.frame(centres,centreweights)

u_v_0<-centres[,3]
u_v_i<-centres[,2]
Yi<-centres[,4]
nj<-centres[,5]
bandwidth<-500

#sum( 
# apply( 
#	as.array(
#		( 1:length( u_v_0 ) ) [ -i ] ) ,
#			1, FUN = function( x ) 
#				1 / sqrt( 2 * pi * bandwidth ) * exp( -.5 * ( ( u_v_0[1] - u_v_0[x] ) / bandwidth ) ^2 ) 
#	)*
# apply( 
#	as.array(
#		( 1:length( u_v_0 ) ) [ -i ] ) ,
#			1, FUN = function( x ) 
#				1 / sqrt( 2 * pi * bandwidth ) * exp( -.5 * ( ( u_v_i[1] - u_v_i[x] ) / bandwidth ) ^2 ) 
#	)
#)*
#(1 / nrow( centres ) * 20 * 20)



#locally smoothed rate r_tilda i
i<-1
dd<-NULL
for(i in 1:nrow(centres)){

	dd<-c(dd,
	sum(
		apply(
			as.array(
				( 1:length( u_v_0 ) ) [ -i ] ) , 1 ,
					FUN = function (x) 1 / sqrt( 2 * pi * bandwidth ) * exp( -.5 * ( ( gdist(u_v_0[ i ] , u_v_i[ i ] , u_v_0[ x ] , u_v_i[ x  ] ) ) / bandwidth ) ^2 )  *
						Yi[x] ) ) / 
							sum ( apply ( 
								as.array(
									( 1:length( u_v_0 ) ) [ -i ] ) , 1 , 
										FUN = function (x) 1 / sqrt( 2 * pi * bandwidth ) * exp( -.5 * ( ( gdist(u_v_0[ i ] , u_v_i[ i ] , u_v_0[ x  ] , u_v_i[ x ] ) ) / bandwidth ) ^2 )  *
											nj[x] ) )  
	)									
}

centres<-data.frame(centres,dd)
names(centres)[6]<-"smoothstat"
smoothplotcbsa<-merge(plotcbsa,centres,by="id")	

		
	smoothplotcbsa$mapme_bin <- cut(as.numeric(smoothplotcbsa$smoothstat), breaks = c(seq(2.39e-06, 4.94e-06, by = 5.087133e-08)))
	smoothplotcbsa <- smoothplotcbsa[ order ( smoothplotcbsa $ order ) , ]

	cbbPalette <- gray.colors(50)


 p <- ggplot( smoothplotcbsa )   + 
		aes( long.x , lat.x ) + 
		#geom_polygon(aes( group = group , colour = smoothstat) , fill = "grey" ) +
		geom_polygon(aes(fill = mapme_bin,group=group))+
		#scale_fill_manual(values=cbbPalette, name = paste("% selecting ",colnames(tomap)[1],"",sep="'"),limits=levels(choro$mapme_bin)) +  
		scale_fill_manual(values=cbbPalette,limits=levels(smoothplotcbsa$mapme_bin)) +  
		  #creates the outlines of the provinces
	#	 geom_path( aes( group = group ) , size = .5 , color = "black" , lty="dashed" ) +
		 geom_point( data = centres , aes( x = long , y = lat ) ) +

		 #theme with white background
		 theme_bw() +
		 #eliminates background, gridlines, and chart border
		 theme( plot.background = element_blank( )
			, panel.grid.major = element_blank( )
			, panel.grid.minor = element_blank( )
			, panel.border = element_blank( )
			, panel.background = element_blank( )
			, axis.title.y = element_blank( )
			, axis.title.x = element_blank( )
			, axis.ticks = element_blank( )
			, axis.text = element_blank( )
			, legend.position = ( " top"  )
			, legend.text = element_text( size = 15 )
			, legend.title = element_text( size = 15 )
		)

		 p <- p + coord_map( )

p


