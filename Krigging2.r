library(maptools)
library(foreign)
library(ggplot2)
library(sp)
library(gstat)
library(rgdal)

tf <- tempfile() ; td <- tempdir()

download.file( "ftp://ftp2.census.gov/geo/tiger/TIGER2011/COUNTY/tl_2011_us_county.zip" , tf , mode = 'wb' )

filedownload<-"tl_2010_us_cbsa11"

z <- unzip( tf , exdir = td )

# read in the entire county map of the united states

y <-  z[grep('shp$',z)] 

gsub(y,filedownload,"")


y<-strsplit(z[1],"/")





filelocation<-"C:/Users/drae/AppData/Local/Temp/Rtmps5x6LT"

y<-readOGR(filelocation,layer="tl_2011_us_county")

y$STATEFP<-as.numeric(as.character(y$STATEFP))

y<-subset(y,y$STATEFP<57)
y<-subset(y,y$STATEFP!=2&y$STATEFP!=15)

map_points <- fortify( y , region = "GEOID" )

# read in the dbf of the united states by count
x <- read.dbf( z[ grep('dbf$',z)])


# make some fake data
a <- x[ , c( 'GEOID' , 'INTPTLAT' , 'INTPTLON' ) ]

a$VALUE <- c(sample(1:10,nrow( a )/2,replace=TRUE),sample(11:20,nrow( a )/2,replace=TRUE))
a$SE <- runif(nrow( a ))*2

names(a)[1]<-"id"
map_points$id<-as.numeric(as.character(map_points$id))
a$id<-as.numeric(as.character(a$id))

plotme<-merge(map_points,a,by="id")
plotme<-plotme[order(plotme$order),]

plotmeUnique<-plotme[!duplicated(plotme[,1]),]

#duplicates????
zerodist(plotmeUnique.grid)

plotmeUnique<-plotmeUnique[!duplicated(plotmeUnique[,3]),]

ggplot(plotme,aes(x=long,y=lat))+
geom_polygon(aes(fill = VALUE,group=group))


plotmeUnique.grid<-SpatialPixelsDataFrame(plotmeUnique[c("long","lat")],data=plotmeUnique,tolerance=.5)
coordinates(plotme) = ~long+lat
coordinates(plotmeUnique) = ~long+lat


v_RI<-variogram(VALUE~1, data=plotmeUnique)

plot(v_RI)


v.fit = fit.variogram(v_RI, vgm(4,"Nug",33))
v.fit = fit.variogram(v_RI, vgm(4,"Sph",33))

plot(v_RI,v.fit)


gridded(plotmeUnique.grid)


plotme_mofo<-krige(VALUE~1,plotmeUnique.grid,plotme,model=vgm(4,"Sph",33))

save(Rtmps5x6LTplotme_mofo, file = "krige.RData")


################################################
setwd("C:/Users/drae/Desktop")

load(file = "krige.RData")

xx_spatial<-Rtmps5x6LTplotme_mofo

spplot(xx_spatial["var1.pred"], main = "ordinary kriging predictions")




xx<-data.frame(attributes(plotme_mofo["var1.pred"])$coords,attributes(plotme_mofo["var1.pred"])$data)

write.csv(xx,"test.csv")

######convert to an evenly spaced grid
#http://stackoverflow.com/questions/14512381/why-will-geom-tile-plot-a-subset-of-my-data-but-not-more
e <- extent( xx_spatial )

ratio <- ( e@xmax - e@xmin ) / ( e@ymax - e@ymin )
r <- raster( nrows = 56 , ncols = floor( 56 * ratio ) , ext = extent(xx_spatial) )
rf <- rasterize( xx_spatial , r , field = "var1.pred" , fun = mean )
rdf <- data.frame( rasterToPoints( rf ) )  
ggplot( NULL ) + geom_raster( data = rdf , aes( x , y , fill = layer ) )
ggplot( NULL )+geom_tile( data = rdf , aes( x , y , fill = layer ) )





hist(xx$var1.pred)
dev.off()

class(xx$var1.pred)

xx<-xx[order(xx$long),]

p<-ggplot(aes(x = long, y = lat), data = xx) 
p+geom_tile(aes(fill = var1.pred))

head(xx)

p+geom_point(aes(fill=var1.pred))


#why won't this work?
plot(xx$long,xx$lat)

require( RCurl )
require( raster )
require( sp )
require( ggplot2 )


spdf <- SpatialPointsDataFrame( data.frame( x = xx$long , y = xx$lat ) , data = data.frame( z = xx$z ) )
e<-extent(spdf)

?extent



####################################################################examples

library(sp)
data(meuse)
# no trend:
coordinates(meuse) = ~x+y
variogram(log(zinc)~1, meuse)


plot_rhodeisland_dataframe.grid<-SpatialPixelsDataFrame(plot_rhodeisland_dataframe[c("long","lat")],data=plot_rhodeisland_dataframe,tolerance=.5)



plotme_mofo<-krige(VALUE~1,locations=plot_rhodeisland_dataframe.grid,plot_rhodeisland,model=vgm(4,"Sph",.6))

spplot(plotme_mofo["var1.pred"], main = "ordinary kriging predictions")



plot_rhodeisland_dataframe
gridded(plot_rhodeisland_dataframe) = ~long+lat


#extract the data and the coordiantes from the spatial pixel frame
xx<-data.frame(attributes(plotme_mofo["var1.pred"])$coords,attributes(plotme_mofo["var1.pred"])$data)

#map it with ggplot
ggplot(aes(x = x, y = y), data = xx) + geom_tile(aes(fill = var1.pred))






####################################################################################

library(sp)
data(meuse)

coordinates(meuse) = ~x+y
data(meuse.grid)
gridded(meuse.grid) = ~x+y
m <- vgm(.59, "Sph", 874, .04)
# ordinary kriging:
x <- krige(log(zinc)~1, meuse, meuse.grid, model = m)
plot(x)

spplot(x["var1.pred"], main = "ordinary kriging predictions")

xx<-data.frame(attributes(x["var1.pred"])$coords,attributes(x["var1.pred"])$data)

p<-ggplot(aes(x=x,y=y), data = xx) 
p+geom_tile(aes(fill = var1.pred))

class(xx$var1.pred)




nrow(meuse.grid)

nrow(meuse)















