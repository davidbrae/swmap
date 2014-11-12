# prevR EXAMPLE: map of infant mortality
# Burkina Faso DHS 2010 - Children born between 2006 and 2008
# Joseph Larmarange

require(prevR)

# DATA IMPORT -------------------
# NB: to adapt according to the script you are using
# For this example, we need children's recode and geographic datasets

require(foreign)
cl <- read.dbf("data/bf2010/BFGE61FL.dbf") # cl for clusters
ch <- read.spss("data/bf2010/BFKR62FL.SAV", to.data.frame = TRUE) # ch for children

# Optional, import boundaries from http://spatialdata.dhsprogram.com/
require(maptools)
bounds <- readShapePoly("data/bf2010/shps/sdr_subnational_boundaries.shp")
class(bounds) <- "SpatialPolygons" # readShapePoly create a SpatialPolygonsDataFrame and not a SpatialPolygons object. This trick is required because as.prevR requires a SpatialPolygons object.

# If you didn't download boundaries from http://spatialdata.dhsprogram.com/, you can use standard boundaries directly available through prevR:
# bounds <- create.boundary("Burkina Faso")

# PREPARE INDIVIDUALS DATA ----------------
# Variables of interest
# V001 cluster number
# V005 Sample weight
# B2 Year of birth
# B5 Child is alive
# B6 Age at Death (1xx in Days, 2xx in months, 3xx in years)

ch$cluster <- ch$V001

# Sampling weight (V005/10000, cf. DHS documentation)
ch$weight <- ch$V005 / 1000000

# We want only children born betwwen 2006 and 2008
ch <- ch[ch$B2>=2006&ch$B2<=2008,]

# Variable im (infant mortality), 1 if death before age of 1, 0 else
ch$im <- 0
ch[ch$B5=="No"&!is.na(ch$B6)&ch$B6<=211,"im"] <- 1

# PREPARE CLUSTERS DATA --------------------
# Variable of interest
# DHSCLUST Cluster number
# LATNUM Latitude
# LONGNUM Longitude
# SOURCE Source of coordinates (MIS indicates that coordinates are missing)

# See ?as.prevR for the list of variables required to create a prevR object

cl$id <- cl$DHSCLUST
cl$x <- cl$LONGNUM
cl$y <- cl$LATNUM

# We will keep only clusters for whose coordinates have been collected
cl <- cl[cl$SOURCE!="MIS",]

# For each cluster, we need the number of observations (n), the number of positive cases (pos)
# and the sum of observations and positive cases weights (wn, wpos)

for (i in cl$id) {
  temp <- ch[ch$cluster==i,c("im","weight")]
  cl[cl$id==i,"n"] <- nrow(temp)
  cl[cl$id==i,"wn"] <- sum(temp$weight)
  temp <- temp[temp$im==1,]
  cl[cl$id==i,"pos"] <- nrow(temp)
  cl[cl$id==i,"wpos"] <- sum(temp$weight)
}


# CREATE prevR OBJECT ------

col <- c(id="id", x="x", y="y", n="n", pos="pos", wn="wn", wpos="wpos")

bf <- as.prevR(cl, col, bounds)

# plot location of clusters
plot(bf)


# EASY ANALYSIS USING prevR-extra -----------------

source("https://raw.githubusercontent.com/larmarange/prevR-extra/master/prevR-extra.R")

prevR(bf, N=c(100,200,300,400))

result <- prevR(bf, return.results = TRUE, legend.title = "infant mortaly\nin %")

writeAsciiGrid(result$prev,"infant_mortality.asc") # Export

# DETAILED ANALYSIS --------------
# KDE ESTIMATION WITH RINGS OF SAME NUMBER OF OBSERVATIONS --------

# We used a minimum number of 300 observations (i.e. N=300)

# Computing bandwidths
bf <- rings(bf, N=300)

# Computing surfaces
bf.map <- kde(bf, N=300, nb.cells=250)

# SIMPLE MAPS -----------

# Simple map, with both weighted and unweighted surfaces
spplot(bf.map)

# Simple map, weighted surface, with a prevR palette
spplot(bf.map, 'k.wprev.N300.RInf', cuts=100, col.regions=prevR.colors.red(101), main="Regional trends of prevalence (N=300)")

# ADVANCE MAPS WITH GGPLOT2 ---------------

require(ggplot2)

# Source: R Graphics Cookbook pp. 317-18
theme_prevR <- function (base_size = 12) {
  require(grid)
  theme_grey(base_size) %+replace%
    theme(
      axis.title        = element_blank(),
      axis.text         = element_blank(),
      panel.background  = element_blank(),
      panel.grid        = element_blank(),
      axis.ticks.length = unit(0, "cm"),
      axis.ticks.margin = unit(0, "lines"),
      plot.margin       = unit(c(0, 0, 0, 0), "lines"),
      complete          = TRUE
    )
}

map.df <- na.omit(as.data.frame(bf.map))
map.df <- map.df[order(map.df$x,map.df$y),]

ggplot(map.df, aes(x=x, y=y, fill=k.wprev.N300.RInf)) +
  geom_raster() +
  scale_fill_gradientn(colours=prevR.colors.red(20)) +
  coord_fixed() + xlab("") + ylab("") +
  labs(fill="Infant mortality\nin %") +
  theme_prevR()

# Adding boundaries to the map

bounds.df <- fortify(bounds)

ggplot(map.df, aes(x=x, y=y, fill=k.wprev.N300.RInf)) +
  geom_raster() +
  scale_fill_gradientn(colours=prevR.colors.red(20)) +
  coord_fixed() + xlab("") + ylab("") +
  labs(fill="Infant mortality\nin %") +
  theme_prevR() +
  geom_path(aes(x=long, y=lat, fill=NULL, group=group), data=bounds.df)

# Adding coutour lines of the bandwiths (radius of the rings)

radius <- krige(r.radius~1, bf, N=300, R=Inf, nb.cells=250) # NB: nb.cells should be the same

dimnames(radius@coords)[[2]] <- c("x","y") # Fixing name of coordinates
radius.df <- na.omit(as.data.frame(radius))
radius.df <- radius.df[order(radius.df$x,radius.df$y),]

map.df$radius <- radius.df$r.radius.N300.RInf

require(directlabels)

p <- ggplot(map.df, aes(x=x, y=y, fill=k.wprev.N300.RInf, z=radius)) +
  geom_raster() +
  scale_fill_gradientn(colours=prevR.colors.red(20)) +
  stat_contour(aes(colour = ..level..)) +
  scale_colour_continuous(low = "grey50", high = "grey50") +
  coord_fixed() + xlab("") + ylab("") +
  labs(fill="Infant mortality\nin %") +
  theme_prevR()
print(direct.label(p,list("top.pieces",cex=0.7)))

# EXPORT --------
writeAsciiGrid(bf.map, "infant_mortality.asc", attr="k.wprev.N300.RInf")
