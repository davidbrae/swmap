library(fields)
data(ozone2)
y16<- ozone2$y[16,] 
good<- !is.na( y16)
y<- y16[good] 
x<- ozone2$lon.lat[ good,]
xknots<- cover.design( x, 25)$design  # select 50 knot points
out<- Krig( x, y, knots=xknots,  cov.function="Exp.cov", theta=1)  
surface(out)