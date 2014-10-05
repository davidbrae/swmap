INCLUDE PICTURES this time!!
============
re-read e-mail from lumley & hadley


============
hi, this is a modified example of [[LINK TO OTHER POST]].  rather than worry about mapping and geocoding, this example uses very simplistic data to pinpoint where i am having trouble.  again, my goal is to replicate (and publish) the methods used to make [[these maps]].  this is the last thing i do not know how to do, so i would love your help.

first, here is some sample data.  it is a 5x5 grid of zeroes and ones.

	library(fields)

	x <-
		data.frame(
			lat = rep( 1:5 , 5 ) ,
			lon = rep( 1:5 , each = 5 ) ,
			values = c( rep( 1:0 , 12 ) , 1 )
		)

if you fit and plot this with the fields:::Krig function, you get yellow dots where the two numbers add up to an even number and blue dots where they add up to an odd number.  that makes sense, since add-to-even dots have x$values==1 and add-to-odd dots have x$values==0.

	fit1 <-
		Krig(
			cbind( x$lat , x$lon ) ,
			x$values ,
			weights = rep( 1 , 25 )
		)
		
	surface( fit1 )

the above example just uses equal weighting.  when you add some weight to point (1,1) the entire interpolated result tilts toward (1,1).  the weight of point (1,1) is one-one-thousandth of a percent bigger than the other points, and this completely destroys the original 5x5 image.

	fit2 <-
		Krig(
			cbind( x$lat , x$lon ) ,
			x$values ,
			weights = c( 1.001 , rep( 1 , 24 ) )
		)
		
	surface( fit2 )

but the behavior i would like to correct is what happens when the weight *is* hugely different.  in this next case, point (1,1) has a weight 1000x other points.
	
	fit3 <-
		Krig(
			cbind( x$lat , x$lon ) ,
			x$values ,
			scale.type='range',
			x.scale = c( 1000 , rep( 1 , 24 ) )
		)
		
	surface( fit3 )

the kriging function arrives at nearly the same result as point (1,1) weighted at 1.001.

how can i make a hugely disproportionately-weighted point (1,1) spill over onto the rest of the points on this graph?

`surface( fit3 )` looks like `surface( fit2 )`

what parameters to a kriging function can i use so that `surface( fit3 )` turns the entire plot bright red, radiating out over the other points due to its enormous weight.