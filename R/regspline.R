"regspline" <-
function (x, y=NULL, xmin=min(x), n=length(x), deltat=(max(x)-min(x))/(n-1), rule=1, periodic=FALSE) {
	# We use spline() for the calculations
	# but we first need to calculate xmax
	if (n <= 0) 
    	stop("regspline requires n >= 1")
    if (deltat <= 0) 
    	stop("regspline requires deltat > 0")
	xmax <- (n-1) * deltat + xmin
	# ... and eliminate missing values
	ok <- !(is.na(x) | is.na(y))
	x <- x[ok]
    y <- y[ok]
	# Make sure data are sorted in increasing order according to x
	srt <- sort.list(x)
	x <- x[srt]
	y <- y[srt]
	# the call to the function spline() is different in S+ and in R!
	if (exists("is.R") && is.function(is.R) && is.R()) {	# We are in R
		if (periodic == TRUE) {
			res <- spline(x, y, n=n, method="periodic", xmin=xmin, xmax=xmax)
		} else {
			res <- spline(x, y, n=n, method="fmm", xmin=xmin, xmax=xmax)
		}
		# Rem: there is also a method="natural" in R, but we don't use it here!
	} else {												# We are in S+
		# In S+ the spline function does not really care of xmin, xmax and n!!!
		# So, we need to interpolate a lot more points, and use approx() in it
		# To get the regulated vector we really requested
		restmp <- spline(x, y, n=50*n, periodic=periodic, xmin=xmin, xmax=xmax)
		xout <- 0:(n-1)*deltat + xmin
		res <- approx(restmp$x, restmp$y, xout, method="linear", rule=2)
	}
	# The spline interpolations sometimes return interpolated values lower than the minimum
	# or higher than the maximum. This is eliminated by assigning them minimum or maximum value
	# For instance, a regulated count of species could return négative values => set the the
	# minimum count (usually 0)
	ymin <- min(y)
	ymax <- max(y)
	res$y[res$y < ymin] <- ymin
	res$y[res$y > ymax] <- ymax
	# If rule == 1, we still must replace values outside {x[1], x[nx]} by NA
    if (rule == 1)
    	res$y[res$x < x[1] | res$x > x[length(x)]] <- NA	
	res
}
