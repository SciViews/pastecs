"extract.regul" <-
function(e, n, series=NULL, ...) {
	if (missing(n)) n <- ncol(e$y)
	nc <- ncol(e$y)
	if (is.null(nc)) nc <- 1		# if ncol() return null, we have just a single vector
	# if series is provided, we use it in priority
	if (is.null(series)) {
		if (n > nc) {
			if (nc > 1) warning(paste("Only", nc, "series exist in the object. Extract all series."))
			n <- nc
		}
		# We create a series value that correspond to the extraction of n first series
		series <- 1:n
	}
	if (nc == 1) {
		warning("Only one series in the object. Extract it.")
		y <- e$y[[1]]
	} else {			# Use series to determine which series to extract
		y <- as.matrix(e$y)[, series]
	}		
	# The treatment is different in R and in S+
	# In R, we create a 'ts' object, in S+, we create a 'rts' object
	if (exists("is.R") && is.function(is.R) && is.R()) {	# We are in R
		res <- ts(y, start=e$tspar$start, frequency=e$tspar$frequency)
		attr(res, "units") <- e$units
	} else {												# We are in S+
		res <- rts(y, start=e$tspar$start, frequency=e$tspar$frequency, units=e$units)
	}
	res
}
