"decdiff" <-
function(x, type="additive", lag=1, order=1, ends="fill") {
	call <- match.call()
	if (exists("is.R") && is.function(is.R) && is.R()) {	# We are in R
		x <- as.ts(x)
	} else {												# We are in S+
		x <- as.rts(x)
	}
	if (is.matrix(x) && ncol(x) != 1) 
		stop("only univariate series are allowed")
	if (!is.numeric(lag) || lag <= 0)
		stop("lag must be a positive number")
	if (!is.numeric(order) || order <= 0)
		stop("order must be a positive number")
	# Check the type argument
	TYPES <- c("additive", "multiplicative")
		typeindex <- pmatch(type, TYPES)
		if (is.na(typeindex)) 
			stop("invalid type value")
		if (typeindex == -1) 
			stop("ambiguous type value")
		# make sure type is fully spelled
		type <- switch(typeindex,
					"additive"="additive",
					"multiplicative"="multiplicative")
	# Check the ends argument and treat the series accordingly
	ENDS <- c("NAs", "fill", "drop")
	endsindex <- pmatch(ends, ENDS)
	if (is.na(endsindex)) 
		stop("invalid ends value")
	if (endsindex == -1) 
		stop("ambiguous ends value")
	# make sure ends is fully spelled
	ends <- switch(endsindex,
				"NAs"="NAs",
				"fill"="fill",
				"drop"="drop")
	# create our own specs component
	specs <- list(method="diff", type=type, lag=lag, order=order, ends=ends)
	# we recuperate units from x
	if (exists("is.R") && is.function(is.R) && is.R()) {	# We are in R
		units <- attr(x, "units")
	} else {
		units <- attr(attr(x, "tspar"), "units")
	}
	if (exists("is.R") && is.function(is.R) && is.R())	# We are in R
		# Now done with Depends: field require(stats)
	# The next function add enough data to the left (either NA or the mean of first few values)
	# to obtain a series of the same length as x after difference
	padleft <- function(x, Lag, fill) {
		if (exists("is.R") && is.function(is.R) && is.R()) {	# We are in R
			x <- window(x, start=start(lag(x, Lag)), end=end(x), extend=TRUE)
		} else {												# We are in S+
			x <-  ts.union(lag(x, Lag), x)[,2]
		}
		if (fill == TRUE)			# We fill padded data with the mean of first few values
			x[1:Lag] <- mean(x[(1:Lag)+Lag], na.rm=TRUE)
		x
	}
	filtered <- switch(endsindex,
					"NAs"=padleft(x, lag*order, fill=FALSE),				# We add NA's in front of the series
					"fill"=padleft(x, lag*order, fill=TRUE),				# We add the mean of first values in front of the series
					"drop"=x)												# We keep x like that
	# perform filtering
	filtered <- diff(filtered, lag=lag, difference=order)
	# Calculate residuals
	if (type == "additive") {
		residuals <- x - filtered
	} else {
		residuals <- x / filtered
	}
	series <- ts.intersect(filtered, residuals)
	# create our own 'tsd' structure
	res <- list(ts="series", series=series, units=units, specs=specs, call=call)
	class(res) <- "tsd"		# change the class of the object to 'tsd'
	res
}
