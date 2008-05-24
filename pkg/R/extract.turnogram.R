"extract.turnogram" <-
function(e, n, level=e$level, FUN=e$fun, drop=0, ...) {
	if (missing(n)) n <- NULL
	if (exists("is.R") && is.function(is.R) && is.R()) {	# We are in R
		data <- as.ts(eval(parse(text=e$data)))
		data <- window(data, start=start(data) + drop)
		if (level == 1) {	# Simply return the original time series
			res <- data
		} else {
			if (is.null(n) || n > length(data)) n <- length(data)
							
			# Check the validity of level
			if (level < 1 || level > n/3) stop("level must be a value between 1 and n/3!")
			res <- aggregate(data, nfrequency=frequency(data)/level, FUN=FUN)
		}
		if (NROW(res) < 10)
			warning("The extracted series contains very few data (n < 10)")
	} else {	# We are in S+
		data <- as.rts(eval(parse(text=e$data)))
		data <- window(data, start=start(data) + drop)
		if (level == 1) {	# Simply return the original time series
			res <- data
		} else {
			if (is.null(n) || n > length(data)) n <- length(data)
			# Check the validity of level
			if (level < 1 || level > n/3) stop("level must be a value between 1 and n/3!")
			res <- aggregate(data, nf=frequency(data)/level, fun=FUN)
		}
		if (length(res) < 10)
		warning("The extracted series contains very few data (n < 10)")
	}
	res
}
