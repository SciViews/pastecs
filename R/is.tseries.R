"is.tseries" <-
function(x) {
	if (exists("is.R") && is.function(is.R) && is.R()) {	# We are in R
		# Now done with Depends: field require(stats)
		is.ts(x)
	} else {												# We are in S+
		is.ts(x) | is.rts(x)
	}
}
