"lines.abund" <-
function(x, n=x$n, lvert=TRUE, lvars=TRUE, col=2, lty=2, ...) {
	# The following function actually draws the lines
	abund.lines <- function(X, N, Lvert, Lvars, Col, Lty, ...) {
		if (is.null(N)) { 	# Missing n argument
			stop("You must provide a value for n!")
		} else {				# We draw the lines
			p <- length(X$p.log.ind)
			# Verify N
			if (N < 0) N <- 0
			if (N > p) N <- p
			if (Lvert==TRUE)		# We draw a vertical line
				lines(c(N+0.5, N+0.5), c(-10,110), lty=Lty, col=Col)
			if (Lvars==TRUE)		# We change colors of selected variables labels
				if (exists("is.R") && is.function(is.R) && is.R()) {	# We are in R
					axis(1, 1:N, labels=as.character(X$vr[1:N]), col.axis=Col)
				} else {	# We are in S+, axis color is set by col
					axis(1, 1:N, labels=as.character(X$vr[1:N]), col=Col)
				}
		}
	}
	invisible(abund.lines(x, n, lvert, lvars, col, lty, ...))
}
