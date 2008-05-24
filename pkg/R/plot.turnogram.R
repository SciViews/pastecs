"plot.turnogram" <-
function(x, level=0.05, lhorz=TRUE, lvert=TRUE, lcol=2, llty=2, xlog=TRUE, xlab=paste("interval (", x$units.text, ")", sep=""), ylab="I (bits)", main=paste(x$type, "turnogram for:",x$data), sub=paste(x$fun, "/", x$proba), ...) {
	# The next function actually draws the graph
	turnogram.graph <- function(X, Level, Xlog, Lhorz, Lvert, Lcol, Llty, Xlab, Ylab, Main, Sub, ...) {
		Ilevel <- -log(Level, base=2)
		if (Xlog == TRUE) xlogstr <- "x" else xlogstr <- ""
		if (X$proba == "two-tailed probability") {
			imin <- -1.1*Ilevel
			two.tailed <- TRUE
		} else {
			imin <- 0
			two.tailed <- FALSE
		}
		if (X$type == "Simple") {
			yrange.dat <- c(X$info, imin, 1.1*Ilevel)
			yrange <- c(min(yrange.dat), max(yrange.dat))
			plot(X$interval, X$info, type="l", log=xlogstr, ylim=yrange, xlab=Xlab, ylab=Ylab, main=Main, sub=Sub, ...)
		} else {
			yrange <- c(min(c(X$info.min, imin)), max(c(X$info.max, 1.1*Ilevel)))
			plot(X$interval, X$info, type="l", log=xlogstr, ylim=yrange, xlab=Xlab, ylab=Ylab, main=Main, sub=Sub, ...)
			lines(X$interval, X$info.min)
			lines(X$interval, X$info.max)
		}
		if (Lhorz == TRUE) {
			if (two.tailed == TRUE) {
				abline(h=0)
				abline(h=-Ilevel, lty=Llty, col=Lcol)
			}
			abline(h=Ilevel, lty=Llty, col=Lcol)
		}
		if (Lvert == TRUE) abline(v=X$level, lty=Llty, col=Lcol)
	}
	invisible(turnogram.graph(x, level, xlog, lhorz, lvert, lcol, llty, xlab, ylab, main, sub, ...))
}
