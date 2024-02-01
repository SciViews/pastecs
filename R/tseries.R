# TODO: make this a generic function with methods for tsd and regul objects
tseries <- function(x) {
  if (!inherits(x, c("regul", "tsd")))
    stop("x must be a 'regul' or a 'tsd' object")

  if (inherits(x, "regul")) {
    if (ncol(x$y) == 1) {
      y <- x$y[[1]]
    } else {
      y <- as.matrix(x$y)
    }

    # Create a 'ts' object
    res <- ts(y, start = x$tspar$start, frequency = x$tspar$frequency)
    attr(res, "units") <- x$units

  } else if (inherits(x, "tsd")) {
    if (length(x$ts) == 1) {# Decomposition of a single series
      # x$series is already a ts or rts object
      res <- x$series

    } else {# De have the decomposition of several series
      # Bind all series together
      res <- x$series[[1]]
      cnames <- paste(x$ts[1], ".", dimnames(x$series[[1]])[[2]], sep = "")
      for (i in 2:length(x$ts)) {
        res2 <- x$series[[i]]
        cnames <- c(cnames, paste(x$ts[i], ".", dimnames(x$series[[i]])[[2]],
          sep = ""))
        res <- cbind(res, res2)
      }
      dimnames(res)[[2]] <- cnames
    }
    attr(res, "units") <- x$units
  }
  res
}
