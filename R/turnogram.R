# TODO: make a separate function to plot the turnogram
turnogram <- function(series, intervals = c(1, length(series)/5), step = 1,
    complete = FALSE, two.tailed = TRUE, FUN = mean, plotit = TRUE,
    level = 0.05, lhorz = TRUE, lvert = FALSE, xlog = TRUE) {

  call <- match.call()
  data <- deparse(substitute(series))
  fun <- deparse(substitute(FUN))
  if (!inherits(series, "ts"))
    stop("'series' must be a single regular time series")

  units <- attr(series, "units")
  units_txt <- GetUnitText(series)

  # Determine if there are many successive sequences of zeros
  x <- as.vector(series)
  n <- length(x)
  strips <- c(x[1] - 1, x[1:(n - 1)]) != x | x != 0
  ns <- length(x[strips])
  if (n > ns + 0.1 * n)
    warning("There are more than 10% of data as successive zeros, turnogram could be biased!")

  if (length(intervals) != 2)
    stop("'intervals' must be a vector with 2 values: (min, max)")
  if (any(intervals < 1) || any(intervals > n/3))
    stop("'intervals' must be larger or equal to 1, and smaller or equal to n/3")

  interv <- seq(intervals[1], intervals[2], step)
  if (length(interv) < 2)
    stop("Less than 2 intervals. Redefine intervals or step")

  n_vec <- nturns_vec <- nturns_min_vec <- nturns_max_vec <-
    info_vec <- info_min_vec <- info_max_vec <- interv

  # Calculate the first step for the turnogram
  turnogram_step1 <- function(series, interval, complete, fun) {
    if (isFALSE(complete)) {# Just start intervals from the first observation

      if (length(series) %% interval != 0)
        series <- c(series, rep(NA, interval - (length(series) %% interval)))

      dim(series) <- c(interval, length(series) %/% interval)
      # There may be NAs appended at the end of the series
      x <- apply(series, 2, fun, na.rm = TRUE)
      n <- length(x)
      nturns <- turnpoints(x)$nturns

      res <- list(n = n, nturns = nturns)

    } else {# Calculate each possible interval

      n <- nturns_vec <- NULL

      for (j in interval:1) {
        ser <- series[j:length(series)]

        if (length(ser) %% interval != 0)
          ser <- c(ser, rep(NA, interval - (length(ser) %% interval)))

        dim(ser) <- c(interval, length(ser) %/% interval)
        # There may be NAs appended at the end of the series
        x <- apply(ser, 2, fun, na.rm = TRUE)
        nturns_vec[j] <- turnpoints(x)$nturns
        n[j] <- length(x)
      }
      res <- list(n = n, nturns = nturns_vec)
    }
    res
  }

  # Calculate all first steps (n and nturns) for the turnogram
  if (isFALSE(complete)) {
    for (i in 1:length(interv)) {
      res <- turnogram_step1(x, interv[i], complete = complete, fun = FUN)
      n_vec[i] <- res$n
      nturns_vec[i] <- res$nturns
    }

    # Calculate I (bits of information) according to either Gleissberg
    # (n <= 50), or normal approximations (n > 50)
    info_vec <- -log(pgleissberg(n_vec, nturns_vec, two.tailed = two.tailed),
      base = 2)
    if (isTRUE(two.tailed)) {# We have to change sign if k > mu.
      rightpart <- nturns_vec > 2 * (n_vec - 2) / 3
      info_vec[rightpart] <- -info_vec[rightpart]
    }

    # By default the extraction level is set to the interval corresponding to
    # the maximum info value
    level <- interv[match(max(info_vec), info_vec)]

    res <- list(interval = interv, n = n_vec, turns = nturns_vec,
      info = info_vec, level = level)
  } else {

    for (i in 1:length(interv)) {
      res <- turnogram_step1(x, interv[i], complete = complete, fun = FUN)
      n_vec[i] <- max(res$n) # To change this!!!
      nturns <- res$nturns
      nturns_vec[i] <- mean(nturns)
      nturns_min_vec[i] <- min(nturns)
      nturns_max_vec[i] <- max(nturns)
      infos <- -log(pgleissberg(res$n, nturns, two.tailed = two.tailed),
        base = 2)
      if (isTRUE(two.tailed)) {# Change the sign if k > mu.
        rightpart <- nturns > 2 * (res$n - 2) / 3
        infos[rightpart] <- -infos[rightpart]
      }
      info_vec[i] <- mean(infos)
      info_min_vec[i] <- min(infos)
      info_max_vec[i] <- max(infos)

      # By default the extraction level is set to the interval corresponding to
      # the maximum info value
      level <- interv[match(max(info_vec), info_vec)]

      res <- list(interval = interv, n = n_vec, turns = nturns_vec,
        turns.min = nturns_min_vec, turns.max = nturns_max_vec,
        info = info_vec, info.min = info_min_vec, info.max = info_max_vec,
        level = level)
    }
  }
  as.data.frame(res)
  res$call <- call
  res$data <- data
  if (isTRUE(complete)) {
    res$type <- "Complete"
  } else {
    res$type <- "Simple"
  }
  res$fun <- fun
  if (isTRUE(two.tailed)) {
    res$proba <- "two-tailed probability"
  } else {
    res$proba <- "one-tailed probability"
  }
  res$units.text <- units_txt
  attr(res, "units") <- units

  # Plot the turnogram?
  if (isTRUE(plotit)) {
    info_level <- -log(level, base = 2)
    if (isTRUE(xlog)) {
      xlog_txt <- "x"
    } else {
      xlog_txt <- ""
    }
    if (isTRUE(two.tailed)) {
      imin <- -1.1 * info_level
    } else {
      imin <- 0
    }
    sub_txt <- paste(fun, "/", res$proba)

    if (isFALSE(complete)) {
      yrange_dat <- c(res$info, imin, 1.1 * info_level)
      yrange <- c(min(yrange_dat), max(yrange_dat))

      plot(res$interval, res$info, type = "l", log = xlog_txt, ylim = yrange,
        xlab = paste("interval", units_txt), ylab = "I (bits)",
        main = paste("Simple turnogram for:", data), sub = sub_txt)
    } else {

      yrange <- c(min(c(res$info.min, imin)),
        max(c(res$info.max, 1.1 * info_level)))

      plot(res$interval, res$info, type = "l", log = xlog_txt, ylim = yrange,
        xlab = paste0("interval (", units_txt, ")"), ylab = "I (bits)",
        main = paste("Complete turnogram for:", data), sub = sub_txt)
      lines(res$interval, res$info.min)
      lines(res$interval, res$info.max)
    }

    if (isTRUE(lhorz)) {
      if (isTRUE(two.tailed)) {
        abline(h = 0)
        abline(h = -info_level, lty = 2, col = 2)
      }
      abline(h = info_level, lty = 2, col = 2)
    }
    if (isTRUE(lvert))
      abline(v = level, lty = 2, col = 2)
  }

  class(res) <- "turnogram"
  res
}
