.gleissberg.calc <- function(n = 50, k = 48) {
  gleiss <- matrix(0, n - 2, k + 1)
  n_max <- nrow(gleiss)
  k_max <- ncol(gleiss)

  gleiss[, 1] <- 2
  gleiss[1, 2] <- 4

  for (i in 2:n_max) {
    gleiss[i, 2] <- 2 * gleiss[i - 1, 2] + 2 * gleiss[i - 1, 1]
    for (j in 3:k_max) {
      for (i in (j - 1):n_max) {
        gleiss[i, j] <- j * gleiss[i - 1, j] + 2 * gleiss[i - 1, j - 1] +
          (i - j + 2) * gleiss[i - 1, j - 2]
      }
    }
  }
  gleiss <- gleiss / gamma((3:n) + 1)		# gamma(n + 1) is equivalent to n!

  # This is the probability, giving any (n, k) pair...
  # but we want a table of right-tailed cumulated probabilities
  gleiss <- t(apply(gleiss, 1L, cumsum))

  as.matrix(gleiss)
}

.gleissberg.table <- .gleissberg.calc()

pgleissberg <- function(n, k, lower.tail = TRUE, two.tailed = FALSE) {
  # Make sure n and k have same length
  if (length(n) > length(k))
    k <- rep(k, length.out = length(n))
  if (length(n) < length(k))
    n <- rep(n, length.out = length(k))

  # Calculate Gleissberg probability for each (n, k) pair
  res <- rep(0, length(n))
  ok <- (n >= 3 & k >= 0 & k <= n - 2)

  if (sum(ok) > 0) {
    ncalc <- n[ok]
    kcalc <- k[ok]
    rescalc <- rep(0, length(ncalc))
    norm <- (ncalc >= 50)

    if (sum(norm) > 0) {
      # Normal approximation of Gleissberg distribution
      nnorm <- ncalc[norm]
      knorm <- kcalc[norm]
      means <- 2 / 3 * (nnorm - 2)
      vars <- (16 * nnorm - 29) / 90

      if (isTRUE(two.tailed)) {
        resnorm <- pnorm(knorm, means, sqrt(vars))
        rightpart <- resnorm > 0.5
        resnorm[rightpart] <- 1 - resnorm[rightpart]
        resnorm <- 2 * resnorm
      } else {# one-sided probability
        if (isTRUE(lower.tail)) {
          resnorm <- pnorm(knorm, means, sqrt(vars))
        } else {
          resnorm <- 1 - pnorm(knorm, means, sqrt(vars))
        }
      }
      rescalc[norm] <- resnorm
    }

    if (sum(!norm) > 0) {
      ng <- ncalc[!norm]
      kg <- kcalc[!norm]

      if (isTRUE(two.tailed)) {
        # As Gleissberg distribution is asymmetric,
        # we have to calculate both sides independently
        mu <- 2 / 3 * (ng - 2)
        delta <- abs(mu - kg)
        resg1 <- .gleissberg.table[ng - 2, mu - delta + 1]
        resg2 <- 1 - .gleissberg.table[ng - 2, mu + delta + 1]
        resg <- resg1 + resg2

        if (!is.null(ncol(resg)))
          resg <- diag(resg)

        } else {# one-sided probability
        resg <- .gleissberg.table[ng - 2, kg + 1]

        if (!is.null(ncol(resg)))
          resg <- diag(resg)

        if (isTRUE(lower.tail))
          resg <- 1 - resg
      }
      rescalc[!norm] <- resg
    }
    res[ok] <- rescalc
  }
  res
}
