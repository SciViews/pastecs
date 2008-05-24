"pgleissberg" <-
function(n, k, lower.tail=TRUE, two.tailed=FALSE) {
	# Make sure n and k have same length
	if (length(n) > length(k)) k <- rep(k, length.out=length(n))
	if (length(n) < length(k)) n <- rep(n, length.out=length(k))
	# Calculate Gleissberg probability for each (n, k) pair
	res <- rep(0, length(n))
	OK <- (n >= 3 & k >= 0 & k <= n-2)
	if (sum(OK) > 0) {
		ncalc <- n[OK]
		kcalc <- k[OK]
		rescalc <- rep(0, length(ncalc))
		Norm <- (ncalc > 50)
		if (sum(Norm) > 0) {
			# Normal approximation of Gleissberg distribution
			nnorm <- ncalc[Norm]
			knorm <- kcalc[Norm]
			Mean <- 2 / 3 * (nnorm - 2)
			Var <- (16 * nnorm - 29) / 90
			if (two.tailed == TRUE) {	# two-sided probability
				resnorm <- pnorm(knorm, Mean, sqrt(Var))
				rightpart <- resnorm > 0.5
				resnorm[rightpart] <- 1 - resnorm[rightpart]
				resnorm <- 2 * resnorm
			} else {					# one-sided probability
				if (lower.tail == TRUE) {
					resnorm <- pnorm(knorm, Mean, sqrt(Var))
				} else {
					resnorm <- 1 - pnorm(knorm, Mean, sqrt(Var))
				}
			}
			rescalc[Norm] <- resnorm
		}
		if (sum(!Norm) > 0) {
			# Calculate exact Gleissberg distribution
			# This is normally loaded from gleissberg.table
			# but if it fails, it can be recalculated with:
			"gleissberg.calc" <- function() {
				n <- 50
				k <- 48
				Gleiss <- matrix(0, n - 2, k + 1)
				N <- nrow(Gleiss)
				K <- ncol(Gleiss)
				Gleiss[,1] <- 2
				Gleiss[1, 2] <- 4
				for (n in 2:N) {
					Gleiss[n, 2] <- 2*Gleiss[n-1, 2] + 2*Gleiss[n-1, 1]
					for (k in 3:K) {
						for (n in (k-1):N) {
							Gleiss[n, k] <- k*Gleiss[n-1, k] + 2*Gleiss[n-1,k-1] + (n-k+2)*Gleiss[n-1, k-2]
						}
					}
				}
				Gleiss <- Gleiss / gamma(4:51)		# gamma(n + 1) is equivalent to n!
				# This is the probability, giving any (n, k) pair... but we want a table of right-tailed cumulated probabilities
				Gleiss <- t(apply(t(Gleiss), 2, cumsum))
				if (exists("is.R") && is.function(is.R) && is.R()) {	# We are in R
					assign(".gleissberg.table", Gleiss, env = .GlobalEnv)	
				} else {												# We are in S+
					assign(".gleissberg.table", Gleiss, where = 0)
				}
				invisible(NULL)
			}
			
			# Determination of Gleissberg probability
			ng <- ncalc[!Norm]
			kg <- kcalc[!Norm]
			if (exists("is.R") && is.function(is.R) && is.R()) {	# We are in R
				if (length(objects(envir=.GlobalEnv, all.names=TRUE, pattern=".gleissberg.table")) == 0) {	# Table not found
					try(data(gleissberg.table))
					if (length(objects(envir=.GlobalEnv, all.names=TRUE, pattern=".gleissberg.table")) == 0) {	# Table still not found
						cat("Creating Gleissberg distribution table...\n\n")
						if (R.Version()$os == "Win32") {flush.console()}
						gleissberg.calc()
					}
				}
			} else {												# We are in S+
				if (exists(".gleissberg.table") == FALSE) {	# Table not found
					try(data(gleissberg.table))
					if (exists(".gleissberg.table") == FALSE) {	# Table still not found
						cat("Creating Gleissberg distribution table...\n\n")
						gleissberg.calc()
					}
				}
			}
			.gleissberg.table <- as.matrix(.gleissberg.table)
			if (two.tailed == TRUE) { 	# two-sided probability
				# As Gleissberg distribution is asymmetric, we have to calculate both sides independently
				mu <- 2 / 3 * (ng - 2)
				delta <- abs(mu - kg)
				resg1 <- .gleissberg.table[ng - 2, mu - delta + 1]
				resg2 <- 1 - .gleissberg.table[ng - 2, mu + delta + 1]
				resg <- resg1 + resg2
				if (!is.null(ncol(resg))) resg <- diag(resg)
			} else {					# one-sided probability
				resg <- .gleissberg.table[ng - 2, kg + 1]
				if (!is.null(ncol(resg))) resg <- diag(resg)
				if (lower.tail == FALSE) resg <- 1 - resg
				}
			rescalc[!Norm] <- resg
		}
		res[OK] <- rescalc
	}
	res
}
