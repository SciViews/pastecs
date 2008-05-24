"trend.test" <-
function(tseries, R=1) {
	Call <- deparse(substitute(tseries))
	if (exists("is.R") && is.function(is.R) && is.R()) {	# We are in R
		# Now done with Depends: field require(stats)
		x <- as.ts(tseries)
		Names <- colnames(x)
	} else {												# We are in S+
		x <- as.rts(tseries)
		Names <- names(x)
	}
	if (R < 2) {	# Simple test
		if (is.matrix(x) == TRUE) {	# Multiple time series	
			n <- ncol(x)
			Time <- time(x)
			res <- NULL
			res[[1]] <- list(statistics=1)
			for (i in 1:n) {
				res[[i]] <- cor.test(x[,i], Time, alternative="two.sided", method="spearman")
				res[[i]]$data.name <- paste(Names[i], " and time(", Names[i], ")", sep="")
			}
			names(res) <- Names
		} else {							# Single time series
			res <- cor.test(x, time(x), alternative="two.sided", method="spearman")
			res$data.name <- paste(Call, " and time(", Call, ")", sep="")
		}
	} else {		# Bootstrap test
		# Spearman's rho for a single time series
		test.trend <- function(Tseries) {
			rho <- cor(rank(Tseries), rank(time(Tseries)))
			rho
		}
		# Spearman's rho used for multiple time series
		test.trends <- function(Tseries) {
			data.rank <- apply(Tseries, 2, rank)
			rhos <- apply(data.rank, 2, cor, rank(time(Tseries)))
			rhos
		}	
		if (exists("is.R") && is.function(is.R) && is.R()) {		# We are in R
			require(boot)
			if (is.matrix(x) == TRUE && ncol(x) > 1) {
				res <- tsboot(x, test.trends, R = R, sim = "fixed", l = 1)
			} else {
				dim(x) <- NULL
				res <- tsboot(x, test.trend, R = R, sim = "fixed", l = 1)
			}
			boot.t <- res$t
			boot.t0 <- res$t0
			boot.R <- res$R
		} else {		# We are in S+
			if (is.matrix(x) == TRUE) {
				res <- bootstrap(as.matrix(x), test.trends, B = R)
			} else {
				res <- bootstrap(as.vector(x), test.trend, B = R)
            }
			boot.t <- res$replicates
			boot.t0 <- res$observed
			boot.R <- res$B	
		}
		# Calculate P-value associated with the bootstrap test
		n <- ncol(boot.t)
		if (is.null(n)) {		# Single test
			if (boot.t0 > 0) {	# Count larger values
				P <- sum(boot.t > boot.t0) / boot.R
			} else {			# Count smaller values
				P <- sum(boot.t < boot.t0) / boot.R
			}
		} else {				# Multiple tests
			P <- NULL
			if (boot.t0 > 0) {	# Count larger values
				for (i in 1:n)
					P[i] <- sum(boot.t[,i] > boot.t0[i]) / boot.R
			} else {			# Count smaller values
				for (i in 1:n)
					P[i] <- sum(boot.t[,i] < boot.t0[i]) / boot.R
			}
			names(P) <- dimnames(boot.t)[[2]]
			res$p.value <- P
		}
	}
	res
}
