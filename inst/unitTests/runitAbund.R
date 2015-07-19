## runitsPhGcode.R test suite
## by Ph. Grosjean <phgrosjean@sciviews.org>
## Run it simply by example(unitTests.PhGcode) TODO: adapt this!

## Create a few objects we need for our tests
# ...

## Create a very simple 'svTest' object
#test_R <- svTest(function () {
#	checkTrue(1 < 2)
#})

## The test cases
.setUp <- function () {
	## Executed before each test function
	# ...
	library(pastecs)
	
	## Create a function (just an example, replace with real code here)
	foo <- function(x) return(x)
}

.tearDown <- function () {
	## Executed after each test function
	## Restore previous exclusion list
	# ...
	## Remove our object with tests in .GlobalEnv
	rm(foo, envir = .GlobalEnv)
	
	## Detach and unload package
	#detach("package:PhGcode", unload = TRUE)
}

testAbund <- function () {
	checkEquals("1:10", abund(1:10)$data, "Trivial data trial")
	checkException(abund("a"), "abund(\"a\") raises an exception")
}
