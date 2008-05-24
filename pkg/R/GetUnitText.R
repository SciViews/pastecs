"GetUnitText" <-
function(series) {
	if (exists("is.R") && is.function(is.R) && is.R()) {	# We are in R
		Unit <- attr(series, "units")
	} else {
		Unit <- attr(attr(series, "tspar"), "units")	# In Splus, "units" is an attribute of "tspar"!!!
	}
	frequency <- frequency(series)
	deltat <- deltat(series)
	if (frequency == 1) pre <- "" else {
		if (round(frequency) == frequency) pre <- paste("1/", frequency, " ", sep="") else {
			pre <- paste(deltat, " ")
		}
	}
	if (is.null(Unit)) UnitTxt <- "" else {
		# Make sure unit is correctly spelled
		if (exists("is.R") && is.function(is.R) && is.R()) {	# We are in R
			# Rem: R v. 1.4.0 add casefold(), but 1.3.1 has only tolower()!!!
			if (tolower(Unit) == "years") Unit <- "years"
			if (tolower(Unit) == "year") Unit <- "years"
			if (tolower(Unit) == "y") Unit <- "years"
			if (tolower(Unit) == "weeks") Unit <- "weeks"
			if (tolower(Unit) == "week") Unit <- "weeks"
			if (tolower(Unit) == "days") Unit <- "days"
			if (tolower(Unit) == "day") Unit <- "days"
			if (tolower(Unit) == "d") Unit <- "days"
			if (tolower(Unit) == "hours") Unit <- "hours"
			if (tolower(Unit) == "hour") Unit <- "hours"
			if (tolower(Unit) == "h") Unit <- "hours"
			if (tolower(Unit) == "minutes") Unit <- "min"
			if (tolower(Unit) == "minute") Unit <- "min"
			if (tolower(Unit) == "min") Unit <- "min"
			if (tolower(Unit) == "secondes") Unit <- "sec"
			if (tolower(Unit) == "seconde") Unit <- "sec"
			if (tolower(Unit) == "sec") Unit <- "sec"	
		} else {												# We are in Splus
			if (casefold(Unit) == "years") Unit <- "years"
			if (casefold(Unit) == "year") Unit <- "years"
			if (casefold(Unit) == "y") Unit <- "years"
			if (casefold(Unit) == "weeks") Unit <- "weeks"
			if (casefold(Unit) == "week") Unit <- "weeks"
			if (casefold(Unit) == "days") Unit <- "days"
			if (casefold(Unit) == "day") Unit <- "days"
			if (casefold(Unit) == "d") Unit <- "days"
			if (casefold(Unit) == "hours") Unit <- "hours"
			if (casefold(Unit) == "hour") Unit <- "hours"
			if (casefold(Unit) == "h") Unit <- "hours"
			if (casefold(Unit) == "minutes") Unit <- "min"
			if (casefold(Unit) == "minute") Unit <- "min"
			if (casefold(Unit) == "min") Unit <- "min"
			if (casefold(Unit) == "secondes") Unit <- "sec"
			if (casefold(Unit) == "seconde") Unit <- "sec"
			if (casefold(Unit) == "sec") Unit <- "sec"
		}
		UnitTxt <- paste(pre, Unit, sep="")
		# Select some particular cases
		if (Unit == "years" & frequency == 12) UnitTxt <- "months"
		if (Unit == "years" & frequency == 24) UnitTxt <- "two-weeks"
		if (Unit == "years" & frequency == 4) UnitTxt <- "quarters"
		if (Unit == "weeks" & frequency == 7) UnitTxt <- "days"
		if (Unit == "days" & frequency == 1/7) UnitTxt <- "weeks"
		if (Unit == "days" & frequency == 24) UnitTxt <- "hours"
		if (Unit == "hours" & frequency == 1/24) UnitTxt <- "days"
		if (Unit == "hour" & frequency == 60) UnitTxt <- "min"
		if (Unit == "min" & frequency == 1/60) UnitTxt <- "hours"
		if (Unit == "min" & frequency == 60) UnitTxt <- "sec"
		if (Unit == "sec" & frequency == 1/60) UnitTxt <- "min"
	}
	UnitTxt
}
