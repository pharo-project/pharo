instance creation
year: aYear week: weekNumber
	"Return the ISO week for the given week number and the given year.
	Week 1 contains the year's first Thursday (weekday = 5)"
	^ self starting: aYear asYear firstThursday + (weekNumber * 7 - 5"adjust for firstThursday") days