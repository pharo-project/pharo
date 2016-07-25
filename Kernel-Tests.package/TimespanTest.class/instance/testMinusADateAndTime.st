tests
testMinusADateAndTime
	"It appears that subtracting a date from a Timespan gives you a duration = to the difference between the start of the timespan and the date "
	self assert: aTimespan - dec31 =  aDay.
	self assert: aDisjointTimespan - jan01 =  aWeek.


