tests
testDayOfWeekWithUTC
	| date |
	"Calculating the day of week should take into account the UTC offset"
	date := DateAndTime julianDayNumber: 2456385 offset:(Duration hours:2).
	"Internally, this date gets represented as the julian day 2456384 with seconds 79200 and offset 2 hours"
	"When asking for the day of week, the offset should be taken into account to return the correct day of week"
	self assert: date dayOfWeek equals: 3.
	