instance creation
starting: aDateAndTime duration: aDuration 
	"Override - a each month has a defined duration"
	| start adjusted days |
	start := aDateAndTime asDateAndTime.
	adjusted := DateAndTime
				year: start year
				month: start month
				day: 1.
	days := self 
		daysInMonth: adjusted month 
		forYear: adjusted year.
	^ super
		starting: adjusted
		duration: (Duration days: days)