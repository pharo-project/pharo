maintime
midnight
	"Answer a DateAndTime starting at midnight (towards the end of the day) local time"
	self dayMonthYearDo: [ :day :month :year|
		^self class 
			year: year
			month: month
			day: day 
			offset: offset ].