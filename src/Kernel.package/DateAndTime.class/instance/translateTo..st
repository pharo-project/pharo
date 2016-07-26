offset
translateTo: anOffset
	"Keep myself's representation and move it to another timezone offset.
	Note that unlike #offset: this WILL change the absolute time in UTC
	
	|t|
	t := DateAndTime now.
	t = (t offset: 2 hours).
	t = (t translateTo: 2 hours).
	" 
	self dayMonthYearDo: [ :day :month :year|
		^ self class
			year: year
			month: month
			day: day
			hour: self hour
			minute: self minute
			second: self second
			nanoSecond: self  nanoSecond
			offset: anOffset asDuration ]