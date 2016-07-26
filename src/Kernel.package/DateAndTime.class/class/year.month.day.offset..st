instance creation
year: year month: month day: day offset: anOffset
	"Return a DateAndTime, midnight in the timezone with the given offset"
	^ self
		year: year
		month: month
		day: day
		hour: 0
		minute: 0
		offset: anOffset 