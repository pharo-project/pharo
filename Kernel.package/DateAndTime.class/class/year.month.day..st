instance creation
year: year month: month day: day
	"Return a DateAndTime, midnight local time"
	^ self
		year: year
		month: month
		day: day
		hour: 0
		minute: 0