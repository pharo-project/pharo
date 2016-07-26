instance creation
year: year month: month day: day hour: hour minute: minute offset: anOffset
	"Return a DateAndTime" 
	
	^ self
 		year: year
 		month: month
 		day: day
 		hour: hour
		minute: minute
		second: 0
		offset: anOffset