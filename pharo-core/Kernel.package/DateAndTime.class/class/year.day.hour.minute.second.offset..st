instance creation
year: year day: dayOfYear hour: hour minute: minute second: second offset: offset 
	"Return a DataAndTime"

	| y d |
	y := self
		year: year
		month: 1
		day: 1
		hour: hour
		minute: minute
		second: second
		nanoSecond: 0
		offset: offset.
	d := Duration days: (dayOfYear - 1).
	^ y + d