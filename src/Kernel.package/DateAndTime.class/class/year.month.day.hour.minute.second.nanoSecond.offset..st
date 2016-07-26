instance creation
year: year month: month day: day hour: hour minute: minute second: second nanoSecond: nanoCount offset: utcOffset
	"Return a DateAndTime with the values in the given TimeZone (UTCOffset)"

	| monthIndex daysInMonth p q r s julianDayNumber localSeconds utcSeconds|

	monthIndex := month isInteger ifTrue: [ month ] ifFalse: [ Month indexOfMonth: month ].
	(monthIndex between: 1 and: 12) ifFalse: [ DateError signal: 'There is no ', monthIndex printString, 'th month' ].
	daysInMonth := Month
		daysInMonth: monthIndex
		forYear: year.
	day < 1 ifTrue: [ DateError signal: 'day may not be zero or negative' ].
	day > daysInMonth ifTrue: [ DateError signal: 'day is after month ends' ].

	p := (monthIndex - 14) quo: 12.
	q := year + 4800 + p.
	r := monthIndex - 2 - (12 * p).
	s := (year + 4900 + p) quo: 100.

	julianDayNumber :=
		((1461 * q) quo: 4) +
			((367 * r) quo: 12) -
			((3 * s) quo: 4) +
			(day - 32075).
			
	localSeconds :=  hour * 60 + minute * 60 + second.
	utcSeconds := localSeconds - utcOffset asSeconds.
	
	^self basicNew
		setJdn: julianDayNumber
		seconds: utcSeconds
		nano: nanoCount
		offset: utcOffset;
		yourself