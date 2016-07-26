enumerating
dayMonthYearDo: aBlock
	"Return the value of executing block with the Gregorian Calender day, month and year as arguments,
	as computed from my Julian Day Number, julianDayNumber.
	See http://en.wikipedia.org/wiki/Julian_date#Gregorian_calendar_from_Julian_day_number
    
    A short Description for the Constants used below:
    - 400 years span 146097 days in gregorian calendar.
    - 100 years span 36524 days, except every 400 years.
    - 4 years span 1461 days, except every 100 years.
    - 1 year spans 365 days, except every four years
    "

	| l n i j monthDay month fullYear |
	l := self julianDayNumber + 68569.
	n := 4 * l // 146097.
	l := l - (146097 * n + 3 // 4).
	i := 4000 * (l + 1) // 1461001.
	l := l - (1461 * i // 4) + 31.
	j := 80 * l // 2447.
	monthDay := l - (2447 * j // 80).
	l := j // 11.
	month := j + 2 - (12 * l).
	fullYear := 100 * (n - 49) + i + l.

	^ aBlock
		value: monthDay
		value: month
		value: fullYear.