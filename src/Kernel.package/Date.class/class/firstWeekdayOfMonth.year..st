accessing
firstWeekdayOfMonth: month year: year
	"Answer the weekday index of the first day in <month> in the <year>."

	^ (self year: year month: month day: 1) weekdayIndex
