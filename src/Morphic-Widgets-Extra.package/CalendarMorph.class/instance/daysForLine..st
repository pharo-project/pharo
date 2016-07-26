private
daysForLine: aNumber
	"Return an array of numbers that correspond to the day-of-month numbers of the given line (row) 
	in the calendar for the month of the receiver's date."

	| dayCount firstWeekday previousDayCount previousMonthDays lastDay |
	dayCount := date month daysInMonth.
	firstWeekday := Date firstWeekdayOfMonth: date monthIndex year: date year.
	previousDayCount := date month previous daysInMonth.
	"First case - handle the first line specially"
	aNumber = 1
		ifTrue: [
			"If this month's first day is Sunday, the first line is the last week from last month"
			firstWeekday = 1
				ifTrue: [^(previousDayCount - 6 to: previousDayCount) asArray].

			"Otherwise, its a mix of last month and this month"
			previousMonthDays := (firstWeekday - 1 to: 1 by: -1) collect: [:each | previousDayCount - each + 1].
			^previousMonthDays, ((1 to: 7) asArray copyFrom: 1 to: 7 - previousMonthDays size)].

	"Recompute the last day from the previous line (I love recursion)"
	lastDay := (self daysForLine: aNumber - 1) last.
	"Second case - the first week of this month starts on Sunday"
	(aNumber = 2 and: [lastDay = previousDayCount])
		ifTrue: [^(1 to: 7) asArray].

	"Third case - the first week of next month starts on Sunday"
	lastDay = dayCount
		ifTrue: [^(1 to: 7) asArray].

	"Fourth case - everything else"
	^(lastDay + 1 to: lastDay + 7) collect: [:each |
		each <= dayCount
			ifTrue: [each]
			ifFalse: [each - dayCount]]