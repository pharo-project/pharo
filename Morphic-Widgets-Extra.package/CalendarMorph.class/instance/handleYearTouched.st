event handling
handleYearTouched

	| newYear dayCount |
	newYear := UIManager default 
		chooseOrRequestFrom: (2000 to: 2020)
		lines: #()
		title: 'Choose a year:'.

	newYear isNil
		ifTrue: [^self].

	newYear := newYear asNumber.
	dayCount := (Month year: newYear month: date monthIndex) daysInMonth.
	self date: (Date 
			year: newYear
			month: date monthIndex 
			day: (date dayOfMonth min: dayCount)).
	self changed.