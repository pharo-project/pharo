event handling
handleMonthNameTouched

	| newMonthName dayCount |
	newMonthName := UIManager default 
		chooseDropList: 'Choose a month:'
		list: #('January' 'February' 'March' 'April' 'May' 'June' 'July'
			'August' 'September' 'October' 'November' 'December').

	newMonthName isNil
		ifTrue: [^self].

	dayCount := (Month year: date year month: newMonthName) daysInMonth.
	self date: (Date 
		year: date year
		month: newMonthName
		day: (date dayOfMonth min: dayCount)).
	self changed.
