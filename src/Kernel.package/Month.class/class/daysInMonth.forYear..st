accessing
daysInMonth: indexOrName forYear: yearInteger 

	| index |
	index := indexOrName isInteger 
				ifTrue: [indexOrName]
				ifFalse: [self indexOfMonth: indexOrName].
	^ (DaysInMonth at: index)
			+ ((index = 2
					and: [Year isLeapYear: yearInteger])
						ifTrue: [1] ifFalse: [0])