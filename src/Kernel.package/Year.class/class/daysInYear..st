accessing
daysInYear: yearInteger

	^ 365 + ((self isLeapYear: yearInteger) ifTrue: [1] ifFalse: [0]).
