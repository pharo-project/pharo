accessing
indexOfMonth: aMonthName 

	1 to: 12 do: [ :i |  (aMonthName, '*' match: (MonthNames at: i)) ifTrue: [^i] ].
 	self error: aMonthName , ' is not a recognized month name'.