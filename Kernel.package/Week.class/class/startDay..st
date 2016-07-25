accessing
startDay: aSymbol

	(DayNames includes: aSymbol)
		ifTrue: [ StartDay := aSymbol ]
		ifFalse: [ self error: aSymbol, ' is not a recognised day name' ]
