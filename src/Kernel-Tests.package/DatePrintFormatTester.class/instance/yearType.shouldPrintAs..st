asserting
yearType: aNumber shouldPrintAs: aString 

	| actual tokens isMonthMissing yearTokenIndex |
	yearType := aNumber.
	tokens := self tokens.
	isMonthMissing := tokens size = 2.
	yearTokenIndex := isMonthMissing
		ifTrue: [ 2 ]
		ifFalse: [ 3 ].
	actual := tokens at: yearTokenIndex.
	
	self assert: actual equals: aString.