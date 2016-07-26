asserting
monthType: aNumber shouldPrintAs: aString 

	| actual |
	monthType := aNumber.
	actual := self tokens at: monthPosition.
	self assert: actual equals: aString.