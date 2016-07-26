tests
testYearsDo
	| yearArray |
	yearArray := Array
				with: (Year year: 2004)
				with: (Year year: 2005).
	self assert: aTimespan years equals: yearArray
