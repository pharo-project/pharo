tests
testSubtractDays

	self assert: (january23rd2004 subtractDays: 0) equals: '2004-01-23' asDate.	
	self assert: (january23rd2004 subtractDays: 30) equals: '2003-12-24' asDate.