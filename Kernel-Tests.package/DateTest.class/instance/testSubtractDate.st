tests
testSubtractDate

	self assert: (january23rd2004 subtractDate: january23rd2004 previous) equals: 1.	
	self assert: (january23rd2004 subtractDate: january23rd2004) equals: 0.	
	self assert: (january23rd2004 subtractDate: january23rd2004 next) equals: -1.	
