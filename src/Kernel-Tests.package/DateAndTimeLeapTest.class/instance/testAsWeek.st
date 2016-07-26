tests
testAsWeek
	self assert: aDateAndTime asWeek equals: ((Week starting: '02-29-2004' asDate) translateTo: 2 hours).
