tests
testAddMonths

	self assert: (january23rd2004 addMonths: 0) equals: '2004-01-23' asDate.
	self assert: (january23rd2004 addMonths: 1) equals: '2004-02-23' asDate.
	self assert: (january23rd2004 addMonths: 12) equals: '2005-01-23' asDate.