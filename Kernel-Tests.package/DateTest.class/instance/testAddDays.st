tests
testAddDays

	self assert: (january23rd2004 addDays: 0) equals: '2004-01-23' asDate.	
	self assert: (january23rd2004 addDays: 31) equals: '2004-02-23' asDate.
	self assert: (january23rd2004 addDays: 366) equals: '2005-01-23' asDate.