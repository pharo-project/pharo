tests
testReadFrom

	self 
		assert: '-1199-01-05T20:33:14.321-05:00' asDateAndTime printString
		equals: '-1199-01-05T20:33:14.321-05:00'.
	self 
		assert: '2002-05-16T17:20:45.1+01:01' asDateAndTime printString
		equals:  '2002-05-16T17:20:45.1+01:01'.
	self 
		assert:	' 2002-05-16T17:20:45.02+01:01' asDateAndTime printString
		equals:  '2002-05-16T17:20:45.02+01:01'.  
	self 
		assert:	'2002-05-16T17:20:45.000000009+01:01' asDateAndTime printString
		equals:   '2002-05-16T17:20:45.000000009+01:01'.
	self 
		assert: ' 2002-05-16T17:20' asDateAndTime translateToUTC printString
		equals:  '2002-05-16T17:20:00+00:00'.
	self 
		assert: '2002-05-16T17:20:45' asDateAndTime translateToUTC printString
		equals:  '2002-05-16T17:20:45+00:00' .
	self 
		assert: ' 2002-05-16T17:20:45+01:57' asDateAndTime printString
		equals:  '2002-05-16T17:20:45+01:57'.
	self 
		assert: ' 2002-05-16T17:20:45-02:34' asDateAndTime
		equals:  '2002-05-16T17:20:45-02:34' asDateAndTime.
	self 
		assert: '2002-05-16T17:20:45+00:00' asDateAndTime
		equals:  '2002-05-16T17:20:45+00:00' asDateAndTime.
	self 
		assert: '1997-04-26T01:02:03+01:02:3' asDateAndTime
		equals:  '1997-04-26T01:02:03+01:02:3' asDateAndTime