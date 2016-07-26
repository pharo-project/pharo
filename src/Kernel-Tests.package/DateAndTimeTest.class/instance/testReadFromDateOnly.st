tests
testReadFromDateOnly
	self 
		assert: '2014-02-19' asDateAndTime translateToUTC printString
		equals: '2014-02-19T00:00:00+00:00'.
	self 
		assert: '2014-02-19Z' asDateAndTime printString
		equals: '2014-02-19T00:00:00+00:00'.
	self 
		assert: '2014-02-19T+07:00' asDateAndTime printString
		equals:  '2014-02-19T00:00:00+07:00'.
	self 
		assert: '2014-02-19 -05:00' asDateAndTime printString
		equals:  '2014-02-19T00:00:00-05:00'.
