tests
testFromString
	self assert: aDateAndTime =  (DateAndTime fromString: ' 1970-01-01T00:00:00+00:00').
	self assert: aDateAndTime =  (DateAndTime fromString: ' 1970-01-01T00:00:00').
	self assert: aDateAndTime =  (DateAndTime fromString: ' 1970-01-01T00:00').
	self assert: aDateAndTime =  (DateAndTime fromString: ' 1970-01-01T00:00:00+00:00').
