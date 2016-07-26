tests
testTimeZone
	aDateAndTime := '2004-02-29T13:33:00+00:00' asDateAndTime.
	self assert: aDateAndTime timeZone name equals: 'Universal Time'.
	self assert: aDateAndTime timeZone abbreviation 	equals: 'UTC'

