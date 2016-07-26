tests
testTimeZone
	self assert: aDateAndTime timeZoneName	= 'Universal Time'.
	self assert: aDateAndTime timeZoneAbbreviation	=  'UTC'

