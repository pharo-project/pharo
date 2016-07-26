tests
testDateOffsetsChanging
	| bst sast object |
	bst := TimeZone timeZones third.
	sast := TimeZone timeZones fourth.
	
	DateAndTime localTimeZone: bst.
	self assert: DateAndTime now offset equals: 1 hour.
	object := Date fromDays: 37023.
	self assert: object start offset equals: 0 hours.
	self serialize:object.
	
	DateAndTime localTimeZone: sast.
	self assert: DateAndTime now offset equals: 2 hours.
	object := Date fromDays: 37023.
	self assert: object start offset equals: 0 hours.
	self assert: self materialized start offset equals: 0 hours
	