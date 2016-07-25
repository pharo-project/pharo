tests
testDateOffsets
	| bst sast materialized object |
	bst := TimeZone timeZones third.
	sast := TimeZone timeZones fourth.
	
	DateAndTime localTimeZone: bst.
	self assert: DateAndTime now offset equals: 1 hour.
	object := Date fromDays: 37023.
	self assert: object start offset equals: 0 hours.
	materialized := self resultOfSerializeAndMaterialize: object.
	self assert: materialized start offset equals: 0 hours.
	
	DateAndTime localTimeZone: sast.
	self assert: DateAndTime now offset equals: 2 hours.
	object := Date fromDays: 37023.
	self assert: object start offset equals: 0 hours.
	materialized := self resultOfSerializeAndMaterialize: object.
	self assert: materialized start offset equals: 0 hours
	