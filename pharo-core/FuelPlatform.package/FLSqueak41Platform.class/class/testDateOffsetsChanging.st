private-extensions-flbasicserializationtest
testDateOffsetsChanging
	<flExtensionOf: #FLBasicSerializationTest>
	| bst sast object |
	bst := TimeZone timeZones third.
	sast := TimeZone timeZones fourth.
	
	DateAndTime localTimeZone: bst.
	self assert: DateAndTime now offset equals: 1 hour.
	object := Date fromDays: 37023.
	self assert: object start offset equals: 1 hour.
	self serialize:object.
	
	DateAndTime localTimeZone: sast.
	self assert: DateAndTime now offset equals: 2 hours.
	object := Date fromDays: 37023.
	self assert: object start offset equals: 2 hours.
	"serialization sould be independent of timezone change"
	self assert: self materialized start offset equals: 1 hour