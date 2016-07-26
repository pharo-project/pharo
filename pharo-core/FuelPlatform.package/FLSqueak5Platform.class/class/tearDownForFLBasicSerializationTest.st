private-extensions-flbasicserializationtest
tearDownForFLBasicSerializationTest
	<flExtensionOf: #FLBasicSerializationTest isSource: true>
	^ 'tearDown
	DateAndTime localTimeZone: currentTimeZone first.
	DateAndTime automaticTimezone: currentTimeZone second.
	super tearDown'