private-extensions-flbasicserializationtest
setUpForFLBasicSerializationTest
	<flExtensionOf: #FLBasicSerializationTest isSource: true>
	^ 'setUp
	super setUp.
	"Automatic timezone setting should have the same setting after the
	tests as before."
	currentTimeZone := {DateAndTime localTimeZone. DateAndTime automaticTimezone}.'