setup
setUp

	localTimeZoneToRestore := DateAndTime localTimeZone.
	DateAndTime localTimeZone: TimeZone default.
	time := self timeClass fromSeconds: 14567.		"4:02:47 am"
	aTime := Time readFrom: '12:34:56 pm' readStream
