tests
testTimesAreLocal

	| remoteDateAndTime remoteTimestamp localTimestamp |
	remoteDateAndTime := DateAndTime current offset: DateAndTime localOffset + 2 hours.
	remoteTimestamp := DosTimestamp fromDateAndTime: remoteDateAndTime.
	localTimestamp := DosTimestamp fromDateAndTime: remoteDateAndTime asLocal.
	self assert: remoteTimestamp equals: localTimestamp.