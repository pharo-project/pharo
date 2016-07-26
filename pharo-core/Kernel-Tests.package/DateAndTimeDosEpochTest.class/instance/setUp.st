running
setUp
	localTimeZoneToRestore := DateAndTime localTimeZone.
	aDateAndTime :=  DateAndTime localTimeZone: TimeZone default; dosEpoch.
	aTimeZone := TimeZone offset: (Duration minutes: 135) name: 'DOS Epoch Test Time Zone' abbreviation: 'DTZ'.
	aDuration := Duration days: 1 hours: 2 minutes: 3 seconds: 4 nanoSeconds: 5 