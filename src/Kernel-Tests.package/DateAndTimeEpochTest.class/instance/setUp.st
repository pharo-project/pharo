running
setUp
     localTimeZoneToRestore := DateAndTime localTimeZone.
	aDateAndTime :=  DateAndTime localTimeZone: TimeZone default; epoch.
	aTimeZone := TimeZone offset: (Duration minutes: 135) name: 'Epoch Test Time Zone' abbreviation: 'ETZ'.
	aDuration := Duration days: 1 hours: 2 minutes: 3 seconds: 4 nanoSeconds: 5 