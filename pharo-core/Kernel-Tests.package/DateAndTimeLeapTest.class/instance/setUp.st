running
setUp
	aDateAndTime := (DateAndTime 
		year: 2004 month: 2 day: 29 
		hour: 13 minute: 33 second: 0 
		offset: 2 hours).
	aTimeZone := TimeZone default.
	aDuration := Duration days: 0 hours: 13 minutes: 33 seconds: 0 nanoSeconds: 0
