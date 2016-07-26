running
setUp
	restoredStartDay := Week startDay.
	restoredTimeZone := DateAndTime localTimeZone.

	Week startDay: #Sunday.
	DateAndTime localTimeZone: (TimeZone timeZones detect: [:tz | tz abbreviation = 'GMT']).