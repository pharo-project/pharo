time zones
localTimeZone: aTimeZone
	"Set the local time zone"

	"
	DateAndTime localTimeZone: (TimeZone offset:  0 hours name: 'Universal Time' abbreviation: 'UTC').
	DateAndTime localTimeZone: (TimeZone offset: -8 hours name: 'Pacific Standard Time' abbreviation: 'PST').
	"

	LocalTimeZone := aTimeZone


