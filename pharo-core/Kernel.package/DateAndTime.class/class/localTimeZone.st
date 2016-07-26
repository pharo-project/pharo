time zones
localTimeZone
	"Answer the local time zone"

	^ LocalTimeZone ifNil: [ LocalTimeZone := TimeZone default ]

