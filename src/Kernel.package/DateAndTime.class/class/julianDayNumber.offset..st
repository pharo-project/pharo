instance creation
julianDayNumber: aJulianDayNumber offset: aTimeZoneOffset
	"Return a DateAndTime at midnight local time at the given julian day"
	| ticks |
	"create a ticks representation in UTC, take the given julian day in local time"
	ticks := aJulianDayNumber days ticks.
	ticks at: 2 put: aTimeZoneOffset asSeconds negated.

	^ self basicNew
		ticks: ticks offset: aTimeZoneOffset;
		yourself