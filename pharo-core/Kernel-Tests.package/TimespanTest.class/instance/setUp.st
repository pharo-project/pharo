running
setUp

	localTimeZoneToRestore := DateAndTime localTimeZone.
	DateAndTime localTimeZone: TimeZone default.

	"100 hours starting noon 22 March 2003"
	timespan := Timespan starting:
					(DateAndTime year: 2003 month: 03 day: 22 hour: 12 minute: 0 second: 0)
						duration: (Duration hours: 100).

	dec31 := (DateAndTime year: 2004 month: 12 day: 31 hour: 0 minute: 0 second: 0).
	jan01 := (DateAndTime year: 2005 month: 1 day: 1 hour: 0 minute: 0 second: 0).
	jan08 := (DateAndTime year: 2005 month: 1 day: 8 hour: 0 minute: 0 second:0).
	aDay := Duration days: 1 hours: 0 minutes: 0 seconds: 0 nanoSeconds: 0.
	aWeek := Duration days: 7 hours: 0 minutes: 0 seconds: 0 nanoSeconds: 0.
	aTimespan := Timespan starting: jan01 duration: aWeek.
	anOverlappingTimespan := Timespan starting: dec31 duration: aWeek.
	anIncludedTimespan := Timespan starting: jan01 duration: aDay.
	aDisjointTimespan := Timespan starting: jan08 duration: aWeek.



