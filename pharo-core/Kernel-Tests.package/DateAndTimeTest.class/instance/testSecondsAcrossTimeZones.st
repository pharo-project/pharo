tests - epoch
testSecondsAcrossTimeZones

	| dateTime seconds dateTime2 utc |
	dateTime := '1/15/2012 0000+00:00' asDateAndTime.
	utc := TimeZone abbreviated: 'UTC'.

	self useTimeZone: 'PDT' during: [ :pdt |
		"Store a DateAndTime as seconds from the epoch"
		seconds := dateTime asSeconds.

		"Now move to Greenwich" 
		DateAndTime localTimeZone: utc. 
		dateTime2 := DateAndTime fromSeconds: seconds.
		
		self assert: dateTime equals: dateTime2 ].