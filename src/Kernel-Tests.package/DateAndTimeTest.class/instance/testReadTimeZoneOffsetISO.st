tests - offset
testReadTimeZoneOffsetISO
	"See https://pharo.fogbugz.com/f/cases/17693/"
	"See ISO 8601 offset format"

	self
		assert: (DateAndTime readTimezoneOffsetFrom: '+05:30' readStream) printString
		equals: '0:05:30:00'