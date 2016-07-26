tests - offset
testReadTimeZoneOffsetRFC822
	"See https://pharo.fogbugz.com/f/cases/17693/"
	"See RFC822 : https://www.w3.org/Protocols/rfc822/"

	self
		assert: (DateAndTime readTimezoneOffsetFrom: '+0530' readStream) printString
		equals: '0:05:30:00'