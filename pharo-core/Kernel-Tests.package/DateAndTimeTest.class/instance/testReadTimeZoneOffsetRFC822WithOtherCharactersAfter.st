tests - offset
testReadTimeZoneOffsetRFC822WithOtherCharactersAfter
	"See https://pharo.fogbugz.com/f/cases/17693/"
	
	self
		assert: (DateAndTime readTimezoneOffsetFrom: '+0530 CommiterName' readStream) printString
		equals: '0:05:30:00'