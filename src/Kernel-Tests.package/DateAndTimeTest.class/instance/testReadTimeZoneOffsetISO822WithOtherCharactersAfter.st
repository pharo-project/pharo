tests - offset
testReadTimeZoneOffsetISO822WithOtherCharactersAfter
	"See https://pharo.fogbugz.com/f/cases/17693/"

	self assert: (DateAndTime readTimezoneOffsetFrom: '+05:30 CommiterName' readStream) printString equals: '0:05:30:00'