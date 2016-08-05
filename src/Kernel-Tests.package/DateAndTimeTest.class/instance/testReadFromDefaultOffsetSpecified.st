tests - offset
testReadFromDefaultOffsetSpecified
	"self debug: #testReadFromDefaultOffsetSpecified"

	self assert: (DateAndTime readFrom: '2016-07-03T21:16:16.708241+02:00' readStream defaultOffset: Duration zero) offset equals: (Duration hours: 2).
