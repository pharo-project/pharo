tests - offset
testReadFromDefaultOffsetNotSpecified
	"self debug: #testReadFromDefaultOffsetSpecified"

	self assert: (DateAndTime readFrom: '2016-07-03T21:16:16.708241' readStream defaultOffset: Duration zero) offset equals: Duration zero.
