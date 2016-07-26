tests - under design
testReadFromNoOffset
	"self debug: #testReadFromNoOffset"

	self assert: (DateAndTime readFrom: '2010-01-05T20:33:14.321' readStream) offset equals: DateAndTime localOffset