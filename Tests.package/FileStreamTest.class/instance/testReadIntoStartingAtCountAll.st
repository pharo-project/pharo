testing
testReadIntoStartingAtCountAll
	"Tests for correct count and contents when all could be read, none pre-buffered"
	self doTestsForReading: 100 
		intoBufferWithSize: 400 
		startingAt: 100 
		fromFileOfSize: 200 
		offsetBy: 0.
	