testing
testReadIntoStartingAtCountBufferedAll
	"Tests for correct count and contents when all could be read, some pre-buffered"
	self doTestsForReading: 5000 
		intoBufferWithSize: 6000 
		startingAt: 500 
		fromFileOfSize: 6000 
		offsetBy: 200.