testing
testReadIntoStartingAtCountBufferedNotAll
	"Tests for correct count and contents when all could be read, some pre-buffered"
	self doTestsForReading: 8000 
		intoBufferWithSize: 10000 
		startingAt: 500 
		fromFileOfSize: 6000 
		offsetBy: 200.