testing
testReadIntoStartingAtCountNotAll
	
	"Tests for correct count and contents when not all could be read, and none pre-buffered"
	self doTestsForReading: 500 
		intoBufferWithSize: 800 
		startingAt: 10 
		fromFileOfSize: 300 
		offsetBy: 0.