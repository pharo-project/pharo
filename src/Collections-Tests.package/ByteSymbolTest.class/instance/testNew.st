tests - non-creation
testNew
	"self debug: #testNew"

	
	self should: [ByteSymbol new: 5 ] raise: self defaultTestError. 