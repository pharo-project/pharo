testing
testByteOrders

	"Test that encoding / decoding a string witll produce the original string for both byte orders"

	"Default endianness"
	self assert: 'test' isTheSameAfterLittleEndianConversion: nil.
	"Big endian"
	self assert: 'test' isTheSameAfterLittleEndianConversion: false.
	"Little endian"
	self assert: 'test' isTheSameAfterLittleEndianConversion: true.
