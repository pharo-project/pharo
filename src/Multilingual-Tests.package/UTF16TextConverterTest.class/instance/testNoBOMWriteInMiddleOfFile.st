testing
testNoBOMWriteInMiddleOfFile
	
	"Test that BOM is not written when our first write does not occur at start of writestream"
	
	| converter stream |
	converter := UTF16TextConverter new.

	
	stream := String new writeStream.
	converter nextPutAll: 'testing' toStream: stream.
	stream position: 2.
	converter useByteOrderMark: true.
		
	converter nextPut: $a toStream: stream.
	self assert: 0 equals:  (stream contents indexOfSubCollection: 16rFEFF asByteArray asString)