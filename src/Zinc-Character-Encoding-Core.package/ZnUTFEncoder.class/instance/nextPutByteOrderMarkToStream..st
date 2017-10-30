convenience
nextPutByteOrderMarkToStream: stream
	"Write the encoded byte-order-mark (BOM) to stream"
	
	self nextPutCodePoint: self byteOrderMark toStream: stream 