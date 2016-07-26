converting
nextPut: character toStream: stream
	"Write the encoding for character to stream"
	
	self nextPutCodePoint: character codePoint toStream: stream  