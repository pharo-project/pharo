converting
nextPut: character toStream: stream
	"Write the encoding for character to stream"
	
	"We should use #codePoint but #asInteger is faster"
	
	self nextPutCodePoint: character asInteger toStream: stream  