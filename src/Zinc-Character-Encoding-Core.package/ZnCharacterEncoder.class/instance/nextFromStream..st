converting
nextFromStream: stream
	"Read and return the next character from stream"
	
	"We should use #codePoint: but #value: is faster"
	
	^ Character value: (self nextCodePointFromStream: stream)