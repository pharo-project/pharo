converting
nextFromStream: stream
	"Read and return the next character from stream"
	
	| codePoint |
	codePoint := self nextCodePointFromStream: stream.
	^ Character codePoint: codePoint