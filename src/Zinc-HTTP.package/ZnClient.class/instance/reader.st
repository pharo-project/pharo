options
reader
	"Return a block that when given a stream reads a response from it."
	
	^ self optionAt: #reader ifAbsentPut: [ [ :stream | ZnResponse readFrom: stream ] ]