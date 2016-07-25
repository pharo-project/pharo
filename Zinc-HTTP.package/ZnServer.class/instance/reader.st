options
reader
	"Return a block that when given a stream reads an entity from it."
	
	^ self optionAt: #reader ifAbsentPut: [ [ :stream | ZnRequest readFrom: stream ] ]