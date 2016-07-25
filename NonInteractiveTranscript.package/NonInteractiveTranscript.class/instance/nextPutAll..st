streaming
nextPutAll: aCollection
	"Output string on the receiver, buffered, not yet shown"

	self critical: [ self stream nextPutAll: aCollection ]