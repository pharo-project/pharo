streaming
nextPutAll: value
	"Output string on the receiver, buffered, not yet shown"

	self critical: [ stream nextPutAll: value ].
	^ value