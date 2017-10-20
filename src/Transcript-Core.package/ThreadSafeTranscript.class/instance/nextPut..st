streaming
nextPut: value
	"Output character on the receiver, buffered, not yet shown"

	self critical: [ stream nextPut: value ].
	^ value