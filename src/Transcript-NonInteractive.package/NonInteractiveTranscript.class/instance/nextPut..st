streaming
nextPut: aCharacter
	"Output character on the receiver, buffered, not yet shown"

	self critical: [  self stream nextPut: aCharacter ]