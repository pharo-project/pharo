truncation and round off
rounded
	"Answer a Point that is the receiver's x and y rounded. Answer the receiver if its coordinates are already integral."

	self isIntegerPoint ifTrue: [ ^ self] .
	^ x rounded @ y rounded
