read, write, position
position
	"Return the receiver's current file position. If the stream is not positionable,
	as in the case of a Unix pipe stream, answer 0."

	^ [super position]
		on: Error
		do: [0]
