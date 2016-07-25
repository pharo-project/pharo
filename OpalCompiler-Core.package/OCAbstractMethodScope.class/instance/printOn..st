printing
printOn: stream

	super printOn: stream.
	stream space.
	self scopeLevel printOn: stream.
