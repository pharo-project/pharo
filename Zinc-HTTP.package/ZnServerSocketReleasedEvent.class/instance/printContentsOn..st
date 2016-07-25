printing
printContentsOn: stream
	super printContentsOn: stream.
	stream << ' Server Socket Released '.
	address do: [ :each | stream print: each ] separatedBy: [ stream nextPut: $. ].
	stream nextPut: $:; print: port