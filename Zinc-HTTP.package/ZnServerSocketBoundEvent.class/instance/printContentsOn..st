printing
printContentsOn: stream
	super printContentsOn: stream.
	stream << ' Server Socket Bound '.
	address do: [ :each | stream print: each ] separatedBy: [ stream nextPut: $. ].
	stream nextPut: $:; print: port
	