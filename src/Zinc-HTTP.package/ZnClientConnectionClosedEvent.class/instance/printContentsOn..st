printing
printContentsOn: stream
	super printContentsOn: stream.
	stream << ' Connection Closed '.
	address do: [ :each | stream print: each ] separatedBy: [ stream nextPut: $. ].
	stream nextPut: $:; print: port