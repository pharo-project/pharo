printing
printContentsOn: stream
	super printContentsOn: stream.
	stream << ' Server Connection Closed '.
	address do: [ :each | stream print: each ] separatedBy: [ stream nextPut: $. ]