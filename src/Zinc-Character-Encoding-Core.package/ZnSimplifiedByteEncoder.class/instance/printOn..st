printing
printOn: stream
	super printOn: stream.
	stream
		nextPut: $(;
		print: identifier.
	strict
		ifTrue: [ stream nextPutAll: ' strict' ].
	stream nextPut: $)