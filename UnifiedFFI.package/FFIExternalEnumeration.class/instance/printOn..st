printing
printOn: stream
	super printOn: stream.
	stream nextPut: $(;
		 nextPutAll: self item printString;
		 nextPut: $)