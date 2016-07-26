printing
printContentsOn: stream
	super printContentsOn: stream.
	stream space; nextPutAll: subject asString