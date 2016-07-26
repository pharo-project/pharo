printing
printContentsOn: stream
	super printContentsOn: stream.
	stream space.
	request requestLine printMethodAndUriOn: stream.
	stream space; print: response code.
	response hasEntity
		ifTrue: [ 
			stream space; print: response entity contentLength; nextPut: $B ].
	stream space; print: self duration; nextPutAll: 'ms'