printing
printContentsOn: stream
	super printContentsOn: stream.
	stream space; nextPutAll: method.
	stream space.
	url printPathQueryFragmentOn: stream.
	stream space; print: response.
	size ifNotNil: [ 
		stream space; print: size; nextPut: $B ].
	stream space; print: duration; nextPutAll: 'ms'