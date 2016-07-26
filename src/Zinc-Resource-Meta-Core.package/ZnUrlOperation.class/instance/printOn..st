printing
printOn: stream
	super printOn: stream.
	stream nextPut: $(; print: self class operation; space; print: self url.
	self argument ifNotNil: [ :arg | stream space; print: arg ].
	stream nextPut: $)