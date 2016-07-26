printing
printContentsOn: stream
	super printContentsOn: stream.
	self string notNil ifTrue: [ 
		stream space; nextPutAll: self string ]