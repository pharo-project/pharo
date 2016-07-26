printing
printContentsOn: stream
	super printContentsOn: stream.
	self bytes notNil ifTrue: [ 
		stream space; print: self bytes ]