printing
printOn: stream
	"Print an extra progress percentage if available"
	
	super printOn: stream.
	self isEmpty 
		ifFalse: [ 
			stream space; print: self percentage rounded; nextPut: $% ]