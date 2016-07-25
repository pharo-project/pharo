printing
printOn: stream
	super printOn: stream.
	id
		ifNotNil: [ 
			stream
				nextPut: $(;
				print: id;
				nextPut: $) ]