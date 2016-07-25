printing
printOn: stream
	timestamp printYMDOn: stream.
	stream space.
	timestamp printHMSOn: stream. 
	stream space.
	id \\ 1000 printOn: stream base: 10 length: 3 padded: true.
	self printContentsOn: stream