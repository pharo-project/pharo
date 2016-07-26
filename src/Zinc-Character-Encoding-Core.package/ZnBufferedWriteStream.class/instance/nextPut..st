accessing
nextPut: object
	self flushBufferIfFull.
	position := position + 1.
	self buffer at: position put: object