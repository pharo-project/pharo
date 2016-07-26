private
writeChunkSize: size
	stream nextPutAll: (self isBinary ifTrue: [ size printStringHex asByteArray ] ifFalse: [ size printStringHex ]).
	self crlf