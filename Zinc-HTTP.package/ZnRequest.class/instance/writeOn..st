writing
writeOn: stream
	| bivalentWriteStream |
	bivalentWriteStream := ZnBivalentWriteStream on: stream.
	self requestLine writeOn: bivalentWriteStream.
	super writeOn: bivalentWriteStream