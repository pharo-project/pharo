writing
writeOn: stream
	| bivalentWriteStream |
	bivalentWriteStream := ZnBivalentWriteStream on: stream.
	self statusLine writeOn: bivalentWriteStream.
	super writeOn: bivalentWriteStream