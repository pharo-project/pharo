writing
writeOn: stream
	| bivalentWriteStream |
	bivalentWriteStream := ZnBivalentWriteStream on: stream.
	self headers writeOn: bivalentWriteStream.
	bivalentWriteStream nextPutAll: String crlf.
	self hasEntity ifTrue: [ 
		(self entityWriterOn: bivalentWriteStream) writeEntity: self entity ]