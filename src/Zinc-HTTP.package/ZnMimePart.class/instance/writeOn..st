writing
writeOn: stream
	self headers writeOn: stream.
	stream nextPutAll: String crlf.
	self hasEntity ifTrue: [ 
		(self entityWriterOn: stream) writeEntity: self entity ]