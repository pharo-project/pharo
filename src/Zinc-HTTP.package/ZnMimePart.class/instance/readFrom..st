initialize-release
readFrom: stream
	self headers: (ZnHeaders readFrom: stream).
	self entity: (self entityReaderOn: stream) readEntity
	