initialize-release
readEntityFrom: stream
	self entity: (self entityReaderOn: stream) readEntity
	