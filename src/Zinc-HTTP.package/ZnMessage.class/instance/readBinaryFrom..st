initialize-release
readBinaryFrom: stream
	| entityReader |
	self readHeaderFrom: stream.
	(entityReader := self entityReaderOn: stream)
		binary.
	self entity: entityReader readEntity