initialize-release
readBinaryFrom: stream
	| entityReader |
	self headers: (ZnHeaders readFrom: stream).
	(entityReader := self entityReaderOn: stream)
		binary.
	self entity: entityReader readEntity