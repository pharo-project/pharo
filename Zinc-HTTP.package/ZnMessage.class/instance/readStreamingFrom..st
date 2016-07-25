initialize-release
readStreamingFrom: stream
	| entityReader |
	self readHeaderFrom: stream.
	(entityReader := self entityReaderOn: stream)
		streaming.
	self entity: entityReader readEntity
	