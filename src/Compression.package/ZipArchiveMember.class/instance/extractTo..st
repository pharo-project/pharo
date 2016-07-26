extraction
extractTo: aStream
	| oldCompression |
	self isEncrypted ifTrue: [ self error: 'encryption is unsupported' ].
	aStream binary.
	oldCompression := self desiredCompressionMethod: CompressionStored.
	self rewindData.
	self writeDataTo: aStream.
	self desiredCompressionMethod: oldCompression.
	self endRead.