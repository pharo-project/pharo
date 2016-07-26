extraction
extractTo: aStream from: start to: finish
	| oldCompression |
	self isEncrypted ifTrue: [ self error: 'encryption is unsupported' ].
	aStream binary.
	oldCompression := self desiredCompressionMethod: CompressionStored.
	self rewindData.
	self writeDataTo: aStream from: start to: finish.
	self desiredCompressionMethod: oldCompression.
	self endRead.