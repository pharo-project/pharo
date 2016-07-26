private-writing
copyDataTo: aStream

	compressionMethod = CompressionStored ifTrue: [ ^self copyDataWithCRCTo: aStream ].

	self copyRawDataTo: aStream.