public access
browseStream: aStream

	self openOn: (RingChunkImporter fromStream: aStream)