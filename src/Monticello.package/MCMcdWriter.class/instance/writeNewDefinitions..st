writing
writeNewDefinitions: aCollection
	self addString: (self serializeDefinitions: aCollection) at: 'new/source.', self snapshotWriterClass extension encodedTo: 'utf8'.