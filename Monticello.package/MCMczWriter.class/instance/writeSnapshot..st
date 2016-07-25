visiting
writeSnapshot: aSnapshot
	self addString: (self serializeDefinitions: aSnapshot definitions) at: 'snapshot/source.', self snapshotWriterClass extension encodedTo: 'utf8'.
	self addString: (self serializeInBinary: aSnapshot) at: 'snapshot.bin'