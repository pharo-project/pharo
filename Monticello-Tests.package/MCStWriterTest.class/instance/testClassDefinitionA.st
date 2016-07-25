testing
testClassDefinitionA
	writer visitClassDefinition: (self mockClassA asClassDefinition).
	self assertContentsOf: stream match: self expectedClassDefinitionA.
	stream := ChunkReadStream on: stream contents readStream.
	2 timesRepeat: [self assertChunkIsWellFormed: stream next]