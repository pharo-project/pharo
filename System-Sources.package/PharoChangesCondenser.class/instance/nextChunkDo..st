helper
nextChunkDo: aBlock
	stream nextChunkPut: (String streamContents: aBlock)