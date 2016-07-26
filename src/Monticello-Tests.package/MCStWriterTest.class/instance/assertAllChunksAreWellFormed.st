asserting
assertAllChunksAreWellFormed
	stream := ChunkReadStream on: stream contents readStream.
	self assertChunkIsWellFormed: stream next.