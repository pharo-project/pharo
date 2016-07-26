testing
testInitializerDefinition
	|chunk lastChunk penultChunk|
	writer writeSnapshot: self mockSnapshot.
	stream := ChunkReadStream on: stream contents readStream.
	
	[stream atEnd] whileFalse:
		[chunk := stream next.
		chunk isAllSeparators ifFalse: [
			penultChunk := lastChunk.
			lastChunk := chunk]].
	self assertContentsOf: penultChunk readStream match: self expectedInitializerA.
	self assertContentsOf: lastChunk readStream match: self expectedInitializerASubclass