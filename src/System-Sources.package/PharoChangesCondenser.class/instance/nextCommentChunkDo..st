helper
nextCommentChunkDo: aBlock
	stream cr; nextPut: $!.
	self nextChunkDo: aBlock.
	stream cr