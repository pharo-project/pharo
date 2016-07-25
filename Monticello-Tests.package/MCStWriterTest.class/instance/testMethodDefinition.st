testing
testMethodDefinition
	writer visitMethodDefinition: (RGMethodDefinition realClass: self mockClassA selector: #one) asMCMethodDefinition.
	self assertContentsOf: stream match: self expectedMethodDefinition.
	
	stream := ChunkReadStream on: stream contents readStream.
	
	self assert: stream next isAllSeparators.
	self assertChunkIsWellFormed: stream next.
	self assertMethodChunkIsWellFormed: stream next.
	self assert: stream next isAllSeparators 