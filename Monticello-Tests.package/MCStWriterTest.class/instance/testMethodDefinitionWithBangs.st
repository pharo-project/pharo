testing
testMethodDefinitionWithBangs
	writer visitMethodDefinition: (RGMethodDefinition 
									realClass: self class
									selector: #methodWithBangs) asMCMethodDefinition.	
	self assertContentsOf: stream match: self expectedMethodDefinitionWithBangs.
	
	stream := ChunkReadStream on: stream contents readStream.
	
	self assert: stream next isAllSeparators.
	self assertChunkIsWellFormed: stream next.
	self assertMethodChunkIsWellFormed: stream next.
	self assert: stream next isAllSeparators 