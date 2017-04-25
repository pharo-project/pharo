testing
testMethodDefinitionWithBangs

	| methodDefinition |
	
	methodDefinition := (RGMethodDefinition 
		realClass: self class
		selector: #methodWithBangs) asMCMethodDefinition.	
	methodDefinition setTimeStamp: ''.
	writer visitMethodDefinition: methodDefinition.
										
	self assertContentsOf: stream contents match: self expectedMethodDefinitionWithBangs.
	
	stream := ChunkReadStream on: stream contents readStream.
	
	self assert: stream next isAllSeparators.
	self assertChunkIsWellFormed: stream next.
	self assertMethodChunkIsWellFormed: stream next.
	self assert: stream next isAllSeparators 