testing
testClassMethodDefinition
	writer visitMethodDefinition: (RGMethodDefinition realClass: self mockClassA class selector: #one) asMCMethodDefinition.
	self assert: (stream contents matchesRegex: self expectedClassMethodDefinition).
	
	stream := ChunkReadStream on: stream contents readStream.
	
	self assert: stream next isAllSeparators.
	self assertChunkIsWellFormed: stream next.
	self assertMethodChunkIsWellFormed: stream next.
	self assert: stream next isAllSeparators 