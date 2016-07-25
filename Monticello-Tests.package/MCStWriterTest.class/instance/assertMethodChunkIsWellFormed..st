asserting
assertMethodChunkIsWellFormed: chunk
	self class compiler
		source: chunk readStream;
		class: UndefinedObject;
		failBlock: [self assert: false];
		translate.
	