asserting
assertChunkIsWellFormed: chunk
	self class compiler
		source:  chunk readStream ;
		class: UndefinedObject;
		noPattern: true;
		failBlock:  [self assert: false];
		translate.