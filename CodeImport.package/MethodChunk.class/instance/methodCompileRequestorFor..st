importing
methodCompileRequestorFor: aRequestor
	aRequestor ifNotNil: [ aRequestor contents ifNil: [ aRequestor contents: contents ] ].
	^ MethodChunkCompilerRequestor new
		methodChunk: self;
		fileCompileRequestor: aRequestor;
		yourself