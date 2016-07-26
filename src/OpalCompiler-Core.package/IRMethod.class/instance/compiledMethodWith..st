translating
compiledMethodWith: trailer

	^compiledMethod 
		ifNil: [self generate: trailer ]
		ifNotNil: [compiledMethod]