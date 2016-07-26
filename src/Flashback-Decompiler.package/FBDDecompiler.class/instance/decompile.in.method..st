public api
decompile: aSelector in: aClass method: aCompiledMethod
	builder := FBDASTBuilder newFor: aClass.
	(self canDecompile: aCompiledMethod)
		ifFalse: [ ^ self generateNativeBoostCallErrorMethodFrom: aCompiledMethod ].
	self initializeForMethod: aCompiledMethod.
	self interpretMethod: aCompiledMethod.
	^ FBDOptimizedMessagesRewriter new
		rewriteAST:
			(builder
				codeMethod: aSelector
				arguments: (simulatedStack first: aCompiledMethod numArgs)
				body: currentSequence
				pragmas: (self pragmasForMethod: aCompiledMethod)
				class: aClass)