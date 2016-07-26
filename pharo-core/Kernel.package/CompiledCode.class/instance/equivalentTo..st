comparing
equivalentTo: aCompiledMethod
	^self = aCompiledMethod
	or: [self class == aCompiledMethod class
		and: [self numArgs = aCompiledMethod numArgs
		and: [self numLiterals = aCompiledMethod numLiterals
		and: [self methodNode = aCompiledMethod methodNode ]]]]