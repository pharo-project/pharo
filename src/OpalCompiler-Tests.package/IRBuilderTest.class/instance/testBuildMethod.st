testing - builder
testBuildMethod
	|  aCompiledMethod |
	
	aCompiledMethod := IRBuilder buildMethod: [ : builder | 
		builder pushLiteral: 3;
		pushDup;
		send: #=;
		returnTop].

	self assert: (aCompiledMethod isKindOf: CompiledMethod).