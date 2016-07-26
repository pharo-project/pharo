testing
testDup

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder buildIR: [ : builder | 
		builder pushLiteral: 3;
		pushDup;
		send: #=;
		returnTop].

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
  	self assert: (aCompiledMethod valueWithReceiver: nil arguments: #() ).
	^iRMethod
	