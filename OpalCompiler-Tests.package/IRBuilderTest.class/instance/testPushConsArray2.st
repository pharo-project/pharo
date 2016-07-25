testing
testPushConsArray2

	| iRMethod aCompiledMethod receiver |
	iRMethod := IRBuilder new
		pushLiteral: 'hi!';
		pushConsArray: 1;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	
	receiver :=  (5@8).

	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (((aCompiledMethod valueWithReceiver: receiver arguments: #()))= #('hi!')).
	^iRMethod
