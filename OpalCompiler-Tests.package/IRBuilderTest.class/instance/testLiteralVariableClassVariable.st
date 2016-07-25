testing
testLiteralVariableClassVariable

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		pushLiteralVariable: (DateAndTime bindingOf: #LocalTimeZone); 	
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.

	self assert: (aCompiledMethod isKindOf: CompiledMethod).
     self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() ) = DateAndTime localTimeZone).
	^iRMethod
	