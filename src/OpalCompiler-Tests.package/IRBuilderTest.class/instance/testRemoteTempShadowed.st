testing - blocks
testRemoteTempShadowed

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		addTemps: #(a);  "we have one real temp"
		pushLiteral: 1;
		storeTemp: #a;
		popTop;
		pushClosureCopyCopiedValues: #() args: #() jumpTo: #block;
			addTemps: #(a);
			pushTemp: #a;                                                                                             "a shadows the outer one"
			send: #isNil;
	     	 	blockReturnTop;
		jumpAheadTarget: #block;	
		send: #value;
		returnTop;
		ir.
	
	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() ) = true).
	^iRMethod
	