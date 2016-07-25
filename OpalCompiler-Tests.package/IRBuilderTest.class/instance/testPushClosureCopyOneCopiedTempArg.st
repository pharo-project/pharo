testing - blocks
testPushClosureCopyOneCopiedTempArg

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		addTemps: #(c a);
		pushReceiver;
		pushLiteral: 1;
		storeTemp: #a;
		popTop;
		pushClosureCopyCopiedValues: #(a) args: #(e) jumpTo: #block;
		addTemps: #(d);  "the temp"
		pushTemp: #a;
		pushTemp: #e;
		send: #+;
		storeTemp:  #d;
		popTop;
		pushTemp: #d;
		blockReturnTop;
		jumpAheadTarget: #block;	
		pushLiteral: 1;
		send: #value:;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
     self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() ) = 2).
	^iRMethod
	