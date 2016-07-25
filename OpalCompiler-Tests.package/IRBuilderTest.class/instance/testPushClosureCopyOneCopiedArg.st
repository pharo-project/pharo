testing - blocks
testPushClosureCopyOneCopiedArg

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		addTemps: #(c a);
		pushReceiver;
		pushLiteral: 1;
		storeTemp: #a;
		popTop;
		pushClosureCopyCopiedValues: #(a) args: #(d) jumpTo: #block;
		pushTemp: #a;
		pushTemp: #d;
		send: #+;
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
	