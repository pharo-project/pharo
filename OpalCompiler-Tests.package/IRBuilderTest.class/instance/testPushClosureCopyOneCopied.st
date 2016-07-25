testing - blocks
testPushClosureCopyOneCopied

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		addTemps: #(c a);
		pushReceiver;
		pushLiteral: 1;
		storeTemp: #a;
		popTop;
		pushClosureCopyCopiedValues: #(a) args: #() jumpTo: #block;
		pushTemp: #a;
		blockReturnTop;
		jumpAheadTarget: #block;	
		send: #value;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
     self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() ) = 1).
	^iRMethod