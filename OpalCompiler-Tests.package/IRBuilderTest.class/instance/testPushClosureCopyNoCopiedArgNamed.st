testing - blocks
testPushClosureCopyNoCopiedArgNamed

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		pushReceiver;
		pushClosureCopyCopiedValues: #() args: #(d) jumpTo: #block;
		pushLiteral: 1;
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
	