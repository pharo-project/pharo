testing - blocks
testPushClosureCopyNoCopied

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		pushReceiver;
		pushClosureCopyCopiedValues: #() args: #() jumpTo: #block;
		pushLiteral: 1;
		pushLiteral: 2;
		send: #+;
	      blockReturnTop;
		jumpAheadTarget: #block;	
		send: #value;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() ) = 3).
	^iRMethod
	