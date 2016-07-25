testing - blocks
testRemoteTemp

	| iRMethod aCompiledMethod |
	iRMethod := IRBuilder new
		addTemps: #(a c);  "we have one real temp"
		createTempVectorNamed:#methodVector withVars: #(b);      "b we know will be written to, so make a tempvector entry" 
		pushLiteral: 1;
		storeTemp: #a;
		popTop;
		pushClosureCopyCopiedValues: #(#a #c #methodVector) args: #() jumpTo: #block;
			pushTemp: #a;                                                                                             "a is just read, so we copy it to the block"
			pushLiteral: 1;
			send: #+;
			storeRemoteTemp: #b inVector: #methodVector;							      "b comes from tempvetor, as we do write to it"
	     	 	blockReturnTop;
		jumpAheadTarget: #block;	
		send: #value;
		pushRemoteTemp: #b inVector: #methodVector;				
		returnTop;
		ir.
	
	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #() ) = 2).
	^iRMethod
	