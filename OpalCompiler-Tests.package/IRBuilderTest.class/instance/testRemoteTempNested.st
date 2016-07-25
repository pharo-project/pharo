testing - blocks
testRemoteTempNested

	| iRMethod aCompiledMethod |

		iRMethod := IRBuilder new
		addTemps: #(a);  "we have one real temp"
		createTempVectorNamed:#methodVector withVars: #(b);      "b we know will be written to, so make a tempvector entry" 
		pushLiteral: 1;
		storeTemp: #a;
		popTop;
		pushClosureCopyCopiedValues: #(methodVector a) args: #() jumpTo: #block;
		createTempVectorNamed:#blockVector withVars: #(f);    
			pushTemp: #a;                                                                                             "a is just read, so we copy it to the block"
				pushClosureCopyCopiedValues: #(methodVector) args: #() jumpTo: #block2;
				pushLiteral: 1;
				storeRemoteTemp: #b inVector: #methodVector;							      "f comes from tempvetor, as we do write to it"
				blockReturnTop;		
				jumpAheadTarget: #block2;	
			send: #value;
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
	self assert: ((aCompiledMethod valueWithReceiver: nil arguments: #()) = 2).
	^iRMethod
	