tests - instructions
testStoreInstVar
	| gen cm symbolic |
	
	gen := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		storeInstVar: 1;
		returnTop.
	cm := gen compiledMethod.
	symbolic := String streamContents: [:str | cm longPrintOn: str ].

	self assert: gen stackFrameSize = 1.
	self assert: cm isCompiledMethod.
	self assert: symbolic =  '13 <76> pushConstant: 1
14 <81 00> storeIntoRcvr: 0
16 <7C> returnTop'