tests - execution
testExample2
	| cm |
	
	cm := self newBytecodeGen
		numArgs: 1;
		numTemps: 1;
		pushInstVar: 2;
		pushTemp: 1;
		send: #<;
		if: false goto: #else;
		pushLiteral: 'yes';
		returnTop ;
		label: #else;
		pushLiteral: 'no';
		returnTop;
		compiledMethod.

	self assert: (cm isKindOf: CompiledMethod).
	self assert: (cm valueWithReceiver: 2@2 arguments: #(1)) = 'no' .
	self assert: (cm valueWithReceiver: 2@2  arguments: #(3)) = 'yes' .	
	^cm
	