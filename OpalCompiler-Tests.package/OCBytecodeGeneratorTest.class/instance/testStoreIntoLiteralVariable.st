tests - instructions
testStoreIntoLiteralVariable
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		storeIntoLiteralVariable: (LookupKey key: '1');
		returnTop;
		compiledMethod.

	self testMethod: cm against:   '17 <76> pushConstant: 1
18 <81 C0> storeIntoLit: 1
20 <7C> returnTop'