tests - instructions
testStorePopInstVar
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		pushLiteral: 1;
		storePopInstVar: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against:   '13 <76> pushConstant: 1
14 <76> pushConstant: 1
15 <60> popIntoRcvr: 0
16 <7C> returnTop'