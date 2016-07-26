tests - instructions
testStorePopTemp
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		pushLiteral: 1;
		storePopTemp: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against:  '13 <76> pushConstant: 1
14 <76> pushConstant: 1
15 <68> popIntoTemp: 0
16 <7C> returnTop'