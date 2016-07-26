tests - instructions
testStoreTemp
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 1;
		pushLiteral: 1;
		storeTemp: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against:  '13 <76> pushConstant: 1
14 <81 40> storeIntoTemp: 0
16 <7C> returnTop'