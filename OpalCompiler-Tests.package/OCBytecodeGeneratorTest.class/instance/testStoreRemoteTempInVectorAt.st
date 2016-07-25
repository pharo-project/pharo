tests - instructions
testStoreRemoteTempInVectorAt
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		storeRemoteTemp: 1 inVectorAt: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against:  '13 <76> pushConstant: 1
14 <8D 00 00> storeIntoTemp: 0 inVectorAt: 0
17 <7C> returnTop'