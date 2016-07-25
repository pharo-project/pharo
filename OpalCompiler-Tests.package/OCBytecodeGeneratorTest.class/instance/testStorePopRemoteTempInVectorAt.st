tests - instructions
testStorePopRemoteTempInVectorAt
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		pushLiteral: 1;
		storePopRemoteTemp: 1 inVectorAt: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against:  '13 <76> pushConstant: 1
14 <76> pushConstant: 1
15 <8E 00 00> popIntoTemp: 0 inVectorAt: 0
18 <7C> returnTop'