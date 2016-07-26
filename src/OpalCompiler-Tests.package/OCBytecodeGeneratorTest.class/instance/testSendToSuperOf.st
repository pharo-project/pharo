tests - instructions
testSendToSuperOf
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		send: #a toSuperOf: Object;
		returnTop;
		compiledMethod.

	self testMethod: cm against:  '17 <76> pushConstant: 1
18 <85 00> superSend: a
20 <7C> returnTop'