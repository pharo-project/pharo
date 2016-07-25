tests - instructions
testPushRemoteTempInVectorAt
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushRemoteTemp: 1 inVectorAt: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against:   '13 <8C 00 00> pushTemp: 0 inVectorAt: 0
16 <7C> returnTop'