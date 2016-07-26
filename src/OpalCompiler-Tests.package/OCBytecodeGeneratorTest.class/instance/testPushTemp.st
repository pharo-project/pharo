tests - instructions
testPushTemp
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 1;
		pushTemp: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against:  '13 <10> pushTemp: 0
14 <7C> returnTop'