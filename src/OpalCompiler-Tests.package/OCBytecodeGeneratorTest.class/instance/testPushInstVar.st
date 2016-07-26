tests - instructions
testPushInstVar
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushInstVar: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against:  '13 <00> pushRcvr: 0
14 <7C> returnTop'
	