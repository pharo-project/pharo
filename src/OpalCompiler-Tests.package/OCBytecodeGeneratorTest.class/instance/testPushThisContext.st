tests - instructions
testPushThisContext
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushThisContext ;
		returnTop;
		compiledMethod.

	self testMethod: cm against:  '13 <89> pushThisContext
14 <7C> returnTop'