tests - instructions
testPushDup
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushDup;
		returnTop;
		compiledMethod.

	self testMethod: cm against:  '13 <88> dup
14 <7C> returnTop'
	