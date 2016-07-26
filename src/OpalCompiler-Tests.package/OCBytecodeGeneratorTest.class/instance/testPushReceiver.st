tests - instructions
testPushReceiver
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushReceiver;
		returnTop;
		compiledMethod.

	self testMethod: cm against:  '13 <70> self
14 <7C> returnTop'