tests - instructions
testReturnReceiver
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		returnReceiver;
		compiledMethod.

	self testMethod: cm against:  'Quick return self'
	