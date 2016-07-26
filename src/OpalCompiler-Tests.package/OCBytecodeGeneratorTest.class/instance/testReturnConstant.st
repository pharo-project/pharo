tests - instructions
testReturnConstant
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		returnConstant: 1;
		compiledMethod.

	self testMethod: cm against:  'Quick return 1'
	