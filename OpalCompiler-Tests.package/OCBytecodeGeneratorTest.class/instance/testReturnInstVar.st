tests - instructions
testReturnInstVar
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		returnInstVar: 1;
		compiledMethod.

	self testMethod: cm against:  'Quick return field 0 (0-based)'
	