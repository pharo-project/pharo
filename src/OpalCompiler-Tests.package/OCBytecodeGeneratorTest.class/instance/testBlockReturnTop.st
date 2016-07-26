tests - instructions
testBlockReturnTop
	| cm |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		blockReturnTop;
		compiledMethod.

	self testMethod: cm against: '13 <7D> blockReturn'.
	