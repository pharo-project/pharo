tests - instructions
testReturnTop
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against:   '13 <76> pushConstant: 1
14 <7C> returnTop'