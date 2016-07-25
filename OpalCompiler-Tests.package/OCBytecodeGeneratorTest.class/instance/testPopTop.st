tests - instructions
testPopTop
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		pushLiteral: 1;
		popTop;
		returnTop;
		compiledMethod.

	self testMethod: cm against:  '13 <76> pushConstant: 1
14 <76> pushConstant: 1
15 <87> pop
16 <7C> returnTop'