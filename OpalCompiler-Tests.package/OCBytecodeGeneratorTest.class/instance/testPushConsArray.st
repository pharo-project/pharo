tests - instructions
testPushConsArray
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		pushConsArray: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against:   '13 <76> pushConstant: 1
14 <8A 81> pop 1 into (Array new: 1)
16 <7C> returnTop'
	