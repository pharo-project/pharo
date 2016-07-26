tests - instructions
testLabel
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		label: #back;
		pushLiteral: 1;
		returnTop;
		goto: #back;
		compiledMethod.

	self testMethod: cm against:   '13 <76> pushConstant: 1
14 <76> pushConstant: 1
15 <7C> returnTop
16 <A3 FC> jumpTo: 14'