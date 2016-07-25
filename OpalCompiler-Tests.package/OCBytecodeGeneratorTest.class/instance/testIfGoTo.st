tests - instructions
testIfGoTo
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 1;
		pushTemp: 1;
		pushLiteral: 1;
		if: true goto: #return;
		returnTop;
		label: #return;
		returnTop;
		compiledMethod.

	self testMethod: cm against:   '13 <10> pushTemp: 0
14 <76> pushConstant: 1
15 <A8 01> jumpTrue: 18
17 <7C> returnTop
18 <7C> returnTop'