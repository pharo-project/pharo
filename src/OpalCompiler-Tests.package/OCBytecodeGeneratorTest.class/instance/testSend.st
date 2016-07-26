tests - instructions
testSend
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteral: 1;
		send: #a;
		returnTop;
		compiledMethod.

	self testMethod: cm against:  '17 <76> pushConstant: 1
18 <D0> send: a
19 <7C> returnTop'	