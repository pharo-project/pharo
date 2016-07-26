tests - instructions
testPushLiteralVariable
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushLiteralVariable: (LookupKey key: '1');
		returnTop;
		compiledMethod.

	self testMethod: cm against:  '17 <40> pushLit: 1
18 <7C> returnTop'
	