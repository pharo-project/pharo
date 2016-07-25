tests - instructions
testPushNewArray
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushNewArray: 1;
		returnTop;
		compiledMethod.

	self testMethod: cm against:  '13 <8A 01> push: (Array new: 1)
15 <7C> returnTop'
	