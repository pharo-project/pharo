tests - instructions
testPushClosureCopyNumCopiedValuesNumArgsTo
	| cm  |
	
	cm := self newBytecodeGen
		numArgs: 0;
		numTemps: 0;
		pushClosureCopyNumCopiedValues: 0 numArgs: 0 to: #end;
		pushReceiver;
		blockReturnTop;
		label: #end;
		returnTop;
		compiledMethod.

	self testMethod: cm against:   '13 <8F 00 00 02> closureNumCopied: 0 numArgs: 0 bytes 17 to 18
17 <70> self
18 <7D> blockReturn
19 <7C> returnTop'