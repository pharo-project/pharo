tests - execution
testClosureExample
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
	
		
	cm methodClass: self class.
	cm selector: #test.

	self assert: (cm isKindOf: CompiledMethod).
	self assert: ((cm valueWithReceiver: 2@2 arguments: #()) isKindOf: BlockClosure).
	self assert: ((cm valueWithReceiver: 2@2 arguments: #()) value = (2@2)).
	^cm
	