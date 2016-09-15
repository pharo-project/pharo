instance creation
pushFullClosureCompiledBlock: compiledBlock copiedValues: copiedValues
	^IRPushFullClosure new
			copiedValues: copiedValues;
			compiledBlock: compiledBlock;
			yourself.