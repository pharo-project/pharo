tests
testBuildMethodSource
	| extractor methodSource | 
	extractor := self testingSharedPool cHeaderExtractor.
	extractor 
		generateCProgram;
		compileCProgram;
		runCProgram.
	methodSource := extractor buildMethodSource.
	self assert: (methodSource beginsWith: 'initVariables').
	self assert: (methodSource includesSubstring: '<platformName:').
	self assert: (methodSource includesSubstring: 'wordSize:').
	self assert: (methodSource includesSubstring: '"WHATEVER is UNDEFINED for this platform"').
	self assert: (methodSource includesSubstring: 'SIGKILL := 9.').
	self assert: (methodSource includesSubstring: 'EINVAL := 22.').
	self assert: methodSource lines size equals: 8. 