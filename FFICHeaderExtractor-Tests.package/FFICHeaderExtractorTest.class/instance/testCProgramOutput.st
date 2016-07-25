tests
testCProgramOutput
	| extractor output | 
	extractor := self testingSharedPool cHeaderExtractor.
	extractor outputFilename asFileReference ensureDelete.
	extractor 
		generateCProgram;
		compileCProgram;
		runCProgram.
	output := extractor outputFilename asFileReference readStream contents.
	self assert: (output includesSubstring: '#(UNDEFINEDWHATEVER 0 0)').
	self assert: (output includesSubstring: '#(SIGKILL 9').
	self assert: (output includesSubstring: '#(EINVAL 22').
	self assert: output lines size equals: 5. 