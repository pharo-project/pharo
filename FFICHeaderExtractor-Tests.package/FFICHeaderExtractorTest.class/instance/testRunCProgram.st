tests
testRunCProgram
	| extractor | 
	extractor := self testingSharedPool cHeaderExtractor.
	extractor outputFilename asFileReference ensureDelete.
	extractor 
		generateCProgram;
		compileCProgram;
		runCProgram.
	self assert: extractor outputFilename asFileReference exists.
	self assert: extractor outputFilename asFileReference size > 10.