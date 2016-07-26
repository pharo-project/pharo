tests
testCompileCProgram
	| extractor | 
	extractor := self testingSharedPool cHeaderExtractor.
	extractor binaryFilename asFileReference ensureDelete.
	extractor 
		generateCProgram;
		compileCProgram.
	self assert: extractor binaryFilename asFileReference exists.
	self assert: extractor binaryFilename asFileReference size > 100.