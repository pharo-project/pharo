tests
testRoundtrip

	| aLog anotherLog |
	aLog := 
		EpLog newWithStore: 
			(OmFileStore 
				named: self outputLogNameForTest
				inDirectory: FileSystem workingDirectory).

	EpOmbuExporter new 
		outputLog: aLog;
		fileOut: inputLog1 entries.

	anotherLog := 
		EpLog newWithStore: 
			(OmFileStore 
				named: self outputLogNameForTest
				inDirectory: FileSystem workingDirectory).

	self assert: anotherLog entries size equals: inputLog1 entries size.