running
tearDown
	super tearDown.
	
	logBuilder1 cleanUp.
	logBuilder2 cleanUp.

	(FileSystem workingDirectory / (self outputLogNameForTest, '.ombu')) ensureDelete.