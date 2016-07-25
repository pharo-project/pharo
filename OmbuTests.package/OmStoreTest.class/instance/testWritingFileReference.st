tests
testWritingFileReference

	anEntry := OmEntry content: 42.
	store newEntry: anEntry.
	
	self assert: store writingFileReference isFile.