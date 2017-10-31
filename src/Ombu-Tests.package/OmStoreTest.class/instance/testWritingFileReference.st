tests
testWritingFileReference

	anEntry := OmEntry content: 42.
	store newEntry: anEntry.
	store flush.
	
	self assert: store writingFileReference isFile.