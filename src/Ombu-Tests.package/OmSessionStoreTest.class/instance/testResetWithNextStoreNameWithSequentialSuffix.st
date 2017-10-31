tests
testResetWithNextStoreNameWithSequentialSuffix
	"The sequencial suffix doesn't change unless the file is created"
	
	| aFileReference anotherFileReference |
	store storeNameStrategy: OmSequentialSuffixStrategy new.
	
	store resetWithNextStoreName.
	aFileReference := store writingFileReference.
	store resetWithNextStoreName.
	anotherFileReference := store writingFileReference.
	
	self assert: aFileReference equals: anotherFileReference.
	
	store newEntry: (OmEntry content: 42).
	store flush.
	store resetWithNextStoreName.
	anotherFileReference := store writingFileReference.
	self deny: aFileReference = anotherFileReference.