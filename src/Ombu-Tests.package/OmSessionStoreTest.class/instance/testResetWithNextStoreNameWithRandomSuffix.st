tests
testResetWithNextStoreNameWithRandomSuffix

	| aFileReference anotherFileReference |
	store storeNameStrategy: OmRandomSuffixStrategy new.

	store resetWithNextStoreName.
	aFileReference := store writingFileReference.
	store resetWithNextStoreName.
	anotherFileReference := store writingFileReference.
	
	self deny: aFileReference = anotherFileReference