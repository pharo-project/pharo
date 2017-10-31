tests
testResetWithNextStoreNameWithTimeStampSuffix

	| aFileReference anotherFileReference |
	store storeNameStrategy: OmTimeStampSuffixStrategy new.

	store resetWithNextStoreName.
	aFileReference := store writingFileReference.
	50 milliSeconds wait.
	store resetWithNextStoreName.
	anotherFileReference := store writingFileReference.
	
	self deny: aFileReference = anotherFileReference