tests
testTempFilePrefixSuffix
	| fileRef |
	fileRef := FileReference newTempFilePrefix: 'FileReference' suffix: 'Test'.
	self assert: (fileRef isKindOf: FileReference).
	self assert: fileRef exists not.