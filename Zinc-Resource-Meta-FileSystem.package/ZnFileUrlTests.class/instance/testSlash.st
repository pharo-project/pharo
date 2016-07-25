testing
testSlash
	| fileUrl fileReference |
	fileUrl := 'file:///' asZnUrl.
	self assert: fileUrl isSlash.
	self assert: fileUrl isFile.
	fileReference := fileUrl asFileReference.
	self assert: fileReference isRoot