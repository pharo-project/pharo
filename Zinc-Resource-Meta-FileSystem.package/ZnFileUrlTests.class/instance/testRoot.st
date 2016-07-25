testing
testRoot
	| fileUrl fileReference |
	fileReference := FileLocator root.
	self assert: fileReference isRoot.
	fileUrl := fileReference asZnUrl.
	self assert: fileUrl isFile.
	self assert: fileUrl isSlash