testing
testRelative
	| fileReference fileUrl |
	fileReference := 'foo/bar/test.txt' asFileReference.
	self assert: fileReference isRelative.
	fileUrl := fileReference asZnUrl.
	self assert: fileUrl isFile.
	self assert: fileUrl pathSegments size >= fileReference pathSegments size.
	self assert: (fileUrl pathSegments asArray endsWith: fileReference pathSegments asArray)