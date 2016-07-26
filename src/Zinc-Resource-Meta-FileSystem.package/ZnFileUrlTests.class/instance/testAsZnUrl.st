testing
testAsZnUrl
	| fileReference fileUrl |
	fileReference := FileLocator root / 'foo' / 'bar' / 'test.txt'.
	fileUrl := fileReference asZnUrl.
	self assert: fileUrl isFile.
	self assert: fileUrl directory equals: 'foo/bar'.
	self assert: fileUrl file equals: 'test.txt'.
	self assert: fileUrl pathSegments asArray equals: #( 'foo' 'bar' 'test.txt').
	self assert: fileUrl pathPrintString equals: '/foo/bar/test.txt'.
	self assert: fileUrl printString equals: 'file:///foo/bar/test.txt'.
	self deny: fileUrl hasHost.
	self deny: fileUrl hasPort.
	self deny: fileUrl hasQuery.
	self deny: fileUrl hasFragment