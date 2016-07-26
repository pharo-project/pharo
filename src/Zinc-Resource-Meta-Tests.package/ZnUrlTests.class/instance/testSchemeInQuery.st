testing
testSchemeInQuery
	| url |
	url := '/foo/bar/file.txt?url=http://www.pharo.org' asZnUrl.
	self deny: url hasScheme.
	self deny: url hasHost.
	self assert: url isFilePath.
	self assert: (url queryAt: #url) equals: 'http://www.pharo.org'