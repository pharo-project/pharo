testing
testPathRemoval
	| url |
	url := 'http://api.foo.com:8080/foo/123/bar.txt?x=1&y=2' asZnUrl.
	self assert: url path equals: 'foo/123/bar.txt'.
	url removeFirstPathSegment.
	self assert: url path equals: '123/bar.txt'.
	url removeLastPathSegment.
	self assert: url path equals: '123'.
	url clearPath.
	self deny: url hasPath.
	self assert: (url queryAt: #x) equals: '1'.
	self assert: (url queryAt: #y) equals: '2'.
	self assert: url scheme equals: #http.
	self assert: url host equals: 'api.foo.com'.
	self assert: url port equals: 8080