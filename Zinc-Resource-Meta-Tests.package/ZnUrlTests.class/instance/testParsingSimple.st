testing
testParsingSimple
	| url |
	url := ZnUrl fromString: 'http://www.example.com:8080/foo/bar/baz.txt?x=1&y=2#m1'.
	self assert: url scheme = #http.
	self assert: url host = 'www.example.com'.
	self assert: url port = 8080.
	self assert: url hasPath.
	self assert: url isFilePath.
	self assert: url hasQuery.
	self assert: url hasFragment.
	self assert: url fragment = 'm1'