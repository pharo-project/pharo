testing
testParsingEscape
	| url |
	url := ZnUrl fromString: 'http://localhost/foo%62%61%72'.
	self assert: url firstPathSegment = 'foobar'