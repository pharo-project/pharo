testing
testParsingWrongScheme
	| url |
	url := ZnUrl fromString: 'git://user@foo.com/bar'.
	self assert: url scheme equals: #git.
	self assert: url username equals: 'user'.
	self assert: url host equals: 'foo.com'.
	self assert: url path equals: 'bar'.
	self should: [ url enforceKnownScheme ] raise: ZnUnknownScheme 