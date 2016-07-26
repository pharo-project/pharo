tests
testParseTrailingSlash
	| path |
	path := Path from: 'griffle/' delimiter: $/.
	self assert: path size equals: 1.
	self assert: (path at: 1) equals: 'griffle'