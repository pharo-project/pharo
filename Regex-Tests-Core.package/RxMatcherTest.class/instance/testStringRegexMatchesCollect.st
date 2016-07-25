testing-extensions
testStringRegexMatchesCollect
	| result |
	result := 'aabbcc' regex: 'b+' matchesCollect: [ :each | each asUppercase ].
	self assert: result size = 1.
	self assert: result first = 'BB'