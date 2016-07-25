testing-extensions
testStringRegexMatchesDo
	| result |
	result := OrderedCollection new.
	'aabbcc' regex: 'b+' matchesDo: [ :each | result add: each ].
	self assert: result size = 1.
	self assert: result first = 'bb'