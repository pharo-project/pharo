testing
testSimple
	| encoder |
	encoder := ZnPercentEncoder new.
	self assert: (encoder encode: 'foo bar') equals: 'foo%20bar'.
	self assert: (encoder decode: 'foo%20bar') equals: 'foo bar'.
	self assert: (encoder encode: '') equals: ''.
	self assert: (encoder decode: '') equals: ''.
	self assert: (encoder decode: 'foo%25bar') equals: 'foo%bar'.
	self assert: (encoder decode: 'foo+bar') equals: 'foo bar'