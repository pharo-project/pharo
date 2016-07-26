testing
testQueryWriting
	| fields string |
	(fields := Dictionary new)
		at: 'foo' put: '100';
		at: 'bar' put: 'a b'.
	string := String streamContents: [ :stream | 
		ZnResourceMetaUtils writeQueryFields: fields on: stream ].
	self assert: (string includesSubstring: 'foo=1').
	self assert: (string includesSubstring: 'bar=a%20b').