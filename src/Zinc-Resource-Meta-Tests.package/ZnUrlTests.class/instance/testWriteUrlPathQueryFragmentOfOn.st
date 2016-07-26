testing
testWriteUrlPathQueryFragmentOfOn
	| string |
	string := String streamContents: [ :stream |
		'http://host:7777/foo/bar/doc.txt?x=1#mark' asZnUrl printPathQueryFragmentOn:  stream ].
	self assert: string = '/foo/bar/doc.txt?x=1#mark'