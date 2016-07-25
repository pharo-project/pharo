testing
testPadding
	| encoder |
	encoder := ZnBase64Encoder new.
	self assert: (encoder encode: 'M' asByteArray) equals: 'TQ=='.
	self assert: (encoder decode: 'TQ==') equals: 'M' asByteArray.
	self assert: (encoder encode: 'Ma' asByteArray) equals: 'TWE='.
	self assert: (encoder decode: 'TWE=') equals: 'Ma' asByteArray