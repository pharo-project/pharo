testing
testSimple
	| encoder |
	encoder := ZnBase64Encoder new.
	self 
		assert: (encoder encode: 'Man' asByteArray)
		equals: 'TWFu'.
	self
		assert: (encoder decode: 'TWFu')
		equals: 'Man' asByteArray 