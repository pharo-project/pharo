testing
testEmpty
	| encoder |
	encoder := ZnBase64Encoder new.
	self 
		assert: (encoder encode: #[])
		equals: ''.
	self
		assert: (encoder decode: '')
		equals: #[]