tests
testByteArrayWithinArray
	self class compile: 'array ^ #( #[1] #[2] )'.
	self assert: (self array isKindOf: Array).
	self assert: (self array size = 2).
	self assert: (self array first isKindOf: ByteArray).
	self assert: (self array first first = 1).
	self assert: (self array last isKindOf: ByteArray).
	self assert: (self array last first = 2)
	
