tests
testByteArrayEmpty
	self class compile: 'array ^ #[]'.
	self assert: (self array isKindOf: ByteArray).
	self assert: (self array isEmpty)