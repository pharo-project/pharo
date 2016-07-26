tests
testByteArrayBase
	self class compile: 'array ^ #[2r1010 8r333 16rFF]'.
	self assert: (self array isKindOf: ByteArray).
	self assert: (self array size = 3).
	self assert: (self array first = 10).
	self assert: (self array second = 219).
	self assert: (self array last = 255)
	
