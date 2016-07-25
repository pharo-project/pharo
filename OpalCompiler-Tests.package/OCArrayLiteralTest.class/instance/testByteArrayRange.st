tests
testByteArrayRange
	self class compile: 'array ^ #[ 0 255 ]'.
	self assert: (self array isKindOf: ByteArray).
	self assert: (self array size = 2).
	self assert: (self array first = 0).
	self assert: (self array last = 255)