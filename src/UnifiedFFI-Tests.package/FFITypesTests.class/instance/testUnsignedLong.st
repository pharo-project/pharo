tests
testUnsignedLong
	self unsigned4ByteNumbers do: [ :int |
		|ref|
		ref := ByteArray new: FFIUInt32 externalTypeSize.
		ref unsignedLongAt: 1 put: int.
		self assert: (ref unsignedLongAt: 1) = int ]