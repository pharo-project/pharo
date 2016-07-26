tests
testUnsignedLongLong
	self unsigned8ByteNumbers do: [ :int |
		|ref|
		ref := ByteArray new: FFIUInt64 externalTypeSize.
		ref unsignedLongLongAt: 1 put: int.
		self assert: (ref unsignedLongLongAt: 1) = int ]