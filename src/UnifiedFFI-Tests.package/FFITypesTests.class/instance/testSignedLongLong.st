tests
testSignedLongLong
	self signed8ByteNumbers do: [ :int |
		|ref|
		ref := ByteArray new: FFIInt64 externalTypeSize.
		ref signedLongLongAt: 1 put: int.
		self assert: (ref signedLongLongAt: 1) = int ]