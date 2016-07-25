tests
testSignedLong
	self signed4ByteNumbers do: [ :int |
		|ref|
		ref := ByteArray new: FFIInt32 externalTypeSize.
		ref signedLongAt: 1 put: int.
		self assert: (ref signedLongAt: 1) = int ]