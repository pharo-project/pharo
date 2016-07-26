tests
testSigned1ByteIntegers
	self signed1ByteNumbers do: [ :int |
		|ref|
		ref := ByteArray new: 1.
		ref integerAt: 1 put: int size: 1 signed: true.
		self assert: (ref integerAt: 1 size: 1 signed: true) = int ]