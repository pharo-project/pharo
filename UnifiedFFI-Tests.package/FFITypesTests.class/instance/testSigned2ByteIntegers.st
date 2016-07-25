tests
testSigned2ByteIntegers
	self signed2ByteNumbers do: [ :int |
		|ref|
		ref := ByteArray new: 2.
		ref integerAt: 1 put: int size: 2 signed: true.
		self assert: (ref integerAt: 1 size: 2 signed: true) = int ]