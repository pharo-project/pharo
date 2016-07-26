tests
testUnsigned2ByteIntegers
	self unsigned2ByteNumbers do: [ :int |
		|ref|
		ref := ByteArray new: 2.
		ref integerAt: 1 put: int size: 2 signed: false.
		self assert: (ref integerAt: 1 size: 2 signed: false) = int ]