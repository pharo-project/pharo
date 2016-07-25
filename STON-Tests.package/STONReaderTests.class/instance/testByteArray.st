tests
testByteArray
	self assert: (self materialize: 'ByteArray[''010203'']') = #(1 2 3) asByteArray
