tests
testByteArrayLong
	self class compile: 'array ^ #[ ' , ((0 to: 255) inject: ' ' into: [ :r :e | r , ' ' , e asString ]) , ' ]'.
	self assert: (self array isKindOf: ByteArray).
	self assert: (self array size = 256).
	0 to: 255 do: [ :index | self assert: (self array at: index + 1) = index ]