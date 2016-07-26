tests-collections
testBitmap
	| bitmap materialized |
	self assertSerializationEqualityOf: (Bitmap with: 0 with: 16rFFFFFFFF).
	self assertSerializationEqualityOf: (Bitmap with: 4278190080).
	self assertSerializationEqualityOf: (Bitmap new: 4096).
	self assertSerializationEqualityOf: (Bitmap with: 0 with: 4294967295).
	self assertSerializationEqualityOf: (Bitmap new: 256).
	self assertSerializationEqualityOf: (Bitmap with: 0).
	bitmap := Bitmap new: 3.
	bitmap at: 1 put: 4324.
	bitmap at: 2 put: 5674.
	bitmap at: 3 put: 8978.
	materialized := self resultOfSerializeAndMaterialize: bitmap.
	self assert: (materialized at: 1) = 4324.
	self assert: (materialized at: 2) = 5674.
	self assert: (materialized at: 3) = 8978.	


