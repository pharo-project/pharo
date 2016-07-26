tests-collections
testByteArrays

	self assertSerializationEqualityOf: { #[1 2 3 4 5 6]. #[1 2 3 4 5 6]. #[7 8 9 10 11 12]. #[7 8 9 10 11 12] }.
	self assertSerializationEqualityOf:  (ByteArray with: 10 with: 20 with: 30).
	self assertSerializationEqualityOf: ByteArray new.
	self assertSerializationEqualityOf: (ByteArray new: 100).
	self assertSerializationEqualityOf: (0 to: 255) asByteArray.