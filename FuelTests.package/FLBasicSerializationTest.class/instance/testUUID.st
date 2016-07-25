tests
testUUID

	self assertSerializationEqualityOf: (UUID fromString: 'a3b64357-377a-5b41-b575-1c653084a121').
	self assertSerializationEqualityOf: UUID nilUUID.
	self assertSerializationEqualityOf: UUID new.
	