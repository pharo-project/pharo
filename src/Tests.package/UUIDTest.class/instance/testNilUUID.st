testing
testNilUUID
	| nilUUIDRepresentation |
	nilUUIDRepresentation := '00000000-0000-0000-0000-000000000000'.
	self assert: UUID nilUUID isNilUUID.
	self assert: UUID nilUUID asString equals: nilUUIDRepresentation.
	self assert: (UUID fromString: nilUUIDRepresentation) equals: UUID nilUUID.
	self deny: UUID new isNilUUID 