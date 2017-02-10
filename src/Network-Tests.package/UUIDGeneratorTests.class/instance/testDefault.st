testing
testDefault
	self assert: UUIDGenerator default equals: UUIDGenerator default.
	self deny: UUIDGenerator next isNilUUID 