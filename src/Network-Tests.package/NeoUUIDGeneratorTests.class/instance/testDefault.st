testing
testDefault
	self assert: NeoUUIDGenerator default equals: NeoUUIDGenerator default.
	self deny: NeoUUIDGenerator next isNilUUID 