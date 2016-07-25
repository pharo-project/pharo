tests
testCreationFromString36With0
	| uuid |
	uuid := UUID fromString36: '0'.
	self assert: (uuid isNilUUID).
	
