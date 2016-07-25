tests
testCreationFromString36NotNil
	| uuid uuid36 |
	uuid := UUID fromString: 'fedcba98-7654-3210-0123-456789abcdef'.
	uuid36 := UUID fromString36: 'e738i8a5the1f87mrh14o6vi6'.
	self should: [uuid = uuid36]