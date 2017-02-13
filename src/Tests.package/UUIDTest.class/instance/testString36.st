testing
testString36
	| uuid |
	uuid := UUID new.
	self assert: (uuid asString36 allSatisfy: [ :e | e isAlphaNumeric ]).
	self assert: (UUID fromString36: uuid asString36) equals: uuid.

	self assert: UUID nilUUID asString36 equals: 0 asString.
	self assert: (UUID fromString36: 0 asString) equals: UUID nilUUID 