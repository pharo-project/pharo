testing
testOne
	| uuid |
	uuid := generator next.
	self assert: (uuid isKindOf: UUID).
	self deny: uuid isNilUUID