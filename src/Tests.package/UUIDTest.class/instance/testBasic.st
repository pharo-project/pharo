testing
testBasic
	| uuid |
	uuid := UUID new.
	self assert: uuid size equals: 16.
	self deny: uuid isNilUUID.
	self deny: uuid = UUID new.