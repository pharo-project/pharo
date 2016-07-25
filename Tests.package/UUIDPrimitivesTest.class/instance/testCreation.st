tests
testCreation
	| uuid |
	uuid := UUID new.
	self should: [uuid size = 16].
	self shouldnt: [uuid isNilUUID].
	self should: [uuid asString size = 36].
