tests - fixture
test0FixtureDictionaryAddingTest
	self nonEmptyDict.
	self deny: self nonEmptyDict isEmpty.
	self associationWithKeyNotInToAdd.
	self deny: (self nonEmptyDict keys includes: self associationWithKeyNotInToAdd key).
	self associationWithKeyAlreadyInToAdd.
	self assert: (self nonEmptyDict keys includes: self associationWithKeyAlreadyInToAdd key)