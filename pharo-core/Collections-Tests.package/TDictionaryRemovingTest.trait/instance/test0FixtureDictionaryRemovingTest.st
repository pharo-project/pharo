tests - fixture
test0FixtureDictionaryRemovingTest
	self nonEmptyDict.
	self deny: self nonEmptyDict isEmpty.
	self keyNotInNonEmptyDict.
	self deny: (self nonEmptyDict keys includes: self keyNotInNonEmptyDict)