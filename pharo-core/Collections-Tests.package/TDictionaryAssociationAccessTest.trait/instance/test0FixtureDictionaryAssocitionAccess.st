tests - fixture
test0FixtureDictionaryAssocitionAccess
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self keyNotIn.
	self deny: (self nonEmpty keys includes: self keyNotIn)