tests - fixture
test0FixtureDictionaryIncludesIdentity
	self nonEmptyWithCopyNonIdentical.
	self deny: self nonEmptyWithCopyNonIdentical isEmpty.
	self nonEmptyWithCopyNonIdentical do: [ :each | self deny: each == each copy ]