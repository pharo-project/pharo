tests - fixture
test0FixtureEmptyTest
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self empty.
	self assert: self empty isEmpty