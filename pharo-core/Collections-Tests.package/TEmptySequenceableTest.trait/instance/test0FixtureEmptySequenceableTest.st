tests - fixture
test0FixtureEmptySequenceableTest
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self empty.
	self assert: self empty isEmpty