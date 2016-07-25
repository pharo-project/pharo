tests - fixture
test0TStructuralEqualityTest
	self empty.
	self nonEmpty.
	self assert: self empty isEmpty.
	self deny: self nonEmpty isEmpty