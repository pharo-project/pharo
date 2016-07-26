tests - fixture
test0FixtureCopySameContentsTest
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self empty.
	self assert: self empty isEmpty