tests - fixture
test0FixtureBeginsEndsWithTest
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self assert: self nonEmpty size > 1.
	self empty.
	self assert: self empty isEmpty