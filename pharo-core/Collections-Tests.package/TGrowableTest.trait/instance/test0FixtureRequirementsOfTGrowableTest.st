tests - fixture
test0FixtureRequirementsOfTGrowableTest
	self empty.
	self nonEmpty.
	self element.
	self elementNotIn.
	self assert: self empty isEmpty.
	self deny: self nonEmpty isEmpty.
	self assert: (self nonEmpty includes: self element).
	self deny: (self nonEmpty includes: self elementNotIn)