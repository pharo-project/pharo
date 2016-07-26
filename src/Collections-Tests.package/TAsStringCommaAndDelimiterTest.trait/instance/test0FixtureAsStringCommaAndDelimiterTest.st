tests - fixture
test0FixtureAsStringCommaAndDelimiterTest
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self empty.
	self assert: self empty isEmpty.
	self nonEmpty1Element.
	self assert: self nonEmpty1Element size = 1