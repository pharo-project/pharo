tests - fixture
test0FixtureOccurrencesTest
	self empty.
	self assert: self empty isEmpty.
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self elementInForOccurrences.
	self assert: (self nonEmpty includes: self elementInForOccurrences).
	self elementNotInForOccurrences.
	self deny: (self nonEmpty includes: self elementNotInForOccurrences)