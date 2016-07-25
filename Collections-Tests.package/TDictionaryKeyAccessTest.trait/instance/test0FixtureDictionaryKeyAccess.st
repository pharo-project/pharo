tests - fixture
test0FixtureDictionaryKeyAccess
	| collection equals |
	self nonEmptyWithoutEqualsValues.
	self deny: self nonEmptyWithoutEqualsValues isEmpty.
	equals := true.
	collection := self nonEmptyWithoutEqualsValues values.
	collection detect: [ :each | (collection occurrencesOf: each) > 1 ] ifNone: [ equals := false ].
	self assert: equals = false.
	self valueNotIn.
	self deny: (self nonEmptyWithoutEqualsValues values includes: self valueNotIn)