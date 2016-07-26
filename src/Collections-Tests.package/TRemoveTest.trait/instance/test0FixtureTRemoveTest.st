tests - fixture
test0FixtureTRemoveTest
	| duplicate |
	self empty.
	self nonEmptyWithoutEqualElements.
	self deny: self nonEmptyWithoutEqualElements isEmpty.
	duplicate := true.
	self nonEmptyWithoutEqualElements
		detect: [ :each | (self nonEmptyWithoutEqualElements occurrencesOf: each) > 1 ]
		ifNone: [ duplicate := false ].
	self assert: duplicate = false.
	self elementNotIn.
	self assert: self empty isEmpty.
	self deny: self nonEmptyWithoutEqualElements isEmpty.
	self deny: (self nonEmptyWithoutEqualElements includes: self elementNotIn)