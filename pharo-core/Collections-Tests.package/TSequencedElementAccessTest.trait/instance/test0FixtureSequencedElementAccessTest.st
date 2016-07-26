tests - fixture
test0FixtureSequencedElementAccessTest
	self moreThan4Elements.
	self assert: self moreThan4Elements size >= 4.
	self subCollectionNotIn.
	self subCollectionNotIn detect: [ :each | (self moreThan4Elements includes: each) not ] ifNone: [ self assert: false ].
	self elementNotInForElementAccessing.
	self deny: (self moreThan4Elements includes: self elementNotInForElementAccessing).
	self elementInForElementAccessing.
	self assert: (self moreThan4Elements includes: self elementInForElementAccessing)