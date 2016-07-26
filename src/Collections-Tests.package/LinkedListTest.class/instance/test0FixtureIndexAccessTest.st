tests - fixture
test0FixtureIndexAccessTest
	| res |
	self collectionMoreThan1NoDuplicates.
	self assert: self collectionMoreThan1NoDuplicates size = 5.
	res := true.
	self collectionMoreThan1NoDuplicates
		detect: [ :each | (self collectionMoreThan1NoDuplicates occurrencesOf: each) > 1 ]
		ifNone: [ res := false ].
	self assert: res = false.
	self elementInForIndexAccessing.
	self assert: (self collectionMoreThan1NoDuplicates includes: self elementInForIndexAccessing).
	self elementNotInForIndexAccessing.
	self deny: (self collectionMoreThan1NoDuplicates includes: self elementNotInForIndexAccessing)