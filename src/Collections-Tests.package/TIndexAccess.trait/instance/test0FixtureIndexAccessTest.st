tests - fixture
test0FixtureIndexAccessTest
	| res collection element |
	self collectionMoreThan1NoDuplicates.
	self assert: self collectionMoreThan1NoDuplicates size > 1.
	res := true.
	self collectionMoreThan1NoDuplicates
		detect: [ :each | (self collectionMoreThan1NoDuplicates occurrencesOf: each) > 1 ]
		ifNone: [ res := false ].
	self assert: res = false.
	self elementInForIndexAccessing.
	self
		assert: ((collection := self collectionMoreThan1NoDuplicates) includes: (element := self elementInForIndexAccessing)).
	self elementNotInForIndexAccessing.
	self deny: (self collectionMoreThan1NoDuplicates includes: self elementNotInForIndexAccessing)