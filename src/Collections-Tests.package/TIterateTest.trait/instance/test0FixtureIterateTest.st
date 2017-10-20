tests - fixture
test0FixtureIterateTest
	| res |
	self collectionWithoutNilElements.
	self assert: (self collectionWithoutNilElements occurrencesOf: nil) = 0.
	res := true.
	self collectionWithoutNilElements
		detect: [ :each | (self collectionWithoutNilElements occurrencesOf: each) > 1 ]
		ifNone: [ res := false ].
	self assert: res = false